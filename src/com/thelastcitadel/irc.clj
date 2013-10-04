(ns com.thelastcitadel.irc
  (:require [compojure.core :refer [GET POST PUT DELETE defroutes]]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.edn :as r]
            [clojure.java.io :as io]
            [ clojure.core.typed :as t])
  (:import (java.util UUID)
           (java.util.concurrent ConcurrentHashMap)
           (org.jibble.pircbot PircBot
                               User)
           (clojure.lang Named
                         IPersistentVector
                         IPersistentMap
                         IMapEntry)))

;; Type Aliases
(t/def-alias Event
  (HMap :mandatory {:time Long
                    :nick String
                    :type clojure.lang.Keyword
                    :server String}
        :optional {:sender String
                   :login String
                   :hostname String
                   :target String
                   :action String
                   :channel String
                   :user-count Number
                   :topic String
                   :target-nick String
                   :source-nick String
                   :source-login String
                   :source-hostname String
                   :message String
                   :notice String}))
(t/def-alias CompleteEvent (U Event (HMap :mandatory {::bid String ::eid String})))
(t/def-alias EventCallback (Fn [Event -> Any]))
(t/def-alias EventMap (IPersistentMap String (IPersistentMap String CompleteEvent)))
(t/def-alias RingResponse (HMap :mandatory {:status Number :body String}))
(t/def-alias BotEntry (HMap :mandatory {:nick String :port Number :server String ::bid String}))
;; Var Types
;;; no-check
;;;; clojure.core
(t/ann ^:no-check clojure.core/coll? [Any -> Boolean])
(t/ann ^:no-check clojure.core/assoc-in (All [x] [x (t/Seqable Any) Any -> x]))
(t/ann ^:no-check clojure.core/update-in (All [x y z a] [x (t/Seqable Any) (Fn [y * -> z]) a * -> x]))
;; (t/ann ^:no-check clojure.core/update-in (All [x y z a] [x (t/Seqable Any) (Fn [x y * -> x]) a * -> x]))
(t/ann ^:no-check clojure.core/*in* java.io.PushbackReader)
(t/ann ^:no-check clojure.edn/read [-> Any])
(t/ann ^:no-check clojure.java.io/reader [Any -> java.io.Reader])
(t/ann ^:no-check clojure.core/val (All [x y] [(IMapEntry x y) -> y]))
;;;;
(t/ann ^:no-check wall-hack-method [Class Named (t/Seqable Class) Any Any * -> Object])
(t/ann ^:no-check pircbot* [String String EventCallback (Fn [PircBot -> Any]) -> PircBot])
(t/ann ^:no-check x [ -> Nothing])
(t/ann ^:no-check complete-event [Event String String -> CompleteEvent])
(t/ann ^:no-check bot-seq [-> (clojure.lang.ISeq (IMapEntry String PircBot))])
(t/ann ^:no-check handler [(HMap :mandatory {}) -> RingResponse])
(t/ann ^:no-check irc [(HMap :mandatory {}) -> RingResponse])
;;;
(t/ann pircbot (Fn [String String EventCallback -> PircBot] [(IPersistentVector Any) String EventCallback -> PircBot]))
(t/ann bots ConcurrentHashMap)
(t/ann get-bot [String -> PircBot])
(t/ann events (t/Atom1 EventMap))
(t/ann create-bot [String Number String String -> RingResponse])
(t/ann destroy-bot [String -> RingResponse])
(t/ann get-channels [String -> RingResponse])
(t/ann join-channel [String String -> RingResponse])
(t/ann get-events [String -> RingResponse])
(t/ann get-users [String String -> RingResponse])
(t/ann part-channel [String String (U nil String) -> RingResponse])
(t/ann get-event [String String -> RingResponse])
(t/ann delete-event [String String -> RingResponse])
(t/ann send-out [String String -> RingResponse])
(t/ann list-bots [-> RingResponse])

(defn wall-hack-method [^Class class-name name- params obj & args]
  (let [method-name (name name-)
        param-classes (into-array Class params)
        method (.getDeclaredMethod class-name method-name param-classes)]
    (assert method)
    (.setAccessible method true)
    (.invoke method obj (into-array Object args))))

(defmacro m [type & items]
  `(~'fun
    ~(assoc (into {} (for [item items] [(keyword (name item)) item]))
       :time `(System/currentTimeMillis)
       :nick 'nick
       :type type
       :server 'server)))

(defn pircbot* [server nick fun connector]
  (proxy [PircBot] []
    (onAction [sender login hostname target action]
      (m :action sender login hostname target action))
    (onChannelInfo [channel user-count topic]
      (m :channel-info channel user-count topic))
    (onConnect []
      (m :connect))
    (onDisconnect []
      (connector this))
    (onInvite [target-nick source-nick source-login source-hostname channel]
      (m :invite target-nick source-nick source-login source-hostname channel))
    (onJoin [channel sender login hostname]
      (m :join channel sender login hostname))
    (onMessage [channel sender login hostname message]
      (m :message channel sender login hostname message))
    (onNotice [source-nick source-login source-hostname target notice]
      (m :notice source-nick source-login source-hostname target notice))
    (onPrivateMessage [sender login hostname message]
      (m :private-message sender login hostname message))))

(defn pircbot [server nick fun]
  (let [connector (if (coll? server)
                    (t/fn> [conn :- PircBot]
                           (let [[server port pass] server]
                             (assert (string? server))
                             (assert (instance? Integer port))
                             (assert (string? pass))
                             (.connect ^PircBot conn server port pass)))
                    (t/fn> [conn :- PircBot]
                           (assert (string? server))
                           (.connect ^PircBot conn server)))
        server (if (coll? server) (first server) server)
        _ (assert (string? server))
        conn (pircbot* server nick fun connector)]
    (assert (instance? PircBot conn))
    (wall-hack-method
     org.jibble.pircbot.PircBot :setName [String] conn nick)
    (connector conn)
    conn))

(def bots (ConcurrentHashMap.))

(defn ^PircBot get-bot [bid]
  (let [bot (.get ^ConcurrentHashMap bots bid)]
    (assert bot)
    (assert (instance? PircBot bot))
    bot))

(def events (atom {}))

(defn x []
  (add-watch events ::expire
             (fn [k r ov nv]
               (when (not= ov nv)
                 (when-let [exp (seq (for [[bid evs] @events
                                           [eid {:keys [time]}] evs
                                           :when (and time
                                                      (> (- (System/currentTimeMillis) time)
                                                         (* 5 60 1000)))]
                                       [bid eid]))]
                   (swap! r (fn [m]
                              (reduce
                               (fn [m [bid eid]]
                                 (update-in m [bid] dissoc eid))
                               m exp))))))))
(x)
;; (add-watch events ::print
;;            (fn [k r ov nv]
;;              (prn nv)))

(defn complete-event [event bid eid]
  (assoc event
    ::bid bid
    ::eid eid))

(defn create-bot [server port nick password]
  (let [bid (str (UUID/randomUUID))
        b (pircbot server nick
                   (t/fn> [event :- Event]
                          (let [eid (str (UUID/randomUUID))
                                completed-event (complete-event event bid eid)]
                            (swap! events (t/fn> [m :- EventMap]
                                                 (assoc-in m [bid eid] completed-event))))))]
    (.put ^ConcurrentHashMap bots bid b)
    {:status 201
     :body bid}))

(defn destroy-bot [bid]
  (let [bot (get-bot bid)]
    (try
      (.disconnect bot)
      (.dispose bot)
      (catch Exception _)))
  (.remove ^ConcurrentHashMap bots bid)
  {:status 200
   :body bid})

(defn get-channels [bid]
  (let [bot (get-bot bid)]
    {:status 200
     :body (pr-str (set (.getChannels bot)))}))

(defn join-channel [bid channel]
  (let [bot (get-bot bid)]
    (.joinChannel bot channel))
  {:status 200
   :body (pr-str #{})})

(defn get-users [bid channel]
  {:status 200
   :body (pr-str (set
                  (map (fn [^User u]
                         (assert (instance? User u))
                         (.getNick u))
                       (.getUsers (get-bot bid) channel))))})

(defn part-channel [bid channel reason]
  (if reason
    (.partChannel (get-bot bid) channel reason)
    (.partChannel (get-bot bid) channel))
  {:status 200
   :body (pr-str #{})})

(defn get-events [bid]
  {:status 200
   :body (pr-str (get @events bid))})

(defn get-event [bid eid]
  {:status 200
   :body (pr-str (get (get @events bid) eid))})

(defn delete-event [bid eid]
  (swap! events (t/fn> [m :- EventMap] (update-in m [bid] dissoc eid)))
  {:status 200
   :body (pr-str #{})})

(defn send-out [bid body]
  (let [m (binding [*in* (-> body io/reader java.io.PushbackReader.)]
            (r/read))
        b (get-bot bid)]
    (case (:type m)
      :action (let [target (:target m)
                    action (:action m)]
                (assert (string? target))
                (assert (string? action))
                (.sendMessage b target action))
      :invite (let [nick (:nick m)
                    channel (:channel m)]
                (assert (string? nick))
                (assert (string? channel))
                (.sendInvite b nick channel))
      :message (let [target (:target m)
                     message (:message m)]
                 (assert (string? target))
                 (assert (string? message))
                 (.sendMessage b target message))
      :notice (let [target (:target m)
                    notice (:notice m)]
                (assert (string? target))
                (assert (string? notice))
                (.sendNotice b target notice)))
    {:status 200
     :body (pr-str #{})}))

(defn bot-seq []
  (seq bots))

(defn list-bots []
  {:status 200
   :body (pr-str (set
                  (t/for> :- BotEntry
                          [v :- (IMapEntry String PircBot) (bot-seq)]
                          (let [id (key v)
                                ^PircBot bot (val v)]
                            (assert (string? id))
                            (assert (instance? PircBot bot))
                            {:nick (let [n (.getNick bot)]
                                     (assert n)
                                     n)
                             :port (.getPort bot)
                             :server (let [s (.getServer bot)]
                                       (assert s)
                                       s)
                             ::bid id}))))})

(defroutes irc
  (GET "/" [request] (list-bots))
  (POST "/" {{:keys [server port nick password]} :params} (create-bot server port nick password))
  (DELETE "/:bid" {{:keys [bid]} :params} (destroy-bot bid))
  (GET "/:bid/channels" {{:keys [bid]} :params} (get-channels bid))
  (POST "/:bid/channel/:channel" {{:keys [bid channel]} :params} (join-channel bid channel))
  (GET "/:bid/channel/:channel" {{:keys [bid channel]} :params} (get-users bid channel))
  (DELETE "/:bid/channel/:channel" {{:keys [bid channel reason]} :params} (part-channel bid channel reason))
  (GET "/:bid/events" {{:keys [bid]} :params} (get-events bid))
  (GET "/:bid/event/:eid" {{:keys [bid eid]} :params} (get-event bid eid))
  (DELETE "/:bid/event/:eid" {{:keys [bid eid]} :params} (delete-event bid eid))
  (PUT "/:bid/send" {:keys [body] {:keys [bid]} :params} (send-out bid body)))

(def handler (-> #'irc handler/api))
