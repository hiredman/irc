(ns com.thelastcitadel.irc
  (:require [compojure.core :refer [GET POST PUT DELETE defroutes]]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.edn :as r]
            [clojure.java.io :as io]
            [clojure.core.typed :as t]
            [com.thelastcitadel.irc.fifo :as fifo])
  (:import (java.util UUID)
           (java.util.concurrent ConcurrentHashMap)
           (org.jibble.pircbot PircBot
                               User)
           (clojure.lang Named
                         IPersistentVector
                         IPersistentMap
                         IMapEntry)
           (com.thelastcitadel.irc.fifo Fifo)))

;; Type Aliases
(t/defalias Event
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
(t/defalias CompleteEvent
  (HMap :mandatory {:time Long
                    :nick String
                    :type clojure.lang.Keyword
                    :server String
                    ::bid String
                    ::eid String}
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
(t/defalias EventCallback (t/IFn [Event -> t/Any]))
(t/defalias RingResponse (HMap :mandatory {:status Number :body String}))
(t/defalias BotEntry (HMap :mandatory {:nick String :port Number :server String ::bid String}))
;; ;; Var Types
;; ;;; no-check
;; ;;;; clojure.core
(t/ann ^:no-check clojure.edn/read [-> t/Any])
(t/ann ^:no-check clojure.java.io/reader [t/Any -> java.io.Reader])
;; ;;;;
(t/ann ^:no-check wall-hack-method [Class Named (t/Seqable Class) t/Any t/Any * -> Object])
(t/ann ^:no-check pircbot* [String String EventCallback (t/IFn [PircBot -> t/Any]) -> PircBot])
(t/ann ^:no-check complete-event [Event String String -> CompleteEvent])
(t/ann ^:no-check bot-seq [-> (clojure.lang.ISeq (IMapEntry String PircBot))])
(t/ann ^:no-check handler [(HMap :mandatory {}) -> RingResponse])
(t/ann ^:no-check irc [(HMap :mandatory {}) -> RingResponse])
;; ;;;
(t/ann pircbot (t/IFn [String String EventCallback -> PircBot]
                      [(IPersistentVector t/Any) String EventCallback -> PircBot]))
(t/ann events (t/Atom1 Fifo))
(t/ann bots ConcurrentHashMap)
(t/ann get-bot [String -> PircBot])
(t/ann create-bot [String String Number String String -> RingResponse])
(t/ann destroy-bot [String -> RingResponse])
(t/ann get-channels [String -> RingResponse])
(t/ann join-channel [String String -> RingResponse])
(t/ann get-events [String -> RingResponse])
(t/ann get-users [String String -> RingResponse])
(t/ann part-channel [String String (t/U nil String) -> RingResponse])
(t/ann get-event [String String -> RingResponse])
(t/ann delete-event [String String -> RingResponse])
(t/ann send-out [String String -> RingResponse])
(t/ann list-bots [-> RingResponse])

(defn wall-hack-method [^Class class-name name- params obj & args]
  (assert (or (string? name-)
              (instance? clojure.lang.Named name-)))
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
                    (t/fn [conn :- PircBot]
                      (let [[server port pass] server]
                        (assert (string? server))
                        (assert (instance? Integer port))
                        (assert (string? pass))
                        (.connect ^PircBot conn server port pass)))
                    (t/fn [conn :- PircBot]
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

(def events (atom (fifo/fifo 1000)))

(defn complete-event [event bid eid]
  (assoc event
    ::bid bid
    ::eid eid))

(defn create-bot [bid server port nick password]
  (if (.contains ^ConcurrentHashMap bots bid)
    {:status 200
     :body bid}
    (let [b (pircbot (if password
                       [server port password]
                       server)
                     nick
                     (t/fn [event :- Event]
                       (let [eid (str (UUID/randomUUID))
                             completed-event (complete-event event bid eid)]
                         (swap! events (t/fn [m :- Fifo] (fifo/add m bid eid completed-event))))))]
      (assert bid)
      (.put ^ConcurrentHashMap bots bid b)
      {:status 201
       :body bid})))

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
   :body (pr-str (get (fifo/entries @events) bid))})

(defn get-event [bid eid]
  {:status 200
   :body (pr-str (get (get (fifo/entries @events) bid) eid))})

(defn delete-event [bid eid]
  (swap! events (t/fn [m :- Fifo] (fifo/without m bid eid)))
  {:status 200
   :body (pr-str #{})})

(defmacro send-fn [name]
  `(t/fn [bot# :- PircBot
          a# :- String
          b# :- String]
     (. ^PircBot bot# ~name ^String a# ^String b#)))

(def send-message (send-fn sendMessage))

(defmacro send- [method-name & args]
  `(do
     ~@(for [a args]
         `(assert (contains? ~'m ~(keyword (name a)))))
     (let [~@(for [n args i [(symbol (name n)) `(get ~'m ~(keyword (name n)) "")]] i)]
       ~@(for [a args
               i [`(assert (not (nil? ~(symbol (name a)))))
                  `(assert (instance? String ~(symbol (name a))))]]
           i)
       (. ~'b ~method-name ~@(for [a args] (symbol (name a)))))))

(defn send-out [bid body]
  (let [m (binding [*in* (-> body io/reader java.io.PushbackReader.)]
            (r/read))
        b (get-bot bid)]
    (assert (map? m))
    (condp = (:type m)
      :action (send- sendMessage target action)
      :invite (send- sendInvite nick channel)
      :message (send- sendMessage target message)
      :notice (send- sendNotice target notice))
    {:status 200
     :body (pr-str #{})}))

(defn bot-seq []
  (seq bots))

(defn list-bots []
  {:status 200
   :body (pr-str (set
                  (map
                   (t/fn [v :- (IMapEntry String PircBot)]
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
                        ::bid id}))
                   (bot-seq))))})

(defroutes irc
  (GET "/" [request] (list-bots))
  (POST "/" {{:keys [server port nick password id]} :params}
        (if id
          (create-bot id server port nick password)
          (create-bot (str (UUID/randomUUID)) server port nick password)))
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
