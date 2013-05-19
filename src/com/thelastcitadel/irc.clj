(ns com.thelastcitadel.irc
  (:require [compojure.core :refer [GET POST DELETE defroutes]]
            [compojure.route :as route]
            [compojure.handler :as handler])
  (:import (java.util UUID)
           (java.util.concurrent ConcurrentHashMap)
           (org.jibble.pircbot PircBot)))

(defn wall-hack-method [class-name name- params obj & args]
  (-> class-name (.getDeclaredMethod (name name-) (into-array Class params))
      (doto (.setAccessible true))
      (.invoke obj (into-array Object args))))

(defn pircbot [server nick fun]
  (let [connector (if (coll? server)
                    (fn [conn]
                      (let [[server port pass] server]
                        (.connect conn server port pass)))
                    #(.connect % server))
        server (if (coll? server) (first server) server)
        conn (proxy [PircBot] []
               (onDisconnect []
                 (connector this))
               (onJoin [channel sender login hostname]
                 (fun {:channel channel
                       :sender sender
                       :login login
                       :hostname hostname
                       :server server
                       :nick nick
                       :type :join
                       :time (System/currentTimeMillis)}))
               (onMessage [channel sender login hostname message]
                 (fun {:channel channel
                       :sender sender
                       :login login
                       :hostname hostname
                       :message message
                       :server server
                       :nick nick
                       :type :message
                       :time (System/currentTimeMillis)})))]
    (wall-hack-method
     org.jibble.pircbot.PircBot :setName [String] conn nick)
    (connector conn)
    conn))

(def bots (ConcurrentHashMap.))

(def events (atom {}))

(add-watch events ::expire
           (fn [k r ov nv]
             (when (not= ov nv)
               (when-let [exp (seq (for [[bid evs] @events
                                         [eid {:keys [time]}] evs
                                         :when (> (- (System/currentTimeMillis) time)
                                                  (* 5 60 1000))]
                                     [bid eid]))]
                 (swap! r (fn [m]
                            (reduce
                             (fn [m [bid eid]]
                               (update-in m [bid] dissoc eid))
                             m exp)))))))

(defn create-bot [server port nick password]
  (let [bid (str (UUID/randomUUID))
        b (pircbot server nick
                   (fn [event]
                     (let [eid (str (UUID/randomUUID))]
                       (swap! events assoc-in [bid eid]
                              (assoc event
                                ::bid bid
                                ::eid eid)))))]
    (.put bots bid b)
    bid))

(defroutes irc
  (POST "/" {{:keys [server port nick password]} :params}
        (let [id (create-bot server port nick password)]
          {:status 201
           :body id}))
  (DELETE "/:bid" {{:keys [bid]} :params}
          (do
            (try
              (.disconnect (.get bots bid))
              (.dispose (.get bots bid))
              (catch Exception _))
            (.remove bots bid)
            {:status 200
             :body bid}))
  (GET "/:bid/channels" {{:keys [bid]} :params}
       {:status 200
        :body (pr-str (set (.getChannels (.get bots bid))))})
  (POST "/:bid/channel/:channel" {{:keys [bid channel]} :params}
        (do
          (.joinChannel (.get bots bid) channel)
          {:status 200
           :body (pr-str #{})}))
  (DELETE "/:bid/channel/:channel" {{:keys [bid channel reason]} :params}
          (do
            (if reason
              (.partChannel (.get bots bid) channel reason)
              (.partChannel (.get bots bid) channel))
            {:status 200
             :body (pr-str #{})}))
  (GET "/:bid/events" {{:keys [bid]} :params}
       {:status 200
        :body (pr-str (get @events bid))})
  (GET "/:bid/event/:ied" {{:keys [bid eid]} :params}
       {:status 200
        :body (pr-str (get (get @events bid) eid))})
  (DELETE "/:bid/event/:eid" {{:keys [bid eid]} :params}
          (do
            (swap! events update-in [bid] dissoc eid)
            {:status 200
             :body (pr-str #{})}))
  )

(def handler (-> #'irc handler/api))
