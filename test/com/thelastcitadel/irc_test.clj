(ns com.thelastcitadel.irc-test
  (:require [clojure.test :refer :all]
            [com.thelastcitadel.irc :refer :all]
            [ring.adapter.jetty :as a]
            [clj-http.client :as http])
  (:import (java.net URLEncoder)))

(use-fixtures :once
  (fn [f]
    (let [s (a/run-jetty handler
                         {:join? false
                          :port 10000})]
      (try
        (f)
        (finally
          (.stop s))))))

(deftest a-test
  (let [{bid :body} (http/post "http://localhost:10000"
                               {:query-params {:server "chat.freenode.net"
                                               :nick "clojurebotIII"}})]
    (Thread/sleep 10000)
    (let [{:keys [body]} (http/get (str "http://localhost:10000/" bid "/channels"))
          channels (read-string body)]
      (is (empty? channels)))
    (http/post (str "http://localhost:10000/" bid "/channel/" (URLEncoder/encode "#clojurebot")))
    (Thread/sleep 10000)
    (let [{:keys [body]}
          (http/get (str "http://localhost:10000/" bid "/channel/" (URLEncoder/encode "#clojurebot")))
          users (read-string body)]
      (is (contains? users "clojurebot")))
    (let [{:keys [body]} (http/get (str "http://localhost:10000/" bid "/channels"))
          channels (read-string body)]
      (is (= #{"#clojurebot"} channels)))
    (let [{:keys [body]} (http/get (str "http://localhost:10000/" bid "/events"))
          events (read-string body)
          [[id join]] (for [[k v] events
                            :when (= :join (:type v))
                            :when (= "#clojurebot" (:channel v))
                            :when (= "clojurebotIII" (:nick v))]
                        [k v])]
      (is join)
      (http/delete (str "http://localhost:10000/" bid "/event/" id))
      (let [{:keys [body]} (http/get (str "http://localhost:10000/" bid "/events"))
            events (read-string body)]
        (is (not (contains? events id)))))
    (http/delete (str "http://localhost:10000/" bid "/channel/" (URLEncoder/encode "#clojurebot")))
    (Thread/sleep 10000)
    (let [{:keys [body]} (http/get (str "http://localhost:10000/" bid "/channels"))
          channels (read-string body)]
      (is (empty? channels)))
    (Thread/sleep 10000)
    (http/delete (str "http://localhost:10000/" bid))))
