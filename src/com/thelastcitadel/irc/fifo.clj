(ns com.thelastcitadel.irc.fifo
  (:require [clojure.core.typed :as t])
  (:import (clojure.lang IPersistentVector
                         PersistentQueue
                         IPersistentMap
                         )))

;;  (t/U nil PersistentQueue) (t/HMap :complete? true)
(t/ann-record Fifo [q :- PersistentQueue
                    m :- (IPersistentMap t/Any (t/U nil (IPersistentMap t/Any t/Any)))
                    c :- Number])

(defrecord Fifo [q m c])

(t/ann fifo (t/IFn [Number -> Fifo]))
(t/ann ^:no-check add (t/IFn [Fifo t/Any t/Any t/Any -> Fifo]))
(t/ann entries (t/IFn [Fifo -> (IPersistentMap t/Any (t/U nil (IPersistentMap t/Any t/Any)))]))
(t/ann without (t/IFn [Fifo t/Any t/Any -> Fifo]))

(defn fifo [entry-count]
  (assert (number? entry-count))
  (let [q PersistentQueue/EMPTY]
    (assert (not (nil? q)))
    (assert (instance? PersistentQueue q))
    (->Fifo q {} entry-count)))

(defn add [f key1 key2 value]
  (let [q (:q f)
        m (:m f)]
    (assert (not (nil? q)))
    (assert (instance? PersistentQueue q))
    (if (>= (count m) (:c f))
      (let [[k1 k2] (peek q)
            q (pop q)]
        (->Fifo (conj q [key1 key2])
                (-> m
                    (assoc m k1 (dissoc (get m k1) k2))
                    (assoc-in m [key1 key2] value))
                (:c f)))
      (->Fifo (conj q [key1 key2])
              (assoc-in m [key1 key2] value)
              (:c f)))))

(defn without [f key1 key2]
  ;; (assert (map? (:m f)))
  ;; (assert (not (nil? (:q f))))
  ;; (assert (instance? PersistentQueue (:q f)))
  ;; (assert (not (nil? (get f key1))))
  (->Fifo (:q f)
          (assoc (:m f)
            key1 (dissoc (get (:m f) key1) key2))
          (:c f)))

(defn entries [f]
  (:m f))
