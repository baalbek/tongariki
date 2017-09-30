(ns tongariki.common
  (:import
    [java.time.temporal ChronoUnit])
  (:require
    [clojure.string :as cs]))

(defn diff-days [d1 d2]
  (.between ChronoUnit/DAYS d1 d2))

(defmacro map-java-fn [map-fn java-obj lst]
  `(map #(~map-fn ~java-obj %) ~lst))


(defmacro map-tuple-java-fn [map-fn java-obj lst]
  `(let [tupled# (fn [arg#]
                  (let [result# (~map-fn ~java-obj arg#)]
                    [arg# result#]))]
     (map tupled# ~lst)))

(defmacro in? [v items]
  `(some #(= ~v %) ~items))

;------------------------- defprops --------------------------------
(defn as-get-set [^String p get-or-set prefix]
  (str prefix get-or-set (cs/upper-case (first p)) (.substring p 1)))

(defn p2kw [prop]
  (keyword (cs/lower-case prop)))

(defn getter [prop get-fn default]
  (if (nil? default)
    `(def ~get-fn
       (fn [this#]
         (let [cache# (.state this#)]
           (~prop @cache#))))
    `(def ~get-fn
       (fn [this#]
         (let [cache# (.state this#)
               val# (~prop @cache#)]
           (if (nil? val#)
             (do
               (swap! cache# assoc ~prop ~default)
               ~default)
             val#))))))

(defn setter [prop set-fn]
  `(def ~set-fn
     (fn [this# value#]
       (let [cache# (.state this#)]
         (swap! cache# assoc ~prop value#)))))

(defn getsetter [prop get-fn set-fn default]
  (if (nil? default)
    `(do
      (def ~set-fn
        (fn [this# value#]
          (let [cache# (.state this#)]
            (swap! cache# assoc ~prop value#))))
      (def ~get-fn
          (fn [this#]
            (let [cache# (.state this#)]
              (~prop @cache#)))))
    `(do
      (def ~set-fn
        (fn [this# value#]
          (let [cache# (.state this#)]
            (swap! cache# assoc ~prop value#))))
      (def ~get-fn
          (fn [this#]
            (let [cache# (.state this#)
                  val# (~prop @cache#)]
              (if (nil? val#)
                (do
                  (swap! cache# assoc ~prop ~default)
                  ~default)
                val#)))))))


(defmacro defprop [variants prop &
                   { :keys [prefix default]
                     :or {prefix "-"
                          default nil}}]
  (let [s-prop (p2kw prop)
        get-fn (symbol (as-get-set prop "get" prefix))
        set-fn (symbol (as-get-set prop "set" prefix))]
    (cond
      (= variants :getset)
      (getsetter s-prop get-fn set-fn default)
      (= variants :get)
      (getter s-prop get-fn default)
      (= variants :set)
      (setter s-prop set-fn))))


;------------------------- numeric --------------------------------
(defn normalize [coll]
  (let [ma (float (reduce max 0 coll))
        mi (Math/abs (float (reduce min 0 coll)))
        m (max ma mi)]
    (map #(/ % m) coll)))

;------------------------- loops --------------------------------
(defmacro process-lists-with [f & lists]
  (let [num-lists (count lists)]
    (cond
      (= 2 num-lists)
        (let [[a b] lists]
          `(loop [a# ~a b# ~b]
            (if (not (nil? a#))
              (do
                (~f (first a#) (first b#))
                (recur (next a#) (next b#))))))
      (= 3 num-lists)
        (let [[a b c] lists]
          `(loop [a# ~a b# ~b c# ~c]
            (if (not (nil? a#))
              (do
                (~f (first a#) (first b#) (first c#))
                (recur (next a#) (next b#) (next c#))))))
      (= 4 num-lists)
        (let [[a b c d] lists]
          `(loop [a# ~a b# ~b c# ~c d# ~d]
            (if (not (nil? a#))
              (do
                (~f (first a#) (first b#) (first c#) (first d#))
                (recur (next a#) (next b#) (next c#) (next d#))))))
      (= 5 num-lists)
        (let [[a b c d e] lists]
          `(loop [a# ~a b# ~b c# ~c d# ~d e# ~e]
            (if (not (nil? a#))
              (do
                (~f (first a#) (first b#) (first c#) (first d#) (first e#))
                (recur (next a#) (next b#) (next c#) (next d#) (next e#))))))
      (= 6 num-lists)
        (let [[a b c d e g] lists]
          `(loop [a# ~a b# ~b c# ~c d# ~d e# ~e g# ~g]
            (if (not (nil? a#))
              (do
                (~f (first a#) (first b#) (first c#) (first d#) (first e#) (first g#))
                (recur (next a#) (next b#) (next c#) (next d#) (next e#) (next g#))))))
      (= 7 num-lists)
        (let [[a b c d e g h] lists]
          `(loop [a# ~a b# ~b c# ~c d# ~d e# ~e g# ~g h# ~h]
            (if (not (nil? a#))
              (do
                (~f (first a#) (first b#) (first c#) (first d#) (first e#) (first g#) (first h#))
                (recur (next a#) (next b#) (next c#) (next d#) (next e#) (next g#) (next h#))))))
      (= 8 num-lists)
        (let [[a b c d e g h j] lists]
          `(loop [a# ~a b# ~b c# ~c d# ~d e# ~e g# ~g h# ~h j# ~j]
            (if (not (nil? a#))
              (do
                (~f (first a#) (first b#) (first c#) (first d#) (first e#) (first g#) (first h#) (first j#))
                (recur (next a#) (next b#) (next c#) (next d#) (next e#) (next g#) (next h#) (next j#)))))))))
