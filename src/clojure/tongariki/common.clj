(ns tongariki.common
  (:require
    [clojure.string :as cs]))

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
