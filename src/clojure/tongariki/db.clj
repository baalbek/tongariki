(ns tongariki.db 
  (:import
    [org.apache.ibatis.io Resources]
    [org.apache.ibatis.session
      SqlSession
      SqlSessionFactory
      SqlSessionFactoryBuilder]
    [java.io  Reader]))

(def get-factory
  (memoize
    (fn [conf-xml]
      (println "Initializing " conf-xml)
      (with-open [reader ^Reader (Resources/getResourceAsReader conf-xml)]
        (let [builder ^SqlSessionFactoryBuilder (SqlSessionFactoryBuilder.)
              factory ^SqlSessionFactory (.build builder reader)]
          factory)))))

(defmacro with-session [conf-xml mapper & body]
  `(let [session# ^SqlSession (.openSession (get-factory ~conf-xml))
         ~'it (.getMapper session# ~mapper)
         result# ~@body]
     (doto session# .commit .close)
     result#))
