(ns find-error
  (:gen-class))

(set! *warn-on-reflection* false)

(defn -main [& args]
  (let [filename (or (first args) "src/vitambo/dispatch.clj")]
    (with-open [r (java.io.PushbackReader. (clojure.java.io/reader filename))]
      (loop [form-num 1]
        (let [form (try
                    (read r false :eof)
                    (catch Exception e
                      (println (str "Error at form " form-num ": " (.getMessage e)))
                      (throw e)))]
          (if (= form :eof)
            (println (str "All " (dec form-num) " forms read OK"))
            (do
              (println (str "Form " form-num " OK"))
              (recur (inc form-num)))))))))
