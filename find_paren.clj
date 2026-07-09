(ns find-paren
  (:gen-class))

(defn -main [& args]
  (let [filename (or (first args) "src/vitambo/dispatch.clj")
        content (slurp filename)
        lines (clojure.string/split-lines content)]
    (loop [i 0 depth 0 min-depth 0]
      (if (>= i (count lines))
        (println (str "Final depth: " depth))
        (let [line (nth lines i)
              line-num (inc i)]
          (loop [j 0 d depth]
            (if (>= j (count line))
              (do
                (when (not= depth d)
                  (println (str "L" line-num " depth " d ": " (subs line 0 (min 70 (count line))))))
                (recur (inc i) d (min min-depth d)))
              (let [c (nth line j)]
                (cond
                  (#{\( \[ \{} c) (recur (inc j) (inc d) min-depth)
                  (#{\) \] \}} c) (recur (inc j) (dec d) min-depth)
                  :else (recur (inc j) d min-depth))))))))))
