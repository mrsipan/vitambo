(ns vitambo.search
  "Search functionality for the vim clone."
  (:require [vitambo.buffer :as buf]))

(defn search-forward
  "Search forward from (row, col) for pattern.
   Returns [{:row :col} match-str] or nil."
  [buffer row col pattern]
  (try
    (let [lines (:lines buffer)
          n (count lines)
          p (re-pattern pattern)]
      ;; Search from col+1 on current line
      (loop [r row c (if (>= row n) 0 (inc col))]
        (if (>= r n)
          ;; Wrap to start
          (loop [r2 0 c2 0]
            (if (>= r2 row)
              nil
              (let [line (nth lines r2 "")
                    m (re-find p (subs line c2))]
                (if m
                  (let [idx (.indexOf line m)]
                    {:row r2 :col idx})
                  (recur (inc r2) 0)))))
          (let [line (nth lines r "")
                remaining (subs line c)
                m (re-find p remaining)]
            (if m
              (let [idx (.indexOf line m (max 0 c))]
                {:row r :col idx})
              (recur (inc r) 0))))))
    (catch Exception e nil)))

(defn search-backward
  "Search backward from (row, col) for pattern."
  [buffer row col pattern]
  (try
    (let [lines (:lines buffer)
          n (count lines)
          p (re-pattern pattern)]
      (loop [r row c (dec col)]
        (if (< r 0)
          ;; Wrap to end
          (loop [r2 (dec n)]
            (if (<= r2 row)
              nil
              (let [line (nth lines r2 "")
                    m (re-find p line)]
                (if m
                  (let [idx (.indexOf line m)]
                    {:row r2 :col idx})
                  (recur (dec r2))))))
          (let [line (nth lines r "")
                ;; Find the LAST occurrence on this line before c
                search-end (if (>= r row) (inc c) (count line))
                m (last (re-seq p (subs line 0 search-end)))]
            (if m
              (let [idx (.lastIndexOf line m (dec search-end))]
                {:row r :col idx})
              (recur (dec r) (count (nth lines (dec r) ""))))))))
    (catch Exception e nil)))

(defn search-word-under-cursor
  "Extract word at cursor position and return search pattern."
  [buffer row col]
  (let [line (nth (:lines buffer) row "")]
    (if (empty? line)
      nil
      (let [start (loop [i col]
                    (if (or (<= i 0)
                            (not (or (Character/isLetter (nth line (dec i)))
                                    (Character/isDigit (nth line (dec i))))))
                      i
                      (recur (dec i))))
            end (loop [i col]
                  (if (or (>= i (count line))
                          (not (or (Character/isLetter (nth line i))
                                  (Character/isDigit (nth line i)))))
                    i
                    (recur (inc i))))]
        (if (< start end)
          (re-pattern (subs line start end))
          nil)))))
