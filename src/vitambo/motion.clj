(ns vitambo.motion
  "Cursor motion functions for the vim clone."
  (:require [vitambo.buffer :as buf]))

(defn whitespace? [s i]
  (and (>= i 0) (< i (count s))
       (Character/isWhitespace (nth s i))))

(defn word-char? [c]
  (or (Character/isLetter c) (Character/isDigit c) (= c \_)))

(defn word-start? [s i len]
  (and (< i len) (not (whitespace? s i))
       (or (= i 0) (whitespace? s (dec i))
           (and (not (word-char? (nth s i)))
                (word-char? (nth s (dec i)))))))

(defn word-end? [s i len]
  (and (>= i 0) (< i len) (not (whitespace? s i))
       (or (= i (dec len)) (whitespace? s (inc i))
           (and (not (word-char? (nth s i)))
                (word-char? (nth s (inc i)))))))

(defn WORD-start? [s i len]
  (and (< i len) (not (whitespace? s i))
       (or (= i 0) (whitespace? s (dec i)))))

(defn WORD-end? [s i len]
  (and (>= i 0) (< i len) (not (whitespace? s i))
       (or (= i (dec len)) (whitespace? s (inc i)))))

(def ^:private open-brackets #{40 91 123 60})
(def ^:private close-brackets #{41 93 125 62})
(def ^:private bracket-pairs {40 41 41 40 91 93 93 91 123 125 125 123 60 62 62 60})

(defn find-matching-bracket [s pos]
  (let [ch (nth s pos nil)]
    (when (and ch (or (open-brackets (int ch)) (close-brackets (int ch))))
      (let [open? (open-brackets (int ch))
            match (char (get bracket-pairs (int ch)))
            dir (if open? 1 -1) len (count s)]
        (loop [i (+ pos dir) depth 1]
          (cond (< i 0) nil (>= i len) nil
                (= (nth s i) match) (if (= depth 1) i (recur (+ i dir) (dec depth)))
                (= (nth s i) ch) (recur (+ i dir) (inc depth))
                :else (recur (+ i dir) depth)))))))

(defn move-h [lines row col cnt]
  {:row row :col (max 0 (- col cnt))})

(defn move-l [lines row col cnt]
  (let [line (nth lines row "")
        c (min (+ col cnt) (count line))]
    {:row row :col c}))

(defn move-j [lines row col cnt]
  (let [r (min (+ row cnt) (max 0 (dec (count lines))))
        line (nth lines r "")
        c (min col (count line))]
    {:row r :col c}))

(defn move-k [lines row col cnt]
  (let [r (max 0 (- row cnt))
        line (nth lines r "")
        c (min col (count line))]
    {:row r :col c}))

(defn move-0 [lines row _col _cnt] {:row row :col 0})

(defn move-$ [lines row _col _cnt]
  {:row row :col (count (nth lines row ""))})

(defn move-first [lines row _col _cnt]
  (let [line (nth lines row "")
        idx (loop [i 0] (if (>= i (count line)) 0
                         (if (Character/isWhitespace (nth line i))
                           (recur (inc i)) i)))]
    {:row row :col idx}))

(defn move-gg [lines _row _col cnt]
  (let [r (max 0 (dec (or cnt 1)))]
    {:row (min r (max 0 (dec (count lines)))) :col 0}))

(defn move-G [lines _row _col cnt]
  (let [last (max 0 (dec (count lines)))
        r (if (and cnt (pos? cnt)) (min (dec cnt) last) last)]
    {:row r :col 0}))

(defn move-w [lines row col cnt]
  (let [n (count lines)]
    (loop [rem cnt r row c col]
      (if (<= rem 0) {:row r :col c}
        (let [line (nth lines r "") len (count line)]
          (if (>= c len)
            (if (< r (dec n)) (recur rem (inc r) 0) {:row r :col len})
            (let [ns (loop [j (inc c)]
                       (cond (>= j len) :nl
                             (word-start? line j len) j
                             :else (recur (inc j))))]
              (if (= ns :nl) (recur rem (inc r) 0) (recur (dec rem) r ns)))))))))

(defn move-b [lines row col cnt]
  (let [n (count lines)]
    (loop [rem cnt r row c col]
      (if (<= rem 0) {:row r :col c}
        (let [line (nth lines r "")]
          (if (<= c 0)
            (if (> r 0) (recur rem (dec r) (count (nth lines (dec r) ""))) {:row 0 :col 0})
            (let [ps (loop [j (dec c)]
                       (if (< j 0) :pl
                         (if (word-start? line j (count line)) j (recur (dec j)))))]
              (if (= ps :pl)
                (if (> r 0) (recur rem (dec r) (count (nth lines (dec r) ""))) {:row 0 :col 0})
                (recur (dec rem) r ps)))))))))

(defn move-e [lines row col cnt]
  (let [n (count lines)]
    (loop [rem cnt r row c col]
      (if (<= rem 0) {:row r :col c}
        (let [line (nth lines r "") len (count line)]
          (if (>= c len)
            (if (< r (dec n)) (recur rem (inc r) 0) {:row r :col len})
            (let [ne (loop [j (inc c)]
                       (if (>= j len) :nl
                         (if (word-end? line j len) j (recur (inc j)))))]
              (if (= ne :nl) (recur rem (inc r) 0) (recur (dec rem) r ne)))))))))

(defn move-W [lines row col cnt]
  (let [n (count lines)]
    (loop [rem cnt r row c col]
      (if (<= rem 0) {:row r :col c}
        (let [line (nth lines r "") len (count line)]
          (if (>= c len)
            (if (< r (dec n)) (recur rem (inc r) 0) {:row r :col len})
            (let [ns (loop [j (inc c)]
                       (cond (>= j len) :nl
                             (WORD-start? line j len) j
                             :else (recur (inc j))))]
              (if (= ns :nl) (recur rem (inc r) 0) (recur (dec rem) r ns)))))))))

(defn move-B [lines row col cnt]
  (let [n (count lines)]
    (loop [rem cnt r row c col]
      (if (<= rem 0) {:row r :col c}
        (let [line (nth lines r "")]
          (if (<= c 0)
            (if (> r 0) (recur rem (dec r) (count (nth lines (dec r) ""))) {:row 0 :col 0})
            (let [ps (loop [j (dec c)]
                       (if (< j 0) :pl
                         (if (WORD-start? line j (count line)) j (recur (dec j)))))]
              (if (= ps :pl)
                (if (> r 0) (recur rem (dec r) (count (nth lines (dec r) ""))) {:row 0 :col 0})
                (recur (dec rem) r ps)))))))))

(defn move-E [lines row col cnt]
  (let [n (count lines)]
    (loop [rem cnt r row c col]
      (if (<= rem 0) {:row r :col c}
        (let [line (nth lines r "") len (count line)]
          (if (>= c len)
            (if (< r (dec n)) (recur rem (inc r) 0) {:row r :col len})
            (let [ne (loop [j (inc c)]
                       (if (>= j len) :nl
                         (if (WORD-end? line j len) j (recur (inc j)))))]
              (if (= ne :nl) (recur rem (inc r) 0) (recur (dec rem) r ne)))))))))

(defn move-ge [lines row col cnt] (move-b lines row col cnt))

(defn move-prev-para [lines row _col _cnt]
  (loop [r (dec row)]
    (cond (<= r 0) {:row 0 :col 0}
          (empty? (nth lines r "")) {:row (dec r) :col 0}
          :else (recur (dec r)))))

(defn move-next-para [lines row _col _cnt]
  (let [n (count lines)]
    (loop [r (inc row)]
      (cond (>= r n) {:row (dec n) :col (count (nth lines (dec n) ""))}
            (>= r (dec n)) {:row (dec n) :col (count (nth lines (dec n) ""))}
            (empty? (nth lines r "")) {:row (inc r) :col 0}
            :else (recur (inc r))))))

(defn move-f [lines row col _cnt ch]
  (let [line (nth lines row "")
        idx (loop [i (inc col)]
              (cond (>= i (count line)) nil
                    (= (nth line i) ch) i
                    :else (recur (inc i))))]
    (or (and idx {:row row :col idx}) {:row row :col col})))

(defn move-F [lines row col _cnt ch]
  (let [line (nth lines row "")
        idx (loop [i (dec col)]
              (cond (< i 0) nil
                    (= (nth line i) ch) i
                    :else (recur (dec i))))]
    (or (and idx {:row row :col idx}) {:row row :col col})))

(defn move-t [lines row col _cnt ch]
  (let [line (nth lines row "")
        idx (loop [i (inc col)]
              (cond (>= i (count line)) nil
                    (= (nth line i) ch) (dec i)
                    :else (recur (inc i))))]
    (or (and idx {:row row :col idx}) {:row row :col col})))

(defn move-T [lines row col _cnt ch]
  (let [line (nth lines row "")
        idx (loop [i (dec col)]
              (cond (< i 0) nil
                    (= (nth line i) ch) (inc i)
                    :else (recur (dec i))))]
    (or (and idx {:row row :col idx}) {:row row :col col})))

(def motions
  {:h move-h :j move-j :k move-k :l move-l
   :w move-w :b move-b :e move-e
   :W move-W :B move-B :E move-E
   :ge move-ge :gE move-E
   :0 move-0 :dollar move-$ :caret move-first
   :gg move-gg :G move-G
   :prev-para move-prev-para :next-para move-next-para})

(defn apply-motion [motion-key lines row col cnt & args]
  (if-let [mf (get motions motion-key)]
    (apply mf lines row col cnt args)
    nil))
