(ns vitambo.text-object
  "Text objects for vim clone: iw, aw, iW, aW, i\", a\", etc.")

(defn word-range
  "Find the range of a word at position. Returns [start end] or nil."
  [s pos]
  (when (and (>= pos 0) (< pos (count s)) (not (Character/isWhitespace (nth s pos))))
    (let [len (count s)
          start (loop [i pos]
                  (if (or (<= i 0) (Character/isWhitespace (nth s (dec i))))
                    i
                    (recur (dec i))))
          end (loop [i pos]
                (if (or (>= i (dec len)) (Character/isWhitespace (nth s (inc i))))
                  (inc i)
                  (recur (inc i))))]
      [start end])))

(defn WORD-range
  "Find the range of a WORD at position. WORD is non-whitespace sequence."
  [s pos]
  (when (and (>= pos 0) (< pos (count s)) (not (Character/isWhitespace (nth s pos))))
    (let [len (count s)
          start (loop [i pos]
                  (if (or (<= i 0) (Character/isWhitespace (nth s (dec i))))
                    i
                    (recur (dec i))))
          end (loop [i pos]
                (if (or (>= i (dec len)) (Character/isWhitespace (nth s (inc i))))
                  (inc i)
                  (recur (inc i))))]
      [start end])))

(defn find-matching
  "Find matching delimiter in string, returning position.
   Supports paired delimiters."
  [s pos]
  (let [pairs {\( \) \) \(
               \[ \] \] \[
               \{ \} \} \{
               \< \> \> \<
               \" \" \' \'}]
    (when-let [ch (get s pos)]
      (if-let [match (get pairs ch)]
        (let [dir (if (#{\( \[ \{ \< \" \'} ch) 1 -1)]
          (loop [i (+ pos dir) depth 1]
            (cond
              (< i 0) nil
              (>= i (count s)) nil
              (= (nth s i) match) (if (= depth 1) i (recur (+ i dir) (dec depth)))
              (= (nth s i) ch) (recur (+ i dir) (inc depth))
              :else (recur (+ i dir) depth))))
        nil))))

(defn inner-quote-range
  "Find the range inside quotes at position. Returns [start end] or nil."
  [s pos quote-char]
  (let [len (count s)]
    (loop [i (dec pos)]
      (if (< i 0)
        nil
        (if (= (nth s i) quote-char)
          (loop [j (inc pos)]
            (if (>= j len)
              nil
              (if (= (nth s j) quote-char)
                [(inc i) j]
                (recur (inc j)))))
          (recur (dec i)))))))

(defn inner-pair-range
  "Find range inside a pair of delimiters. Returns [start end] or nil."
  [s pos open close]
  (let [len (count s)]
    (loop [depth 1 i (dec pos)]
      (cond
        (< i 0) nil
        (= (nth s i) open) (if (= depth 1)
                            (loop [depth2 1 j (inc pos)]
                              (cond
                                (>= j len) nil
                                (= (nth s j) open) (recur (inc depth2) (inc j))
                                (= (nth s j) close) (if (= depth2 1) [(inc i) j] (recur (dec depth2) (inc j)))
                                :else (recur depth2 (inc j))))
                            (recur (dec depth) (dec i)))
        (= (nth s i) close) (recur (inc depth) (dec i))
        :else (recur depth (dec i))))))

(defn a-pair-range
  "Find range including a pair of delimiters (a(, a[, a{, a<, a\", a')."
  [s pos open close]
  (let [len (count s)]
    (loop [depth 1 i (dec pos)]
      (cond
        (< i 0) nil
        (= (nth s i) open) (if (= depth 1)
                            (loop [depth2 1 j (inc pos)]
                              (cond
                                (>= j len) nil
                                (= (nth s j) open) (recur (inc depth2) (inc j))
                                (= (nth s j) close) (if (= depth2 1) [i (inc j)] (recur (dec depth2) (inc j)))
                                :else (recur depth2 (inc j))))
                            (recur (dec depth) (dec i)))
        (= (nth s i) close) (recur (inc depth) (dec i))
        :else (recur depth (dec i))))))

(def ^:private iw-keyword (keyword "iw"))
(def ^:private aw-keyword (keyword "aw"))
(def ^:private iW-keyword (keyword "iW"))
(def ^:private aW-keyword (keyword "aW"))
(def ^:private i-quote-keyword (keyword "i\""))
(def ^:private a-quote-keyword (keyword "a\""))
(def ^:private i-squote-keyword (keyword "i'"))
(def ^:private a-squote-keyword (keyword "a'"))
(def ^:private i-paren (keyword "i("))
(def ^:private a-paren (keyword "a("))
(def ^:private i-rparen (keyword "i)"))
(def ^:private a-rparen (keyword "a)"))
(def ^:private i-bracket (keyword "i["))
(def ^:private a-bracket (keyword "a["))
(def ^:private i-rbracket (keyword "i]"))
(def ^:private a-rbracket (keyword "a]"))
(def ^:private i-brace (keyword "i{"))
(def ^:private a-brace (keyword "a{"))
(def ^:private i-rbrace (keyword "i}"))
(def ^:private a-rbrace (keyword "a}"))
(def ^:private i-angle (keyword "i<"))
(def ^:private a-angle (keyword "a<"))
(def ^:private i-rangle (keyword "i>"))
(def ^:private a-rangle (keyword "a>"))

(def text-object-dispatch
  "Map from text object keyword to handler fn [buffer row col] returning [r1 c1 r2 c2]"
  {iw-keyword (fn [buffer row col]
                (let [line (nth (:lines buffer) row)]
                  (when-let [[s e] (word-range line col)]
                    [row s row e])))
   aw-keyword (fn [buffer row col]
                (let [line (nth (:lines buffer) row)]
                  (when-let [[s e] (word-range line col)]
                    [row s row e])))
   iW-keyword (fn [buffer row col]
                (let [line (nth (:lines buffer) row)]
                  (when-let [[s e] (WORD-range line col)]
                    [row s row e])))
   aW-keyword (fn [buffer row col]
                (let [line (nth (:lines buffer) row)]
                  (when-let [[s e] (WORD-range line col)]
                    [row s row e])))
   i-quote-keyword (fn [buffer row col]
                     (let [line (nth (:lines buffer) row)]
                       (when-let [[s e] (inner-quote-range line col \")]
                         [row s row e])))
   a-quote-keyword (fn [buffer row col]
                     (let [line (nth (:lines buffer) row)]
                       (when-let [[s e] (inner-quote-range line col \")]
                         [row (dec s) row e])))
   i-squote-keyword (fn [buffer row col]
                      (let [line (nth (:lines buffer) row)]
                        (when-let [[s e] (inner-quote-range line col \')]
                          [row s row e])))
   a-squote-keyword (fn [buffer row col]
                      (let [line (nth (:lines buffer) row)]
                        (when-let [[s e] (inner-quote-range line col \')]
                          [row (dec s) row e])))
   i-paren (fn [buffer row col]
             (let [line (nth (:lines buffer) row)]
               (when-let [[s e] (inner-pair-range line col \( \))]
                 [row s row e])))
   a-paren (fn [buffer row col]
             (let [line (nth (:lines buffer) row)]
               (when-let [[s e] (a-pair-range line col \( \))]
                 [row s row e])))
   i-rparen (fn [buffer row col]
              (let [line (nth (:lines buffer) row)]
                (when-let [[s e] (inner-pair-range line col \( \))]
                  [row s row e])))
   a-rparen (fn [buffer row col]
              (let [line (nth (:lines buffer) row)]
                (when-let [[s e] (a-pair-range line col \( \))]
                  [row s row e])))
   i-bracket (fn [buffer row col]
               (let [line (nth (:lines buffer) row)]
                 (when-let [[s e] (inner-pair-range line col \[ \])]
                   [row s row e])))
   a-bracket (fn [buffer row col]
               (let [line (nth (:lines buffer) row)]
                 (when-let [[s e] (a-pair-range line col \[ \])]
                   [row s row e])))
   i-rbracket (fn [buffer row col]
                (let [line (nth (:lines buffer) row)]
                  (when-let [[s e] (inner-pair-range line col \[ \])]
                    [row s row e])))
   a-rbracket (fn [buffer row col]
                (let [line (nth (:lines buffer) row)]
                  (when-let [[s e] (a-pair-range line col \[ \])]
                    [row s row e])))
   i-brace (fn [buffer row col]
             (let [line (nth (:lines buffer) row)]
               (when-let [[s e] (inner-pair-range line col \{ \})]
                 [row s row e])))
   a-brace (fn [buffer row col]
             (let [line (nth (:lines buffer) row)]
               (when-let [[s e] (a-pair-range line col \{ \})]
                 [row s row e])))
   i-rbrace (fn [buffer row col]
              (let [line (nth (:lines buffer) row)]
                (when-let [[s e] (inner-pair-range line col \{ \})]
                  [row s row e])))
   a-rbrace (fn [buffer row col]
              (let [line (nth (:lines buffer) row)]
                (when-let [[s e] (a-pair-range line col \{ \})]
                  [row s row e])))
   i-angle (fn [buffer row col]
             (let [line (nth (:lines buffer) row)]
               (when-let [[s e] (inner-pair-range line col \< \>)]
                 [row s row e])))
   a-angle (fn [buffer row col]
             (let [line (nth (:lines buffer) row)]
               (when-let [[s e] (a-pair-range line col \< \>)]
                 [row s row e])))
   i-rangle (fn [buffer row col]
              (let [line (nth (:lines buffer) row)]
                (when-let [[s e] (inner-pair-range line col \< \>)]
                  [row s row e])))
   a-rangle (fn [buffer row col]
              (let [line (nth (:lines buffer) row)]
                (when-let [[s e] (a-pair-range line col \< \>)]
                  [row s row e])))})

(defn text-object-range
  "Get [start-row start-col end-row end-col] for a text object.
   buffer: the buffer map
   row, col: current cursor position
   obj: keyword like :iw, :aw, :iW, :aW, :i\", :a\", etc."
  [buffer row col obj]
  (if-let [handler (get text-object-dispatch obj)]
    (handler buffer row col)
    nil))
