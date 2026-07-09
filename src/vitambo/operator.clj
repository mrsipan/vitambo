(ns vitambo.operator
  "Operators for the vim clone (d, c, y, p, >, <, ~, x)."
  (:require [clojure.string :as str]
            [vitambo.buffer :as buf]
            [vitambo.motion :as motion]))

(defn get-region
  "Get the text region defined by start and end positions.
   Returns {:text text :type :char/:line/:block}."
  [buf start-pos end-pos mode]
  (let [[r1 c1] start-pos
        [r2 c2] end-pos
        [r1 c1 r2 c2] (if (or (< r1 r2) (and (= r1 r2) (<= c1 c2)))
                        [r1 c1 r2 c2]
                        [r2 c2 r1 c1])]
    (case mode
      :line
      {:text (str/join "\n" (subvec (:lines buf) r1 (inc r2)))
       :start [r1 0]
       :end [r2 (count (nth (:lines buf) r2 ""))]
       :type :line}
      :block
      (let [c-min (min c1 c2)
            c-max (max c1 c2)]
        {:text (str/join "\n"
                (for [i (range r1 (inc r2))]
                  (let [line (nth (:lines buf) i "")]
                    (subs line c-min (min c-max (count line))))))
         :start [r1 c-min]
         :end [r2 c-max]
         :type :block})
      ;; char mode
      {:text (buf/get-text buf r1 c1 r2 c2)
       :start [r1 c1]
       :end [r2 c2]
       :type :char})))

;; ── Linewise operators ──

(defn op-delete
  "Delete region. Returns [new-buf, yank-text, new-pos]."
  [buf start-pos end-pos mode yank-reg]
  (let [[r1 c1] start-pos
        [r2 c2] end-pos
        [r1 c1 r2 c2] (if (or (< r1 r2) (and (= r1 r2) (<= c1 c2)))
                        [r1 c1 r2 c2]
                        [r2 c2 r1 c1])]
    (if (= mode :line)
      (let [yanked (str/join "\n" (subvec (:lines buf) r1 (inc r2)))
            new-buf (if (= (buf/line-count buf) (inc (- r2 r1)))
                      (buf/make-buffer [""])
                      (let [nl (vec (concat (take r1 (:lines buf))
                                           (drop (inc r2) (:lines buf))))]
                        (assoc buf :lines nl :modified true)))]
        [new-buf yanked [r1 0]])
      ;; char/block mode
      (let [yanked (buf/get-text buf r1 c1 r2 c2)
            lines (:lines buf)]
        (if (= r1 r2)
          (let [line (nth lines r1)
                new-line (str (subs line 0 c1) (subs line c2))
                new-lines (assoc lines r1 new-line)]
            [(assoc buf :lines new-lines :modified true) yanked [r1 c1]])
          ;; multi-line
          (let [first-part (subs (nth lines r1) 0 c1)
                last-part (subs (nth lines r2) c2)
                new-lines (vec (concat (take r1 lines)
                                      [(str first-part last-part)]
                                      (drop (inc r2) lines)))]
            [(assoc buf :lines new-lines :modified true) yanked [r1 c1]]))))))

(defn op-change
  "Change region (delete + enter insert mode). Returns [new-buf, yank-text, new-pos, insert?]."
  [buf start-pos end-pos mode]
  (let [[new-buf yanked [nr nc]] (op-delete buf start-pos end-pos mode nil)]
    [new-buf yanked [nr nc] true]))

(defn op-yank
  "Yank region (copy to register). Returns yank-text."
  [buf start-pos end-pos mode]
  (:text (get-region buf start-pos end-pos mode)))

(defn op-put-after
  "Put yanked text after cursor. Returns [new-buf new-pos]."
  [buf row col yanked]
  (if (nil? yanked)
    [buf [row col]]
    (let [[new-buf new-col] (buf/insert-at buf row col yanked)]
      [new-buf [row new-col]])))

(defn op-put-before
  "Put yanked text before cursor."
  [buf row col yanked]
  (if (nil? yanked)
    [buf [row col]]
    (let [text (if (str/ends-with? yanked "\n")
                 yanked
                 (str yanked "\n"))]
      (if (str/ends-with? text "\n")
        ;; Linewise: insert new line before current
        (let [new-buf (buf/insert-line buf row text)]
          [new-buf [row 0]])
        (let [[new-buf new-col] (buf/insert-at buf row col yanked)]
          [new-buf [row new-col]])))))

(defn op-indent
  "Indent region (add spaces). Returns [new-buf new-pos]."
  [buf start-pos end-pos mode]
  (let [[r1 c1] start-pos [r2 c2] end-pos
        [r1 r2] (if (< r1 r2) [r1 r2] [r2 r1])
        tab-stops (or (some-> buf :settings :tab-width) 4)
        new-lines (mapv (fn [i]
                         (if (and (>= i r1) (<= i r2))
                           (str (apply str (repeat tab-stops " ")) (nth (:lines buf) i))
                           (nth (:lines buf) i)))
                       (range (count (:lines buf))))]
    [(assoc buf :lines new-lines :modified true) [r1 (+ c1 tab-stops)]]))

(defn op-outdent
  "Outdent region (remove leading spaces)."
  [buf start-pos end-pos mode]
  (let [[r1 c1] start-pos [r2 c2] end-pos
        [r1 r2] (if (< r1 r2) [r1 r2] [r2 r1])
        tab-stops (or (some-> buf :settings :tab-width) 4)
        new-lines (mapv (fn [i]
                         (if (and (>= i r1) (<= i r2))
                           (let [line (nth (:lines buf) i "")]
                             (loop [spaces 0]
                               (if (and (< spaces tab-stops)
                                        (< spaces (count line))
                                        (= (nth line spaces) \space))
                                 (recur (inc spaces))
                                 (subs line spaces))))
                           (nth (:lines buf) i)))
                       (range (count (:lines buf))))]
    [(assoc buf :lines new-lines :modified true) [r1 (max 0 (- c1 tab-stops))]]))

(defn op-toggle-case
  "Toggle case of character(s) in region."
  [buf start-pos end-pos mode]
  (let [[r1 c1] start-pos [r2 c2] end-pos]
    (if (= r1 r2)
      (let [line (nth (:lines buf) r1)
            ch (nth line c1)
            new-ch (if (Character/isUpperCase ch)
                     (Character/toLowerCase ch)
                     (Character/toUpperCase ch))
            new-line (str (subs line 0 c1) new-ch (subs line (inc c1)))
            new-lines (assoc (:lines buf) r1 new-line)]
        [(assoc buf :lines new-lines :modified true) [r1 c1]])
      buf)))

(defn op-delete-char
  "Delete count chars at cursor (x)."
  [buf row col cnt]
  (let [cnt (or cnt 1)
        line (nth (:lines buf) row)
        max-del (min cnt (- (count line) col))
        yanked (subs line col (+ col max-del))]
    (if (> max-del 0)
      (let [new-line (str (subs line 0 col) (subs line (+ col max-del)))
            new-lines (assoc (:lines buf) row new-line)]
        [(assoc buf :lines new-lines :modified true) yanked [row col]])
      [buf nil [row col]])))
