(ns vitambo.buffer
  "Text buffer management for the vim clone.
   A buffer is a vector of line strings plus metadata."
  (:require [clojure.string :as str]))

(defn make-buffer
  "Create a new buffer from lines (vector of strings or single string)."
  ([]
   (make-buffer [""]))
  ([lines]
   (let [lines (if (string? lines) (str/split-lines lines) lines)
         lines (if (empty? lines) [""] (mapv str lines))]
     {:lines lines
      :filename nil
      :modified false
      :undo-stack []
      :redo-stack []
      :saved-lines lines})))

(defn line-count
  "Number of lines in the buffer."
  [buf]
  (count (:lines buf)))

(defn line-str
  "Get the text of line n (0-indexed)."
  [buf n]
  (nth (:lines buf) n nil))

(defn line-len
  "Length of line n (0-indexed)."
  [buf n]
  (count (line-str buf n)))

(defn insert-char
  "Insert a character at (row, col). Returns new buffer."
  [buf row col ch]
  (let [line (line-str buf row)
        new-line (str (subs line 0 col) ch (subs line col))
        new-lines (assoc (:lines buf) row new-line)]
    (assoc buf :lines new-lines :modified true)))

(defn delete-char
  "Delete character at (row, col). Returns nil if at end of buffer."
  [buf row col]
  (let [line (line-str buf row)]
    (if (>= col (count line))
      ;; At end of line, join with next if possible
      (if (< row (dec (line-count buf)))
        (let [next-line (line-str buf (inc row))
              joined (str line next-line)
              new-lines (-> (:lines buf)
                           (assoc row joined)
                           (concat (drop (+ row 2) (:lines buf)))
                           vec)]
          (assoc buf :lines new-lines :modified true))
        buf)
      ;; Delete char on same line
      (let [new-line (str (subs line 0 col) (subs line (inc col)))
            new-lines (assoc (:lines buf) row new-line)]
        (assoc buf :lines new-lines :modified true)))))

(defn split-line
  "Split line at col, moving rest to next line."
  [buf row col]
  (let [line (line-str buf row)
        before (subs line 0 col)
        after (subs line col)
        new-lines (vec (concat (take row (:lines buf))
                              [before after]
                              (drop (inc row) (:lines buf))))]
    (assoc buf :lines new-lines :modified true)))

(defn join-lines
  "Join line n with line n+1."
  [buf row]
  (if (< row (dec (line-count buf)))
    (let [current (line-str buf row)
          next (line-str buf (inc row))
          joined (str current next)
          new-lines (vec (concat (take row (:lines buf))
                                [joined]
                                (drop (+ row 2) (:lines buf))))]
      (assoc buf :lines new-lines :modified true))
    buf))

(defn insert-line
  "Insert a new (empty) line at given index."
  [buf row text]
  (let [new-lines (vec (concat (take row (:lines buf))
                              [(or text "")]
                              (drop row (:lines buf))))]
    (assoc buf :lines new-lines :modified true)))

(defn delete-line
  "Delete line at row."
  [buf row]
  (if (<= (line-count buf) 1)
    (assoc buf :lines [""] :modified true)
    (let [new-lines (vec (concat (take row (:lines buf))
                                (drop (inc row) (:lines buf))))]
      (assoc buf :lines new-lines :modified true))))

(defn replace-line
  "Replace line at row with new text."
  [buf row text]
  (let [new-lines (assoc (:lines buf) row (or text ""))]
    (assoc buf :lines new-lines :modified true)))

(defn yank-line
  "Get the text of line for yanking (includes newline)."
  [buf row]
  (str (line-str buf row) "\n"))

(defn insert-at
  "Insert text at (row, col). Returns [new-buf new-col]."
  [buf row col text]
  (if (empty? text)
    [buf col]
    (let [lines (str/split text #"\n")
          n-lines (count lines)]
      (if (= n-lines 1)
        ;; Single line insert
        (let [line (line-str buf row)
              new-line (str (subs line 0 col) (first lines) (subs line col))
              new-lines (assoc (:lines buf) row new-line)]
          [(assoc buf :lines new-lines :modified true) (+ col (count (first lines)))])
        ;; Multi-line insert
        (let [first-part (subs (line-str buf row) 0 col)
              last-part (subs (line-str buf row) col)
              new-lines (vec (concat (take row (:lines buf))
                                    [(str first-part (first lines))]
                                    (map str (butlast (rest lines)))
                                    [(str (last lines) last-part)]
                                    (drop (inc row) (:lines buf))))]
          [(assoc buf :lines new-lines :modified true) (count (last lines))])))))

(defn delete-range
  "Delete from (r1,c1) inclusive to (r2,c2) exclusive.
   Returns [new-buf end-row end-col]."
  [buf r1 c1 r2 c2]
  (if (= r1 r2)
    (let [line (line-str buf r1)
          new-line (str (subs line 0 c1) (subs line c2))
          new-lines (assoc (:lines buf) r1 new-line)]
      [(assoc buf :lines new-lines :modified true) r1 c1])
    (let [first-part (subs (line-str buf r1) 0 c1)
          last-part (subs (line-str buf r2) c2)
          middle (vec (concat [(str first-part (subs (line-str buf r2) c2))]
                             (drop (inc r2) (:lines buf))))
          new-lines (vec (concat (take r1 (:lines buf))
                                [(str first-part last-part)]
                                (drop (inc r2) (:lines buf))))]
      [(assoc buf :lines new-lines :modified true) r1 c1])))

(defn get-text
  "Get text from buffer, optionally from range."
  ([buf]
   (str/join "\n" (:lines buf)))
  ([buf r1 c1 r2 c2]
   (if (= r1 r2)
     (subs (line-str buf r1) c1 c2)
     (let [first-line (subs (line-str buf r1) c1)
           middle (subvec (:lines buf) (inc r1) r2)
           last-line (subs (line-str buf r2) 0 c2)]
       (str first-line "\n"
            (str/join "\n" middle)
            (when (seq middle) "\n")
            last-line)))))

(defn load-file!
  "Load a file into a new buffer."
  [filename]
  (let [content (slurp filename)]
    (assoc (make-buffer content)
           :filename filename
           :modified false
           :saved-lines (:lines (make-buffer content)))))

(defn save-file!
  "Save buffer to its file."
  [buf]
  (if-let [f (:filename buf)]
    (do (spit f (get-text buf))
        (assoc buf :modified false :saved-lines (:lines buf)))
    buf))

(defn save-as!
  "Save buffer to a new file."
  [buf filename]
  (spit filename (get-text buf))
  (assoc buf :filename filename :modified false :saved-lines (:lines buf)))
