(ns vitambo.editor
  "Core editor state management."
  (:require [vitambo.buffer :as buf]
            [vitambo.mode :as mode]))

;; A split is an independent editing context
(defn make-split-state
  "Create a new split state with its own buffer, cursor, etc."
  ([]
   (make-split-state (buf/make-buffer)))
  ([buffer]
   {:buffer buffer
    :cursor {:row 0 :col 0}
    :scroll-top 0
    :mode mode/normal
    ;; Visual selection
    :visual-start nil
    :visual-end nil
    ;; Yank register
    :registers {"" nil "\"" nil "0" nil "1" nil "2" nil "3" nil "4" nil
                "5" nil "6" nil "7" nil "8" nil "9" nil}
    ;; Last change for dot-repeat
    :last-change nil
    ;; Undo/redo stacks
    :undo-stack []
    :redo-stack []
    ;; Search state
    :search-pattern nil
    :search-direction :forward
    ;; Command-line input
    :command-buffer ""
    ;; Key sequence accumulator (for multi-key commands)
    :key-acc ""
    ;; Count prefix
    :count 0
    ;; Last f/t/F/T char
    :last-ft-char nil
    :last-ft-direction :forward
    ;; Operator pending state
    :operator nil
    ;; Motion pending state
    :motion-arg nil
    ;; Insert mode accumulated text (for dot repeat)
    :insert-text ""
    ;; Mappings (normal mode key seq -> seq)
    :mappings {}
    ;; Operator-pending state
    :operator-pending nil}))

(defn editor-state
  "Create the full editor state (multi-split)."
  ([]
   (editor-state (buf/make-buffer)))
  ([buffer]
   {:splits [(make-split-state buffer)]
    :active-split 0
    :running true
    :message nil
    ;; Global command line for : and / modes
    :cmdline ""
    :cmdline-type nil  ;; :command, :search-forward, :search-backward
    :cmdline-cursor 0}))

(defn active-split
  "Get the currently active split."
  [editor]
  (get-in editor [:splits (:active-split editor 0)]))

(defn update-active-split
  "Update the active split."
  [editor f & args]
  (let [idx (:active-split editor 0)
        split (get-in editor [:splits idx])
        new-split (apply f split args)]
    (assoc-in editor [:splits idx] new-split)))

(defn set-mode
  "Set the mode of the active split."
  [editor m]
  (update-active-split editor assoc :mode m))

(defn cursor
  "Get cursor from active split."
  [editor]
  (:cursor (active-split editor)))

(defn set-cursor
  "Set cursor position in active split."
  [editor row col]
  (update-active-split editor
    (fn [s]
      (assoc s :cursor {:row (max 0 row) :col (max 0 col)}))))

(defn move-cursor
  "Move cursor by delta row/col."
  [editor dr dc]
  (let [c (cursor editor)]
    (set-cursor editor (+ (:row c) dr) (+ (:col c) dc))))

(defn clamp-cursor
  "Clamp cursor to valid buffer position."
  [editor]
  (let [split (active-split editor)
        b (:buffer split)
        c (:cursor split)
        max-row (max 0 (dec (buf/line-count b)))
        row (min (:row c) max-row)
        line-len (buf/line-len b row)
        col (min (:col c) line-len)]
    (update-active-split editor assoc :cursor {:row row :col col})))

(defn with-buffer
  "Apply fn to active buffer, passing editor and current buffer."
  [editor fn-buf & args]
  (let [split (active-split editor)
        b (:buffer split)
        [new-buf & rest] (apply fn-buf b args)]
    (if (= new-buf b)
      editor
      (let [new-split (assoc split :buffer new-buf)]
        (if (seq rest)
          [editor (assoc-in editor [:splits (:active-split editor)] new-split) (first rest)]
          (assoc-in editor [:splits (:active-split editor)] new-split))))))

(defn push-undo
  "Push current buffer state onto undo stack."
  [editor]
  (update-active-split editor
    (fn [s]
      (-> s
          (update :undo-stack conj {:lines (:lines (:buffer s))
                                    :cursor (:cursor s)})
          (assoc :redo-stack [])))))
