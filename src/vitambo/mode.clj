(ns vitambo.mode
  "Mode definitions and state management for the vim clone.")

;; Mode keywords
(def normal :normal)
(def insert :insert)
(def visual-char :visual-char)
(def visual-line :visual-line)
(def visual-block :visual-block)
(def command :command)
(def search :search)
(def operator-pending :operator-pending)

;; Mode display names
(def mode-names
  {normal "NORMAL"
   insert "INSERT"
   visual-char "VISUAL"
   visual-line "VISUAL LINE"
   visual-block "VISUAL BLOCK"
   command "COMMAND"
   search "SEARCH"
   operator-pending "OPERATOR"})

;; Mode colors (for cursor rendering)
(def mode-colors
  {normal :white
   insert :green
   visual-char :magenta
   visual-line :magenta
   visual-block :magenta
   command :yellow
   search :yellow
   operator-pending :yellow})

;; Cursor shape per mode (block, bar, underline)
(def mode-cursor-shapes
  {normal :block
   insert :bar
   visual-char :block
   visual-line :block
   visual-block :block
   command :block
   search :block
   operator-pending :block})
