(ns vitambo.render
  "TamboUI-based rendering for the vim clone."
  (:import (dev.tamboui.tui TuiRunner TuiConfig)
           (dev.tamboui.tui.event KeyEvent KeyCode)
           (dev.tamboui.tui.keys Keys)
           (dev.tamboui.widgets.paragraph Paragraph)
           (dev.tamboui.text Text)
           (dev.tamboui.widgets.block Block)
           (dev.tamboui.layout Constraint)
           (dev.tamboui.terminal Terminal))
  (:require [clojure.string :as str]
            [vitambo.editor :as ed]
            [vitambo.buffer :as buf]
            [vitambo.mode :as mode]
            [vitambo.dispatch :as dispatch]
            [vitambo.search :as search]))

(defn- key-event->str
  "Convert a TamboUI KeyEvent to a string representation for the dispatcher."
  [^KeyEvent event]
  (let [ch (.getCharacter event)]
    (if (and ch (not= (int ch) 0))
      ;; Regular character
      (str ch)
      ;; Special key
      (let [code (.getCode event)]
        (when code
          (case code
            (:ESCAPE :ESC) "\u001b"
            (:ENTER :NEWLINE) "\n"
            (:TAB) "\t"
            (:BACKSPACE :DELETE) "\u007f"
            (:SPACE) " "
            ;; Arrow keys mapped to vim motions
            (:LEFT :UP :DOWN :RIGHT)
            (case code
              :LEFT "h"
              :UP "k" 
              :DOWN "j"
              :RIGHT "l")
            ;; Ctrl modifier detection
            ;; For shortcuts, check ctrl modifier
            nil))))))

(defn- key-event->ctrl-str
  "Check if a KeyEvent has Ctrl modifier and return the control character."
  [^KeyEvent event]
  (let [ch (.getCharacter event)]
    (when (and ch (not= (int ch) 0))
      (let [c (int ch)]
        ;; Ctrl+A through Ctrl+Z are codes 1-26
        (when (and (>= c 1) (<= c 26))
          (str (char c)))))))

(defn- char->ctrl-code
  "Convert a char like 'd' to its Ctrl code (e.g., Ctrl-D = code 4)."
  [^Character ch]
  (when ch
    (let [c (Character/toUpperCase (char ch))]
      (when (Character/isLetter c)
        (char (- (int c) 64))))))  ;; Ctrl+A = 1, Ctrl+B = 2, ...

(defn- style-for-mode
  "Get the ANSI color/style keyword for the current mode's cursor."
  [m]
  (case m
    mode/normal :white
    mode/insert :green
    mode/visual-char :magenta
    mode/visual-line :magenta
    mode/visual-block :magenta
    mode/command :yellow
    mode/search :yellow
    mode/operator-pending :yellow
    :white))

(defn render-status-line
  "Create a status line string for the editor."
  [editor]
  (let [split (ed/active-split editor)
        b (:buffer split)
        c (:cursor split)
        m (:mode split mode/normal)
        fname (or (:filename b) "[No Name]")
        modified (if (:modified b) " [+]" "")
        lines (buf/line-count b)
        row (inc (:row c))
        col (inc (:col c))
        pct (if (> lines 1) (int (* 100 (/ (float (:row c)) (float (dec lines))))) 0)
        mode-name (get mode/mode-names m "NORMAL")
        count-str (if (pos? (or (:count split) 0)) (str " " (:count split)) "")]
    (str " " mode-name count-str "   "
         fname modified "   "
         "Line " row "/" lines " Col " col "   "
         pct "%")))

(defn get-visible-lines
  "Get visible portion of buffer."
  [editor term-height]
  (let [split (ed/active-split editor)
        b (:buffer split)
        scroll-top (:scroll-top split 0)
        ;; Reserve 2 lines for status/message
        avail-height (max 3 (- term-height 2))
        end-line (min (+ scroll-top avail-height) (buf/line-count b))]
    (subvec (:lines b) scroll-top end-line)))

(defn- render-editor-frame
  "Render the editor state into a frame."
  [editor frame]
  (let [split (ed/active-split editor)
        b (:buffer split)
        c (:cursor split)
        scroll-top (:scroll-top split 0)
        area (.area frame)
        width (.width area)
        height (.height area)
        ;; Available lines for content (leave room for status line and cmdline)
        content-height (- height 2)
        total-lines (buf/line-count b)
        end-line (min (+ scroll-top content-height) total-lines)
        ;; Status line
        status-line (render-status-line editor)
        ;; Command line
        cmdline (:cmdline editor "")
        msg (:message editor "")
        has-cmdline? (or (not-empty cmdline) (not-empty msg))
        ;; Line number width
        ln-width 4
        text-width (- width ln-width 1)]

    ;; Render buffer content
    (let [lines-str (str/join "\n"
                      (for [i (range (- end-line scroll-top))]
                        (let [abs-line (+ scroll-top i)
                              line (buf/line-str b abs-line)
                              is-current (= abs-line (:row c))
                              marker (if is-current ">" " ")
                              truncated (if (and text-width (> (count line) text-width))
                                         (str (subs line 0 text-width) "…")
                                         (or line ""))]
                          (str marker (format "%3d" (inc abs-line)) " " truncated))))]
      (.renderWidget frame
        (Paragraph/builder (.text (Text/of lines-str)) .build)
        (.withY (.withX area 0) 0)))

    ;; Render status line
    (let [status-para (Paragraph/builder
                        (.text (Text/of (str status-line
                                            (if has-cmdline?
                                              (str "\n" cmdline msg)
                                              ""))))
                        .build)]
      (.renderWidget frame status-para
        (.withY (.withX area 0) (- height (if has-cmdline? 3 2)))))

    ;; Render command line separately
    (when has-cmdline?
      (let [cmd-para (Paragraph/builder
                       (.text (Text/of (str cmdline
                                          (when (and msg (not-empty msg))
                                            (str "  " msg)))))
                       .build)]
        (.renderWidget frame cmd-para
          (.withY (.withX area 0) (- height 2)))))))

(defn create-tui-config
  "Create the TuiConfig for the vim editor."
  []
  (-> (TuiConfig/builder)
      (.tickRate (java.time.Duration/ofMillis 50))
      (.mouseCapture false)
      .build))

(defn start-editor!
  "Start the vim editor TUI with the given initial editor state."
  [initial-editor]
  (let [config (create-tui-config)
        editor-state (atom initial-editor)]

    (try
      (with-open [tui (TuiRunner/create config)]
        (.run tui
          ;; ── Event handler ──
          (reify java.util.function.BiFunction
            (apply [this event runner]
              (cond
                (instance? KeyEvent event)
                (let [^KeyEvent ke event]
                  ;; Check quit (Ctrl+C or q)
                  (if (.isQuit ke)
                    (do (.quit runner) false)
                    (let [key-str (or (key-event->str ke) "")
                          ctrl-str (key-event->ctrl-str ke)]
                      ;; Try control character first
                      (let [k (if (and ctrl-str (not= ctrl-str key-str)) ctrl-str key-str)]
                        (when (and k (not-empty k))
                          (let [result (dispatch/handle-key @editor-state k)]
                            (when (map? result)
                              (let [new-state (if (:editor result) (:editor result) result)]
                                (reset! editor-state new-state))
                              ;; Check if we should quit
                              (when (false? (:running @editor-state true))
                                (.quit runner))))))
                      false)))
                :else false)))
          ;; ── Render function ──
          (reify java.util.function.Consumer
            (accept [this frame]
              (try
                (render-editor-frame @editor-state frame)
                (catch Exception e
                  (.printStackTrace e)))))))
      (catch Exception e
        (println "Error running editor:" (.getMessage e))
        (.printStackTrace e)))))
