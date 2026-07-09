(ns vitambo.render
  "TamboUI-based rendering for the vim clone."
  (:import (dev.tamboui.tui TuiRunner TuiConfig)
           (dev.tamboui.tui.event KeyEvent KeyCode)
           (dev.tamboui.tui EventHandler Renderer)
           (dev.tamboui.widgets.paragraph Paragraph)
           (dev.tamboui.terminal Frame)
           (dev.tamboui.layout Rect))
  (:require [clojure.string :as str]
            [vitambo.editor :as ed]
            [vitambo.buffer :as buf]
            [vitambo.mode :as mode]
            [vitambo.dispatch :as dispatch]))

(defn- key-event->str
  "Convert a KeyEvent to a string for the dispatcher."
  [^KeyEvent event]
  (let [ch (.character event)]
    (if (and ch (not= (int ch) 0))
      ;; Regular character
      (let [c (int ch)]
        (case c
          10 "\n"    ;; Enter/Newline
          13 "\n"    ;; Carriage return
          27 "\u001b"  ;; Escape
          8 "\u007f"  ;; Backspace (BS)
          127 "\u007f" ;; Delete/Backspace (DEL)
          9 "\t"     ;; Tab
          (str ch)))
      ;; Non-character key - check key code
      (let [code (.code event)]
        (when code
          (case code
            (:UP) "k"
            (:DOWN) "j"
            (:LEFT) "h"
            (:RIGHT) "l"
            (:ENTER :NEWLINE) "\n"
            (:TAB) "\t"
            (:ESCAPE) "\u001b"
            (:BACKSPACE :DELETE) "\u007f"
            nil))))))

(defn- key-event->ctrl-str
  "Get Ctrl+letter code from a KeyEvent.
   Ctrl+A through Ctrl+Z have codes 1-26, but Tab(9), Enter(10), Esc(27) are excluded."
  [^KeyEvent event]
  (let [ch (.character event)]
    (when (and ch (not= (int ch) 0))
      (let [c (int ch)]
        (when (and (>= c 1) (<= c 26) (not= c 9) (not= c 10) (not= c 13))
          (str (char c)))))))

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
        cnt-str (if (pos? (or (:count split) 0)) (str " " (:count split)) "")]
    (str " " mode-name cnt-str "   "
         fname modified "   "
         "Line " row "/" lines " Col " col "   "
         pct "%")))

(defn- render-editor-frame
  "Render the editor state into a frame."
  [editor ^Frame frame]
  (let [split (ed/active-split editor)
        b (:buffer split)
        c (:cursor split)
        scroll-top (:scroll-top split 0)
        area (.area frame)
        width (.width area)
        height (.height area)
        content-height (- height 2)
        total-lines (buf/line-count b)
        end-line (min (+ scroll-top content-height) total-lines)
        status-line (render-status-line editor)
        cmdline (:cmdline editor "")
        msg (:message editor "")
        has-cmdline? (or (not-empty cmdline) (not-empty msg))
        text-width (- width 5)]
    ;; Render buffer content
    (let [lines-str (str/join "\n"
                      (for [i (range (- end-line scroll-top))]
                        (let [abs-line (+ scroll-top i)
                              line (buf/line-str b abs-line)
                              is-current (= abs-line (:row c))
                              marker (if is-current ">" " ")
                              truncated (if (and text-width (> (count line) text-width))
                                         (str (subs line 0 text-width) "\u2026")
                                         (or line ""))]
                          (str marker (format "%3d" (inc abs-line)) " " truncated))))]
      (.renderWidget frame
        (Paragraph/from lines-str)
        (Rect. 0 0 width content-height)))
    ;; Render status line
    (let [status-y (- height (if has-cmdline? 3 2))]
      (.renderWidget frame
        (Paragraph/from (str status-line
                            (when has-cmdline?
                              (str "\n" cmdline
                                   (when (not-empty msg) (str "  " msg))))))
        (Rect. 0 (max 0 status-y) width 2)))))

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
          (reify EventHandler
            (handle [this event runner]
              (if (instance? KeyEvent event)
                (let [^KeyEvent ke event]
                  ;; Quit on Ctrl+C (either via isCtrlC() or direct char code 3)
                  (if (or (.isCtrlC ke) (= (.character ke) \u0003))
                    (do (.quit runner) true)
                    (let [key-str (or (key-event->str ke) "")
                          ctrl-str (key-event->ctrl-str ke)
                          k (if (and ctrl-str (not= ctrl-str key-str)) ctrl-str key-str)]
                      (when (and k (not-empty k))
                        (let [result (dispatch/handle-key @editor-state k)]
                          (when (map? result)
                            (let [new-state (if (:editor result) (:editor result) result)]
                              (reset! editor-state new-state))
                            (when (false? (:running @editor-state true))
                              (.quit runner)))))
                      true)))
                false)))
          (reify Renderer
            (render [this frame]
              (try
                (render-editor-frame @editor-state frame)
                (catch Exception e
                  (.printStackTrace e)))))))
      (catch Exception e
        (println "Error running editor:" (.getMessage e))
        (.printStackTrace e)))))
