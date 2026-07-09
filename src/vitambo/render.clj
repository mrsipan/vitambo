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

(def ^:private log-file "/tmp/vitambo-debug.log")

(defn- log
  "Write a debug message to the log file."
  [& msgs]
  (try
    (spit log-file (str (java.time.LocalDateTime/now) " " (str/join " " msgs) "\n") :append true)
    (catch Exception _)))

(defn- log-clear!
  "Clear the debug log."
  []
  (try (spit log-file "") (catch Exception _)))

(defn- key-event->str
  "Convert a KeyEvent to a string for the dispatcher."
  [^KeyEvent event]
  (let [ch (.character event)]
    (log "key-event->str: char=" (pr-str ch) " int=" (int (or ch 0))
         " hasCtrl=" (.hasCtrl event) " isCtrlC=" (.isCtrlC event)
         " code=" (.code event))
    (if (and ch (not= (int ch) 0))
      ;; Regular character
      (let [c (int ch)]
        (case c
          10 (do (log "key-event->str: ENTER") "\n")
          13 (do (log "key-event->str: CR") "\n")
          27 (do (log "key-event->str: ESC") "\u001b")
          8 (do (log "key-event->str: BS") "\u007f")
          127 (do (log "key-event->str: DEL") "\u007f")
          9 (do (log "key-event->str: TAB") "\t")
          (do (log "key-event->str: char " c " = " (str ch))
              (str ch))))
      ;; Non-character key - check key code (Java enum, convert to keyword)
      (if-let [code (.code event)]
        (let [kwd (keyword (str code))]
          (log "key-event->str: code=" code " kwd=" kwd)
          (case kwd
            :UP "k"
            :DOWN "j"
            :LEFT "h"
            :RIGHT "l"
            :ENTER "\n"
            :NEWLINE "\n"
            :TAB "\t"
            :ESCAPE "\u001b"
            :BACKSPACE "\u007f"
            :DELETE "\u007f"
            nil))
        (do (log "key-event->str: no character, no code") nil)))))

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
        m (:mode split mode/normal)
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
    (log "render: mode=" m " cursor=" c " lines=" total-lines)
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
    (log-clear!)
    (log "=== Editor started ===")
    (try
      (with-open [tui (TuiRunner/create config)]
        ;; Show cursor at startup
        (try (.showCursor (.backend tui)) (catch Exception _))
        (.run tui
          (reify EventHandler
            (handle [this event runner]
              ;; Keep cursor visible
              (try (.showCursor (.backend runner)) (catch Exception _))
              (log "handle called with event: " (type event))
              (if (instance? KeyEvent event)
                (let [^KeyEvent ke event]
                  ;; Quit on Ctrl+C (either via isCtrlC() or direct char code 3)
                  (if (or (.isCtrlC ke) (= (.character ke) \u0003))
                    (do (log "  -> QUIT") (.quit runner) true)
                    (let [k (key-event->str ke)]
                      (log "  key-string=" (pr-str k))
                      (when (and k (not-empty k))
                        (let [result (try (dispatch/handle-key @editor-state k)
                                         (catch Exception e
                                           (log "  DISPATCH ERROR: " (.getMessage e))
                                           {:editor @editor-state}))]
                          (when (map? result)
                            (if (:editor result)
                              (reset! editor-state (:editor result))
                              (reset! editor-state result))
                            (when (false? (:running @editor-state true))
                              (log "  -> QUIT (running=false)")
                              (.quit runner)))))
                      true)))
                (do (log "  NOT a KeyEvent") false))))
          (reify Renderer
            (render [this frame]
              (try
                (let [cursor (get-in @editor-state [:splits 0 :cursor] {:row 0 :col 0})
                      scroll-top (get-in @editor-state [:splits 0 :scroll-top] 0)
                      mode (get-in @editor-state [:splits 0 :mode] mode/normal)
                      vis-row (- (:row cursor) scroll-top)
                      vis-col (+ (:col cursor) 5)]
                  ;; Render content first
                  (render-editor-frame @editor-state frame)
                  ;; Show cursor (some terminals hide it)
                  (try (let [raw (.rawOutput frame)]
                         (.write raw (.getBytes "\u001b[?25h" "UTF-8"))
                         ;; Set cursor style based on mode  
                         (.write raw (.getBytes (case mode
                                                  mode/insert "\u001b[6 q"   ;; bar
                                                  "\u001b[2 q") "UTF-8"))   ;; block
                         (.flush raw))
                       (catch Exception _))
                  ;; Position cursor via Frame (handled by Terminal after render)
                  (.setCursorPosition frame vis-col vis-row))
                (catch Exception e
                  (log "RENDER ERROR: " (.getMessage e))
                  (.printStackTrace e)))))))
      (catch Exception e
        (log "STARTUP ERROR: " (.getMessage e))
        (println "Error running editor:" (.getMessage e))
        (.printStackTrace e)))))
