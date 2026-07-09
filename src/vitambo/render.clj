(ns vitambo.render
  "TamboUI-based rendering for the vim clone."
  (:import (dev.tamboui.tui TuiRunner TuiConfig)
           (dev.tamboui.tui.event KeyEvent KeyCode)
           (dev.tamboui.tui EventHandler Renderer)
           (dev.tamboui.widgets.paragraph Paragraph)
           (dev.tamboui.terminal Frame))
  (:require [clojure.string :as str]
            [vitambo.editor :as ed]
            [vitambo.buffer :as buf]
            [vitambo.mode :as mode]
            [vitambo.dispatch :as dispatch]))

(defn- key-event->str
  "Convert a KeyEvent to a string for the dispatcher."
  [^KeyEvent event]
  (cond
    (.isQuit event) nil
    (.isCancel event) "\u001b"
    (.isDeleteBackward event) "\u007f"
    (.isSelect event) "\n"
    :else
    (let [ch (.character event)]
      (if (and ch (not= (int ch) 0))
        (str ch)
        (let [code (.code event)]
          (when code
            (case code
              (:UP) "k"
              (:DOWN) "j"
              (:LEFT) "h"
              (:RIGHT) "l"
              (:ENTER :NEWLINE) "\n"
              (:TAB) "\t"
              nil)))))))

(defn- key-event->ctrl-str
  "Get Ctrl+letter code from a KeyEvent."
  [^KeyEvent event]
  (when (.hasCtrl event)
    (let [ch (.character event)]
      (when (and ch (not= (int ch) 0))
        (let [c (int ch)]
          (when (and (>= c 1) (<= c 26))
            (str (char c))))))))

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
        (.withY (.withX area 0) 0)))
    ;; Render status line
    (let [status-y (- height (if has-cmdline? 3 2))]
      (.renderWidget frame
        (Paragraph/from (str status-line
                            (when has-cmdline?
                              (str "\n" cmdline
                                   (when (not-empty msg) (str "  " msg))))))
        (.withY (.withX area 0) (max 0 status-y))))))

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
                  (if (.isQuit ke)
                    (do (.quit runner) false)
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
                      false)))
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
