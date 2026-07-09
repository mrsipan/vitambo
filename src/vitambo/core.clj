(ns vitambo.core
  "Main entry point for the vim clone."
  (:require [vitambo.editor :as ed]
            [vitambo.buffer :as buf]
            [vitambo.mode :as mode]
            [vitambo.dispatch :as dispatch]
            [vitambo.render :as render]
            [vitambo.motion :as motion]
            [vitambo.operator :as op]
            [vitambo.text-object :as to]
            [vitambo.search :as search]
            [vitambo.command :as cmd]))

(defn -main
  "Main entry point. Opens files given as arguments or starts with an empty buffer."
  [& args]
  (let [editor (if (seq args)
                 (let [bufs (mapv (fn [f]
                                   (try (buf/load-file! f)
                                        (catch Exception _ (buf/make-buffer))))
                                 args)]
                   (ed/editor-state (first bufs)))
                 (ed/editor-state (buf/make-buffer ["~ Vim Clone - TamboUI ~" ""])))]
    (render/start-editor! editor)))

;; When run directly
(when *command-line-args*
  (apply -main *command-line-args*))
