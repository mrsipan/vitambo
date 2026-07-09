(ns vitambo.command
  "Command-line mode for vim clone (: commands)."
  (:require [clojure.string :as str]
            [vitambo.buffer :as buf]
            [vitambo.search :as search]))

(defn active-split
  [editor]
  (get-in editor [:splits (:active-split editor 0)]))

(defn update-active-split
  [editor f & args]
  (let [idx (:active-split editor 0)]
    (apply update-in editor [:splits idx] f args)))

(defn execute-command
  "Execute a command string. Returns {:editor updated-editor :message msg}."
  [editor cmd]
  (let [trimmed (str/trim cmd)]
    (cond
      ;; :q - quit
      (re-matches #"q(uit)?!?" trimmed)
      (assoc editor :running false :message "Quit")

      ;; :w - write
      (re-matches #"w(rite)?!?" trimmed)
      (let [split (get-in editor [:splits (:active-split editor 0)])
            buf (:buffer split)
            new-buf (buf/save-file! buf)]
        {:editor (assoc-in editor [:splits (:active-split editor 0) :buffer] new-buf)
         :message "Written"})

      ;; :wq - write and quit
      (re-matches #"wq!?" trimmed)
      (let [split (get-in editor [:splits (:active-split editor 0)])
            buf (:buffer split)
            new-buf (buf/save-file! buf)]
        {:editor (-> editor
                    (assoc-in [:splits (:active-split editor 0) :buffer] new-buf)
                    (assoc :running false))
         :message "Written and quit"})

      ;; :w <filename>
      (re-matches #"w[!]?\s+.+" trimmed)
      (let [filename (str/trim (subs cmd (if (= (second cmd) \!) 2 1)))
            split (get-in editor [:splits (:active-split editor 0)])
            buf (:buffer split)
            new-buf (buf/save-as! buf filename)]
        {:editor (assoc-in editor [:splits (:active-split editor 0) :buffer] new-buf)
         :message (str "Written to " filename)})

      ;; :e <filename> - edit file
      (re-matches #"e[dit]?\s+.+" trimmed)
      (let [filename (str/trim (subs cmd 2))]
        (try
          (let [new-buf (buf/load-file! filename)]
            {:editor (assoc-in editor [:splits (:active-split editor 0) :buffer] new-buf)
             :message (str "Opened " filename)})
          (catch Exception e
            {:editor editor :message (str "Cannot open " filename)})))

      ;; :split <file>
      (re-matches #"split?\s*(.*)" trimmed)
      (let [filename (str/trim (or (re-find #"split?\s+(.+)" trimmed) ""))
            new-buf (if (not-empty filename)
                     (try (buf/load-file! filename) (catch Exception _ (buf/make-buffer)))
                     (buf/make-buffer))
            new-split (-> (assoc (get-in editor [:splits 0]) :buffer new-buf)
                         (assoc-in [:cursor] {:row 0 :col 0}))]
        {:editor (-> editor
                    (update :splits conj new-split)
                    (assoc :active-split (count (:splits editor))))
         :message "Split created"})

      ;; :vsplit <file>
      (re-matches #"vsplit?\s*(.*)" trimmed)
      (let [filename (str/trim (or (re-find #"vsplit?\s+(.+)" trimmed) ""))
            new-buf (if (not-empty filename)
                     (try (buf/load-file! filename) (catch Exception _ (buf/make-buffer)))
                     (buf/make-buffer))
            new-split (-> (assoc (get-in editor [:splits 0]) :buffer new-buf)
                         (assoc-in [:cursor] {:row 0 :col 0}))]
        {:editor (-> editor
                    (update :splits conj new-split)
                    (assoc :active-split (count (:splits editor))))
         :message "Vertical split created"})

      ;; :close / :q / :quit
      (re-matches #"(close|q|quit)!?" trimmed)
      (let [n (count (:splits editor))]
        (if (<= n 1)
          (assoc editor :running false :message "Quit")
          (let [idx (:active-split editor 0)]
            {:editor (-> editor
                        (update :splits #(vec (concat (take idx %) (drop (inc idx) %))))
                        (assoc :active-split (max 0 (dec idx))))
             :message "Closed split"})))

      ;; :map
      (re-matches #"map\s+(.+)" trimmed)
      (let [rest (str/trim (subs cmd 4))
            parts (str/split rest #"\s+" 2)]
        (if (= (count parts) 2)
          (let [from-seq (first parts)
                to-seq (second parts)]
            {:editor (update-active-split editor
                      (fn [s] (assoc-in s [:mappings from-seq] to-seq)))
             :message (str "Mapped " from-seq " -> " to-seq)})
          {:editor editor :message "Usage: :map <from> <to>"}))

      ;; :nohl
      (re-matches #"nohl(?:search)?" trimmed)
      {:editor (update-active-split editor assoc :search-pattern nil)
       :message nil}

      ;; :set hlsearch
      (re-matches #"set\s+hlsearch" trimmed)
      {:editor (update-active-split editor assoc :hlsearch true)
       :message "Search highlighting on"}

      ;; :set nohlsearch
      (re-matches #"set\s+nohlsearch" trimmed)
      {:editor (update-active-split editor assoc :hlsearch false)
       :message "Search highlighting off"}

      ;; Number - go to line
      (re-matches #"\d+" trimmed)
      (let [n (Integer/parseInt trimmed)]
        {:editor (update-active-split editor
                   (fn [s]
                     (let [b (:buffer s)
                           max-line (max 0 (dec (buf/line-count b)))
                           target (min (dec n) max-line)]
                       (assoc s :cursor {:row target :col 0}))))
         :message nil})

      ;; :<range>d - delete lines
      (re-matches #"\d+,\s*\d+d" trimmed)
      (let [[_ s e] (re-find #"(\d+),\s*(\d+)d" trimmed)
            r1 (Integer/parseInt s)
            r2 (Integer/parseInt e)
            split (active-split editor)
            b (:buffer split)
            yanked (str/join "\n" (subvec (:lines b) (dec r1) r2))
            nl (vec (concat (take (dec r1) (:lines b)) (drop r2 (:lines b))))]
        (if (empty? nl)
          {:editor (-> editor
                      (assoc-in [:splits (:active-split editor 0) :buffer] (buf/make-buffer [""]))
                      (assoc-in [:splits (:active-split editor 0) :registers "\""] yanked))
           :message (str "Deleted " (inc (- r2 r1)) " lines")}
          {:editor (-> editor
                      (assoc-in [:splits (:active-split editor 0) :buffer]
                        (assoc b :lines nl :modified true))
                      (assoc-in [:splits (:active-split editor 0) :registers "\""] yanked)
                      (assoc-in [:splits (:active-split editor 0) :cursor] {:row (dec r1) :col 0}))
           :message (str "Deleted " (inc (- r2 r1)) " lines")}))

      :else
      {:editor editor :message (str "Unknown command: " cmd)})))
