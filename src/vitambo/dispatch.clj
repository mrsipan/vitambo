(ns vitambo.dispatch
  "Main key event dispatcher for the vim clone."
  (:require [vitambo.buffer :as buf]
            [vitambo.mode :as mode]
            [vitambo.motion :as motion]
            [vitambo.operator :as op]
            [vitambo.text-object :as to]
            [vitambo.search :as search]
            [vitambo.command :as cmd]))

(defn active-split [editor]
  (get-in editor [:splits (:active-split editor 0)]))

(defn update-active-split [editor f & args]
  (let [idx (:active-split editor 0)]
    (apply update-in editor [:splits idx] f args)))

(defn update-buffer [editor f & args]
  (let [idx (:active-split editor 0)]
    (apply update-in editor [:splits idx :buffer] f args)))

(defn- reset-op-state [s]
  (assoc s :operator nil :operator-pending nil :count 0
         :key-acc "" :text-object-prefix nil :last-op-key nil))

(defn- set-mode [editor m]
  (update-active-split editor assoc :mode m))

(defn- buf-of [editor]
  (:buffer (active-split editor)))

(defn- cursor-of [editor]
  (:cursor (active-split editor)))

(defn- lines-of [editor]
  (:lines (buf-of editor)))

(defn- row-col [editor]
  (let [c (cursor-of editor)]
    [(:row c) (:col c)]))

(defn- split-count [editor]
  (let [c (:count (active-split editor))]
    (if (and c (pos? c)) c 1)))

;; ── Normal mode ───────────────────────────────────────────────────

(defn- handle-count [editor key-str]
  (update-active-split editor
    (fn [s]
      (let [d (Character/digit (int (first key-str)) 10)]
        (if (and d (>= d 0))
          (update s :count #(+ (* (or % 0) 10) d))
          s)))))

(defn handle-key-normal [editor key-str]
  (let [[row col] (row-col editor) lines (lines-of editor)
        b (buf-of editor) split (active-split editor) cnt (split-count editor)]

    (cond
      ;; Count digit
      (re-matches #"[0-9]" key-str)
      (if (and (= "0" key-str) (zero? (or (:count split) 0)))
        {:editor (update-active-split editor assoc :cursor (motion/move-0 lines row col nil))}
        {:editor (handle-count editor key-str)})

      ;; ── Operator keys (d, c, y, >, <) ──
      (#{"d" "c" "y" ">" "<"} key-str)
      (let [op-kw ({"d" :delete "c" :change "y" :yank ">" :indent "<" :outdent} key-str)]
        (if (= (:last-op-key split) key-str)
          ;; Double-tap: linewise
          (let [line-len (count (nth lines row ""))
                line-range [row 0 row line-len]]
            (case op-kw
              :delete
              (let [[nb yk [nr nc]] (op/op-delete b [row 0] [row line-len] :line nil)]
                {:editor (-> editor
                            (update-active-split assoc :buffer nb :cursor {:row (min nr (max 0 (dec (buf/line-count nb)))) :col nc})
                            (update-active-split assoc :last-op-key nil))
                 :mode mode/normal})
              :change
              (let [[nb yk [nr nc] ins?] (op/op-change b [row 0] [row line-len] :line)]
                {:editor (-> editor
                            (update-active-split assoc :buffer nb :cursor {:row nr :col nc} :mode mode/insert)
                            (update-active-split assoc :last-op-key nil))
                 :mode mode/insert})
              :yank
              (let [yk (op/op-yank b [row 0] [row line-len] :line)]
                {:editor (update-active-split editor
                           (fn [s] (-> s (assoc-in [:registers "\""] yk) (assoc :last-op-key nil))))
                 :mode mode/normal})
              (:indent :outdent)
              (let [indent-fn (if (= op-kw :indent) op/op-indent op/op-outdent)
                    [nb [nr nc]] (indent-fn b [row 0] [row line-len] :line)]
                {:editor (update-active-split editor
                           (fn [s] (-> s (assoc :buffer nb) (assoc :last-op-key nil))))
                 :mode mode/normal})))
          ;; First tap: enter operator-pending
          {:editor (update-active-split editor
                     (fn [s] (assoc s :operator op-kw :operator-pending true :last-op-key key-str)))
           :mode mode/operator-pending}))

      ;; D (delete to end), C (change to end), Y (yank to end)
      (#{"D" "C" "Y"} key-str)
      (let [line-len (count (nth lines row ""))]
        (case key-str
          "D" (let [[nb yk [nr nc]] (op/op-delete b [row col] [row line-len] :char nil)]
                {:editor (update-active-split editor
                           (fn [s] (-> s (assoc :buffer nb :cursor {:row nr :col nc})
                                      (assoc-in [:registers "\""] yk))))
                 :mode mode/normal})
          "C" (let [[nb yk [nr nc] ins?] (op/op-change b [row col] [row line-len] :char)]
                {:editor (update-active-split editor
                           (fn [s] (-> s (assoc :buffer nb :cursor {:row nr :col nc} :mode mode/insert))))
                 :mode mode/insert})
          "Y" (let [yk (op/op-yank b [row col] [row line-len] :char)]
                {:editor (update-active-split editor
                           (fn [s] (assoc-in s [:registers "\""] yk)))
                 :mode mode/normal})))

      ;; x (delete char)
      (= "x" key-str)
      (let [[nb yk [nr nc]] (op/op-delete-char b row col cnt)]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc :buffer nb :cursor {:row nr :col nc}))))
         :mode mode/normal})

      ;; ~ (toggle case)
      (= "~" key-str)
      (let [[nb [nr nc]] (op/op-toggle-case b [row col] [row (inc col)] :char)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :buffer nb :cursor (or [nr nc] {:row row :col col}))))})

      ;; p / P (put)
      (#{"p" "P"} key-str)
      (let [yk (or (get-in split [:registers "\""]) (get-in split [:registers "0"]))]
        (if yk
          (if (= "p" key-str)
            (let [[nb [nr nc]] (op/op-put-after b row (inc col) yk)]
              {:editor (update-active-split editor
                         (fn [s] (assoc s :buffer nb :cursor {:row nr :col nc})))})
            (let [[nb [nr nc]] (op/op-put-before b row col yk)]
              {:editor (update-active-split editor
                         (fn [s] (assoc s :buffer nb :cursor {:row nr :col nc})))}))
          {:editor editor}))

      ;; . (dot repeat)
      (= "." key-str)
      {:editor editor}  ;; Simplified

      ;; u (undo)
      (= "u" key-str)
      (let [us (:undo-stack split)]
        (if (seq us)
          (let [last-st (peek us) cl (:lines b)]
            {:editor (update-active-split editor
                       (fn [s] (-> s
                                  (assoc :buffer (assoc b :lines (:lines last-st)))
                                  (assoc :cursor (:cursor last-st))
                                  (update :undo-stack pop)
                                  (update :redo-stack conj {:lines cl :cursor (cursor-of editor)}))))})
          {:editor editor}))

      ;; Ctrl-R (redo)
      (= "\u0012" key-str)
      (let [rs (:redo-stack split)]
        (if (seq rs)
          (let [next-st (peek rs) cl (:lines b)]
            {:editor (update-active-split editor
                       (fn [s] (-> s
                                  (assoc :buffer (assoc b :lines (:lines next-st)))
                                  (assoc :cursor (:cursor next-st))
                                  (update :redo-stack pop)
                                  (update :undo-stack conj {:lines cl :cursor (cursor-of editor)}))))})
          {:editor editor}))

      ;; Ctrl-V (visual block)
      (= "\u0016" key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :mode mode/visual-block
                                :visual-start [row col] :visual-end [row col])))
       :mode mode/visual-block}

      ;; i, I, a, A, o, O (insert entry)
      (#{"i" "I" "a" "A" "o" "O"} key-str)
      (case key-str
        "i" {:editor (update-active-split editor
                     (fn [s] (assoc s :mode mode/insert :insert-text "" :count 0)))
             :mode mode/insert}
        "I" (let [fb (loop [i 0] (if (>= i (count (nth lines row ""))) 0
                                   (if (Character/isWhitespace (nth (nth lines row "") i))
                                     (recur (inc i)) i)))]
              {:editor (update-active-split editor
                         (fn [s] (assoc s :mode mode/insert :insert-text ""
                                        :cursor {:row row :col fb} :count 0)))
               :mode mode/insert})
        "a" {:editor (update-active-split editor
                     (fn [s] (assoc s :mode mode/insert :insert-text ""
                                    :cursor {:row row :col (min (inc col) (count (nth lines row "")))}
                                    :count 0)))
             :mode mode/insert}
        "A" {:editor (update-active-split editor
                     (fn [s] (assoc s :mode mode/insert :insert-text ""
                                    :cursor {:row row :col (count (nth lines row ""))}
                                    :count 0)))
             :mode mode/insert}
        "o" (let [nb (buf/insert-line b (inc row) "")]
              {:editor (update-active-split editor
                         (fn [s] (assoc s :buffer nb :mode mode/insert :insert-text ""
                                        :cursor {:row (inc row) :col 0} :count 0)))
               :mode mode/insert})
        "O" (let [nb (buf/insert-line b row "")]
              {:editor (update-active-split editor
                         (fn [s] (assoc s :buffer nb :mode mode/insert :insert-text ""
                                        :cursor {:row row :col 0} :count 0)))
               :mode mode/insert}))

      ;; v, V (visual mode entry)
      (= "v" key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :mode mode/visual-char
                                :visual-start [row col] :visual-end [row col])))
       :mode mode/visual-char}

      (= "V" key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :mode mode/visual-line
                                :visual-start [row 0] :visual-end [row (count (nth lines row ""))])))
       :mode mode/visual-line}

      ;; : (command), / (search), ? (search backward)
      (= ":" key-str)
      {:editor (assoc editor :cmdline ":" :cmdline-type :command :cmdline-cursor 1)
       :mode mode/command}

      (= "/" key-str)
      {:editor (assoc editor :cmdline "/" :cmdline-type :search-forward :cmdline-cursor 1)
       :mode mode/search}

      (= "?" key-str)
      {:editor (assoc editor :cmdline "?" :cmdline-type :search-backward :cmdline-cursor 1)
       :mode mode/search}

      ;; n, N (search next/prev)
      (= "n" key-str)
      (if-let [pat (:search-pattern split)]
        (if-let [m (search/search-forward b row col pat)]
          {:editor (update-active-split editor assoc :cursor m)}
          {:editor editor :message "Pattern not found"})
        {:editor editor})

      (= "N" key-str)
      (if-let [pat (:search-pattern split)]
        (if-let [m (search/search-backward b row col pat)]
          {:editor (update-active-split editor assoc :cursor m)}
          {:editor editor :message "Pattern not found"})
        {:editor editor})

      ;; *, # (search word)
      (= "*" key-str)
      (if-let [pat (search/search-word-under-cursor b row col)]
        (if-let [m (search/search-forward b row (inc col) pat)]
          {:editor (update-active-split editor
                     (fn [s] (assoc s :cursor m :search-pattern pat)))}
          {:editor editor :message "Pattern not found"})
        {:editor editor})

      (= "#" key-str)
      (if-let [pat (search/search-word-under-cursor b row col)]
        (if-let [m (search/search-backward b row col pat)]
          {:editor (update-active-split editor
                     (fn [s] (assoc s :cursor m :search-pattern pat)))}
          {:editor editor :message "Pattern not found"})
        {:editor editor})

      ;; ── Motions ──
      ;; h, j, k, l
      (#{"h" "j" "k" "l"} key-str)
      (let [mfn ({"h" motion/move-h "j" motion/move-j "k" motion/move-k "l" motion/move-l} key-str)
            new-pos (mfn lines row col cnt)]
        (if (= "j" key-str)
          (let [st (:scroll-top split 0) half (quot 20 2)
                ns (cond (< (:row new-pos) st) (:row new-pos)
                         (>= (:row new-pos) (+ st (* 2 half))) (- (:row new-pos) half)
                         :else st)]
            {:editor (update-active-split editor
                       (fn [s] (assoc s :cursor new-pos :scroll-top ns :count 0)))})
          {:editor (update-active-split editor
                     (fn [s] (assoc s :cursor new-pos :count 0)))}))

      ;; w, b, e, W, B, E
      (#{"w" "b" "e" "W" "B" "E"} key-str)
      (let [mfn ({"w" motion/move-w "b" motion/move-b "e" motion/move-e
                  "W" motion/move-W "B" motion/move-B "E" motion/move-E} key-str)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :cursor (mfn lines row col cnt) :count 0)))})

      ;; 0, $, ^
      (#{"0" "$" "^"} key-str)
      (let [mfn ({"0" motion/move-0 "$" motion/move-$ "^" motion/move-first} key-str)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :cursor (mfn lines row col cnt) :count 0)))})

      ;; gg, G
      (= "gg" key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :cursor (motion/move-gg lines row col cnt) :scroll-top 0 :count 0)))}

      (= "G" key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :cursor (motion/move-G lines row col nil) :count 0)))}

      ;; {, }
      (#{"{" "}"} key-str)
      (let [mfn (if (= "{") motion/move-prev-para motion/move-next-para)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :cursor (mfn lines row col cnt) :count 0)))})

      ;; % (matching bracket)
      (= "%" key-str)
      (if-let [mp (motion/find-matching-bracket (nth lines row "") col)]
        {:editor (update-active-split editor assoc :cursor {:row row :col mp})}
        {:editor editor})

      ;; f, F, t, T (char find)
      (#{"f" "F" "t" "T"} key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :operator :ft-char :operator-pending true :last-op-key key-str)))
       :mode mode/operator-pending}

      ;; ; , (repeat f/t)
      (#{";" ","} key-str)
      (if-let [lfc (:last-ft-char split)]
        (let [lfd (:last-ft-direction split)
              mfn (if (= key-str ";")
                    (if (= lfd :forward) motion/move-f motion/move-F)
                    (if (= lfd :forward) motion/move-F motion/move-f))]
          {:editor (update-active-split editor
                     (fn [s] (assoc s :cursor (mfn lines row col 1 lfc) :count 0)))})
        {:editor editor})

      ;; zz, zt, zb
      (= "zz" key-str)
      (let [half (quot 20 2)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :scroll-top (max 0 (- row half)) :count 0)))})

      (= "zt" key-str)
      {:editor (update-active-split editor assoc :scroll-top row :count 0)}

      (= "zb" key-str)
      (let [scr 20]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :scroll-top (max 0 (- row (dec scr))) :count 0)))})

      ;; Ctrl-D, Ctrl-U (scroll)
      (= "\u0004" key-str)
      (let [half (quot 20 2) nr (min (+ row half) (max 0 (dec (buf/line-count b))))
            st (:scroll-top split 0)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :cursor {:row nr :col col} :scroll-top (min (+ st half) nr))))})

      (= "\u0015" key-str)
      (let [half (quot 20 2) nr (max 0 (- row half))
            st (:scroll-top split 0)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :cursor {:row nr :col col} :scroll-top (max 0 (- st half)))))})

      ;; Ctrl-W (split commands)
      (= "\u0017" key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :operator :ctrl-w :operator-pending true :last-op-key "ctrl-w")))
       :mode mode/operator-pending}

      ;; ge, gE
      (#{"ge" "gE"} key-str)
      (let [mfn (if (= "ge") motion/move-ge motion/move-E)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :cursor (mfn lines row col cnt) :count 0)))})

      ;; g prefix
      (= "g" key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :operator :g-prefix :operator-pending true :last-op-key "g")))
       :mode mode/operator-pending}

      ;; Esc
      (= "\u001b" key-str)
      {:editor (update-active-split editor
                 (fn [s] (-> s (assoc :mode mode/normal :count 0 :key-acc ""
                                     :operator nil :operator-pending nil))))}

      ;; Unknown key: try as motion (for operator-pending fallback)
      :else
      {:editor editor :mode mode/normal})))

;; ── Insert mode ───────────────────────────────────────────────────

(defn handle-key-insert [editor key-str]
  (let [[row col] (row-col editor) b (buf-of editor)]
    (cond
      (= "\u001b" key-str)
      {:editor (update-active-split editor
                 (fn [s] (-> s (assoc :mode mode/normal :count 0)
                            (update :cursor #(let [l (count (nth (:lines b) (:row %) ""))]
                                              (assoc % :col (min (:col %) l)))))))}

      (= "\n" key-str)
      (let [nb (buf/split-line b row col)]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc :buffer nb :cursor {:row (inc row) :col 0}))))})

      (= "\u007f" key-str)
      (cond
        (> col 0)
        (let [nb (buf/delete-char b row (dec col))]
          {:editor (update-active-split editor
                     (fn [s] (-> s (assoc :buffer nb :cursor {:row row :col (dec col)}))))})
        (> row 0)
        (let [pl (count (buf/line-str b (dec row))) nb (buf/join-lines b (dec row))]
          {:editor (update-active-split editor
                     (fn [s] (-> s (assoc :buffer nb :cursor {:row (dec row) :col pl}))))})
        :else {:editor editor})

      (= "\u0017" key-str)  ;; Ctrl-W
      (if (> col 0)
        (let [line (buf/line-str b row)
              ws (loop [i (dec col)] (if (or (<= i 0) (Character/isWhitespace (nth line i))) i (recur (dec i))))
              nl (str (subs line 0 ws) (subs line col))
              nb (assoc b :lines (assoc (:lines b) row nl) :modified true)]
          {:editor (update-active-split editor
                     (fn [s] (-> s (assoc :buffer nb :cursor {:row row :col ws}))))})
        {:editor editor})

      :else
      (let [_ (try (spit "/tmp/vitambo-dispatch.log" (str "INSERT else: row=" row " col=" col " key=" key-str "\n") :append true) (catch Exception _))
            nb (buf/insert-char b row col (first key-str))]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc :buffer nb :cursor {:row row :col (inc col)})
                              (update :insert-text str (first key-str)))))}))))

;; ── Operator-pending mode ─────────────────────────────────────────

(defn- apply-op-on-region [editor op-kw r1 c1 r2 c2]
  (let [b (buf-of editor)]
    (case op-kw
      :delete
      (let [[nb yk [nr nc]] (op/op-delete b [r1 c1] [r2 c2] :char nil)]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc :buffer nb :cursor {:row nr :col nc})
                              (assoc-in [:registers "\""] yk) (reset-op-state))))
         :mode mode/normal})
      :change
      (let [[nb yk [nr nc] ins?] (op/op-change b [r1 c1] [r2 c2] :char)]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc :buffer nb :cursor {:row nr :col nc}
                                        :mode (if ins? mode/insert mode/normal))
                              (assoc-in [:registers "\""] yk) (reset-op-state))))
         :mode (if ins? mode/insert mode/normal)})
      :yank
      (let [yk (op/op-yank b [r1 c1] [r2 c2] :char)]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc-in [:registers "\""] yk) (reset-op-state))))
         :mode mode/normal})
      :indent
      (let [r1* (min r1 r2) r2* (max r1 r2)
            [nb [nr nc]] (op/op-indent b [r1* 0] [r2* 0] :line)]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc :buffer nb) (reset-op-state))))
         :mode mode/normal})
      :outdent
      (let [r1* (min r1 r2) r2* (max r1 r2)
            [nb [nr nc]] (op/op-outdent b [r1* 0] [r2* 0] :line)]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc :buffer nb) (reset-op-state))))
         :mode mode/normal})
      nil)))

(defn handle-key-operator [editor key-str]
  (let [split (active-split editor) op-kw (:operator split)
        [row col] (row-col editor) lines (lines-of editor) b (buf-of editor)
        cnt (split-count editor)]

    (if (= :ft-char op-kw)
      ;; f/F/t/T - next char is the target
      (let [ch (first key-str) lop (:last-op-key split "f")
            mfn ({"f" motion/move-f "F" motion/move-F
                  "t" motion/move-t "T" motion/move-T} lop motion/move-f)
            new-pos (mfn lines row col 1 ch)]
        {:editor (update-active-split editor
                   (fn [s] (-> s (assoc :cursor new-pos :last-ft-char ch
                                        :last-ft-direction (if (#{"f" "t"} lop) :forward :backward))
                              (reset-op-state))))})

      (if (= :ctrl-w op-kw)
        ;; Ctrl-W commands
        (case key-str
          "v" (let [n (count (:splits editor))
                    ns (assoc (active-split editor) :buffer (buf/make-buffer) :cursor {:row 0 :col 0})]
                {:editor (-> editor (assoc :splits (conj (:splits editor) ns)) (assoc :active-split n))})
          "s" (let [n (count (:splits editor))
                    ns (assoc (active-split editor) :buffer (buf/make-buffer) :cursor {:row 0 :col 0})]
                {:editor (-> editor (assoc :splits (conj (:splits editor) ns)) (assoc :active-split n))})
          "h" {:editor (assoc editor :active-split (max 0 (dec (:active-split editor 0))))}
          "j" (let [n (count (:splits editor))] {:editor (assoc editor :active-split (min (dec n) (inc (:active-split editor 0))))})
          "k" {:editor (assoc editor :active-split (max 0 (dec (:active-split editor 0))))}
          "l" (let [n (count (:splits editor))] {:editor (assoc editor :active-split (min (dec n) (inc (:active-split editor 0))))})
          "w" (let [n (count (:splits editor))] {:editor (assoc editor :active-split (mod (inc (:active-split editor 0)) n))})
          "q" (let [n (count (:splits editor))]
                (if (<= n 1) (assoc editor :running false)
                    (let [idx (:active-split editor 0)]
                      {:editor (-> editor (update :splits #(vec (concat (take idx %) (drop (inc idx) %))))
                                  (assoc :active-split (max 0 (dec idx))))})))
          ;; Unknown ctrl-w key - reset
          {:editor (update-active-split editor (fn [s] (reset-op-state s)))})

        (if (= :g-prefix op-kw)
          ;; g prefix commands
          (case key-str
            "R" (if-let [fn (:filename b)]
                  (try {:editor (update-active-split editor assoc :buffer (buf/load-file! fn))
                        :message "File reloaded"}
                       (catch Exception e {:editor editor :message (str "Cannot reload:" (.getMessage e))}))
                  {:editor editor :message "No file name"})
            "e" (let [np (motion/move-ge lines row col cnt)]
                  {:editor (update-active-split editor
                             (fn [s] (-> s (assoc :cursor np) (reset-op-state))))})
            "E" (let [np (motion/move-E lines row col cnt)]
                  {:editor (update-active-split editor
                             (fn [s] (-> s (assoc :cursor np) (reset-op-state))))})
            {:editor (update-active-split editor (fn [s] (reset-op-state s)))})

          ;; Regular operator: check motion or text object
          (let [motion-fn (get motion/motions (keyword key-str))]
            (if motion-fn
              ;; Motion
              (let [new-pos (motion-fn lines row col cnt)]
                (if (or (not= row (:row new-pos)) (not= col (:col new-pos)))
                  (apply-op-on-region editor op-kw row col (:row new-pos) (:col new-pos))
                  {:editor (update-active-split editor
                             (fn [s] (-> s (reset-op-state))))}))
              ;; Check for text object
              (let [to-prefix (:text-object-prefix split)]
                (if to-prefix
                  ;; Complete text object
                  (let [tok (keyword (str to-prefix key-str))]
                    (if-let [[r1 c1 r2 c2] (to/text-object-range b row col tok)]
                      (apply-op-on-region editor op-kw r1 c1 r2 c2)
                      {:editor (update-active-split editor
                                 (fn [s] (-> s (reset-op-state))))}))
                  ;; Check if this key starts a text object (i or a)
                  (if (#{"i" "a"} key-str)
                    {:editor (update-active-split editor
                               (fn [s] (assoc s :text-object-prefix key-str)))}
                    ;; Unknown key - reset
                    {:editor (update-active-split editor
                               (fn [s] (reset-op-state s)))}))))))))))

;; ── Visual mode ───────────────────────────────────────────────────

(defn handle-key-visual [editor key-str vtype]
  (let [[row col] (row-col editor) lines (lines-of editor) b (buf-of editor)
        split (active-split editor) vs (:visual-start split)]

    (cond
      (= "\u001b" key-str)
      {:editor (update-active-split editor
                 (fn [s] (assoc s :mode mode/normal :visual-start nil :visual-end nil)))}

      ;; d/x, c, y, >, <, ~ in visual mode
      (#{"d" "x"} key-str)
      (let [[r1 c1] vs [r2 c2] (or (:visual-end split) [row col])]
        (apply-op-on-region editor :delete r1 c1 r2 c2))

      (= "c" key-str)
      (let [[r1 c1] vs [r2 c2] (or (:visual-end split) [row col])]
        (apply-op-on-region editor :change r1 c1 r2 c2))

      (= "y" key-str)
      (let [[r1 c1] vs [r2 c2] (or (:visual-end split) [row col])]
        (apply-op-on-region editor :yank r1 c1 r2 c2))

      (= ">" key-str)
      (let [[r1 _] vs [r2 _] (or (:visual-end split) [row col])]
        (apply-op-on-region editor :indent r1 0 r2 0))

      (= "<" key-str)
      (let [[r1 _] vs [r2 _] (or (:visual-end split) [row col])]
        (apply-op-on-region editor :outdent r1 0 r2 0))

      (= "~" key-str)
      (let [[r1 c1] vs [r2 c2] (or (:visual-end split) [row col])]
        (loop [e editor r r1 c c1]
          (if (> r r2)
            {:editor (update-active-split e
                       (fn [s] (assoc s :mode mode/normal :visual-start nil :visual-end nil)))}
            (let [ln (nth (:lines b) r "")
                  mc (if (= r r2) (dec c2) (count ln))]
              (if (>= c mc)
                (recur e (inc r) 0)
                (let [ch (nth ln c) nch (if (Character/isUpperCase ch)
                                         (Character/toLowerCase ch)
                                         (Character/toUpperCase ch))
                      nb (update-buffer e (fn [buf] (assoc buf :lines (assoc (:lines buf) r
                                                                             (str (subs ln 0 c) nch (subs ln (inc c)))))))]
                  (if (>= (inc c) mc)
                    (recur nb (inc r) 0)
                    (recur nb r (inc c)))))))))

      ;; Motions in visual mode extend selection
      (#{"h" "j" "k" "l" "w" "b" "e" "W" "B" "E" "0" "$" "^" "gg" "G" "{" "}"} key-str)
      (let [mm {"h" motion/move-h "j" motion/move-j "k" motion/move-k "l" motion/move-l
                "w" motion/move-w "b" motion/move-b "e" motion/move-e
                "W" motion/move-W "B" motion/move-B "E" motion/move-E
                "0" motion/move-0 "$" motion/move-$ "^" motion/move-first
                "gg" motion/move-gg "G" motion/move-G
                "{" motion/move-prev-para "}" motion/move-next-para}
            mfn (get mm key-str motion/move-l) cnt (split-count editor)
            np (mfn lines row col cnt)]
        {:editor (update-active-split editor
                   (fn [s] (assoc s :cursor np :visual-end [(:row np) (:col np)] :count 0)))})

      ;; Mode switch within visual
      (= "v" key-str)
      {:editor (update-active-split editor (fn [s] (assoc s :mode mode/visual-char)))}

      (= "V" key-str)
      {:editor (update-active-split editor (fn [s] (assoc s :mode mode/visual-line)))}

      :else {:editor editor :mode vtype})))

;; ── Command-line mode ─────────────────────────────────────────────

(defn handle-key-cmdline [editor key-str cmdline-type]
  (let [cmd (:cmdline editor "") cp (:cmdline-cursor editor (count cmd))]
    (cond
      (= "\n" key-str)
      (let [cmd-str (subs cmd 1)]
        (case cmdline-type
          :command
          (let [res (cmd/execute-command editor cmd-str)]
            (if (map? res)
              (-> (or (:editor res) res)
                 (assoc :cmdline "" :cmdline-type nil :mode mode/normal
                        :message (:message res)))
              (assoc editor :cmdline "" :cmdline-type nil :mode mode/normal)))
          :search-forward
          (if (empty? cmd-str)
            (assoc editor :cmdline "" :cmdline-type nil :mode mode/normal)
            (if-let [m (search/search-forward (:buffer (active-split editor))
                         (:row (cursor-of editor)) (:col (cursor-of editor)) cmd-str)]
              (-> editor (update-active-split assoc :search-pattern cmd-str :cursor m)
                 (assoc :cmdline "" :cmdline-type nil :mode mode/normal))
              (-> editor (update-active-split assoc :search-pattern cmd-str)
                 (assoc :cmdline "" :cmdline-type nil :mode mode/normal :message "Pattern not found"))))
          :search-backward
          (if (empty? cmd-str)
            (assoc editor :cmdline "" :cmdline-type nil :mode mode/normal)
            (if-let [m (search/search-backward (:buffer (active-split editor))
                         (:row (cursor-of editor)) (:col (cursor-of editor)) cmd-str)]
              (-> editor (update-active-split assoc :search-pattern cmd-str :cursor m)
                 (assoc :cmdline "" :cmdline-type nil :mode mode/normal))
              (-> editor (update-active-split assoc :search-pattern cmd-str)
                 (assoc :cmdline "" :cmdline-type nil :mode mode/normal :message "Pattern not found"))))))

      (= "\u001b" key-str)
      (assoc editor :cmdline "" :cmdline-type nil :mode mode/normal)

      (= "\u007f" key-str)
      (if (> (count cmd) 1)
        (assoc editor :cmdline (str (subs cmd 0 (dec (count cmd))))
               :cmdline-cursor (dec cp))
        (assoc editor :cmdline "" :cmdline-type nil :mode mode/normal))

      :else
      (assoc editor :cmdline (str cmd key-str)
             :cmdline-cursor (inc (or cp (count cmd)))))))

;; ── Main dispatcher ───────────────────────────────────────────────

(defn handle-key [editor key-str]
  (let [split (active-split editor) cur-mode (:mode split mode/normal)]
    (try (spit "/tmp/vitambo-dispatch.log" (str "DISPATCH handle-key: key=" (pr-str key-str) " mode=" cur-mode "\n") :append true) (catch Exception _))
    (cond
      (= cur-mode mode/normal) (handle-key-normal editor key-str)
      (= cur-mode mode/insert) (handle-key-insert editor key-str)
      (= cur-mode mode/visual-char) (handle-key-visual editor key-str mode/visual-char)
      (= cur-mode mode/visual-line) (handle-key-visual editor key-str mode/visual-line)
      (= cur-mode mode/visual-block) (handle-key-visual editor key-str mode/visual-block)
      (= cur-mode mode/command) (handle-key-cmdline editor key-str (:cmdline-type editor))
      (= cur-mode mode/search) (handle-key-cmdline editor key-str (:cmdline-type editor))
      (= cur-mode mode/operator-pending) (handle-key-operator editor key-str)
      :else (handle-key-normal editor key-str))))
