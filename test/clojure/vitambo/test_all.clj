(ns vitambo.test-all
  "Integration tests for the vitambo vim clone."
  (:require [vitambo.buffer :as buf]
            [vitambo.editor :as ed]
            [vitambo.mode :as mode]
            [vitambo.dispatch :as dispatch]
            [vitambo.motion :as motion]
            [vitambo.operator :as op]
            [vitambo.text-object :as to]
            [vitambo.search :as search]
            [vitambo.command :as cmd]
            [clojure.string :as str]))

(defn- test [name expected actual]
  (if (= expected actual)
    (println (str "  ✓ " name))
    (println (str "  ✗ " name ":\n      expected: " (pr-str expected) "\n      actual:   " (pr-str actual)))))

(println "=== Vitambo Vim Clone Tests ===\n")

(println "--- Buffer Tests ---")
(let [b (buf/make-buffer ["hello" "world"])]
  (test "line-count" 2 (buf/line-count b))
  (test "line-str" "hello" (buf/line-str b 0))
  (test "line-len" 5 (buf/line-len b 0))
  (let [b2 (buf/insert-char b 0 0 \X)]
    (test "insert-char" "Xhello" (buf/line-str b2 0)))
  (let [b3 (buf/delete-char b 0 0)]
    (test "delete-char" "ello" (buf/line-str b3 0)))
  (let [b4 (buf/split-line b 0 3)]
    (test "split-line" ["hel" "lo" "world"] (:lines b4)))
  (let [b5 (buf/insert-line b 1 "new")]
    (test "insert-line" ["hello" "new" "world"] (:lines b5)))
  (let [b6 (buf/delete-line b 0)]
    (test "delete-line" ["world"] (:lines b6)))
  (let [b7 (buf/make-buffer ["hello" "world"])
        [b8 c2] (buf/insert-at b7 0 5 "\nnew\nline")]
    (test "insert-at multiline" 4 (buf/line-count b8))
    (test "insert-at col" 4 c2)))

(println "\n--- Mode Tests ---")
(test "mode-names" "NORMAL" (get mode/mode-names mode/normal))
(test "mode-names insert" "INSERT" (get mode/mode-names mode/insert))
(test "mode-colors" :green (get mode/mode-colors mode/insert))

(println "\n--- Editor State Tests ---")
(let [e (ed/editor-state (buf/make-buffer ["a" "b"]))]
  (test "active-split" 0 (:active-split e))
  (test "splits count" 1 (count (:splits e)))
  (test "cursor initial" {:row 0 :col 0} (ed/cursor e))
  (let [e2 (ed/set-cursor e 1 0)]
    (test "set-cursor" {:row 1 :col 0} (ed/cursor e2))))

(println "\n--- Motion Tests ---")
(let [lines ["hello world foo bar" "second line" "third" "a b c d e"]]
  (test "h" {:row 1 :col 3} (motion/move-h lines 1 5 2))
  (test "l" {:row 1 :col 8} (motion/move-l lines 1 5 3))
  (test "j" {:row 2 :col 0} (motion/move-j lines 0 0 2))
  (test "k" {:row 1 :col 3} (motion/move-k lines 2 3 1))
  (test "w" {:row 0 :col 6} (motion/move-w lines 0 0 1))
  (test "b" {:row 0 :col 6} (motion/move-b lines 0 12 1))
  (test "0" {:row 1 :col 0} (motion/move-0 lines 1 5 nil))
  (test "$" {:row 1 :col 11} (motion/move-$ lines 1 5 nil))
  (test "gg" {:row 0 :col 0} (motion/move-gg lines 0 0 1))
  (test "G" {:row 4 :col 0} (motion/move-G lines 0 0 nil)))

(println "\n--- Operator Tests ---")
(let [b (buf/make-buffer ["hello" "world"])]
  (let [[b2 yanked [r c]] (op/op-delete b [0 0] [0 0] :line nil)]
    (test "delete-line yank" "hello" yanked)
    (test "delete-line result" ["world"] (:lines b2)))
  (let [y (op/op-yank b [0 0] [0 5] :char)]
    (test "yank char" "hello" y))
  (let [[b2 yanked [r c] ins?] (op/op-change b [0 0] [0 5] :char)]
    (test "change" "" (buf/line-str b2 0))
    (test "change insert" true ins?))
  (let [[b2 yanked [r c]] (op/op-delete-char b 0 0 3)]
    (test "delete-char (x)" "hel" (buf/line-str b2 0))
    (test "delete-char yank" "lo" yanked)))

(println "\n--- Text Object Tests ---")
(let [b (buf/make-buffer ["hello (world) foo"])]
  (test "i(" [0 6 0 13] (to/text-object-range b 0 10 (keyword "i(")))
  (let [b2 (buf/make-buffer ["hello \"world\" foo"])]
    (test "i\"" [0 7 0 14] (to/text-object-range b2 0 8 (keyword "i\"")))))

(println "\n--- Search Tests ---")
(let [b (buf/make-buffer ["hello world" "foo hello"])]
  (test "search forward" {:row 0 :col 0} (search/search-forward b 0 0 "hello"))
  (test "search forward next" {:row 1 :col 4} (search/search-forward b 0 1 "hello"))
  (test "search backward" {:row 0 :col 0} (search/search-backward b 1 0 "hello")))

(println "\n--- Command Tests ---")
(let [b (buf/make-buffer ["line1" "line2"])
      e (ed/editor-state b)]
  (test ":q" false (:running (:editor (cmd/execute-command e "q"))))
  (test ":1" {:row 0 :col 0} (get-in (:editor (cmd/execute-command e "1")) [:splits 0 :cursor]))
  (test ":2" {:row 1 :col 0} (get-in (:editor (cmd/execute-command e "2")) [:splits 0 :cursor]))
  (test ":unknown" "Unknown command: foo" (:message (cmd/execute-command e "foo"))))

(println "\n--- Dispatch Tests ---")
;; Test key dispatch in normal mode
(let [b (buf/make-buffer ["hello world" "line 2"])
      e (ed/editor-state b)]
  ;; hjkl
  (let [e1 (:editor (dispatch/handle-key e "j"))]
    (test "j dispatch" {:row 1 :col 0} (get-in e1 [:splits 0 :cursor])))
  ;; insert mode entry
  (let [e2 (:editor (dispatch/handle-key e "i"))]
    (test "i dispatch" mode/insert (get-in e2 [:splits 0 :mode])))
  ;; visual mode
  (let [e3 (:editor (dispatch/handle-key e "v"))]
    (test "v dispatch" mode/visual-char (get-in e3 [:splits 0 :mode]))))

(println "\n=== All tests completed ===")
