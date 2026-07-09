# Vitambo Development Log

## 2026-07-09 — Initial Implementation

Built a full Vim clone in Clojure using the TamboUI terminal framework.

### Approach

REPL-oriented development: each namespace was built and tested incrementally via the nREPL server (port 7888), using the `eval_clojure` tool provided by the `pi-clojure-nrepl` extension.

### Architecture

**11 source files** across 2,177 lines of Clojure:

| Layer | Files | Purpose |
|-------|-------|---------|
| **Data** | `buffer.clj`, `mode.clj`, `editor.clj` | Core data structures and state management |
| **Editing** | `motion.clj`, `operator.clj`, `text_object.clj`, `search.clj`, `command.clj` | Vim editing primitives |
| **Dispatch** | `dispatch.clj` | Key event routing across all modes |
| **UI** | `render.clj` | TamboUI terminal rendering |
| **Entry** | `core.clj` | Application entry point |

### Challenges Encountered

1. **Clojure reader macros clash with Vim key names** — Function names like `move-^`, `move-{`, `move-}` conflict with Clojure's `^` (metadata) and `{`/`}` (map literal) reader macros. Solved by renaming to `move-first`, `move-prev-para`, `move-next-para`.

2. **`count` parameter shadowing `clojure.core/count`** — Several motion functions used `count` as a parameter name, which shadowed `clojure.core/count`. This worked in the nREPL due to incremental compilation but failed with `clojure -M` (fresh compilation). Fixed by renaming all parameters to `cnt`.

3. **Character map literals confusing the reader** — The bracket-matching pairs map `{\( \) ...}` used character literals like `\{` and `\}`, which contain brace characters. The reader would fail because `}` inside a form was interpreted as closing a map instead of being a character literal. Solved by switching to integer character codes: `{40 41 41 40 91 93 ...}`.

4. **Keyword literals containing special characters** — Keywords like `:i"` contain quotes which break the reader when written as `:i\"`. Solved by creating keywords dynamically with `(keyword "i\"")`.

5. **TamboUI API integration** — The Java interop for TamboUI's `TuiRunner`, `KeyEvent`, and rendering API required careful type hinting and reification of Java functional interfaces (`BiFunction`, `Consumer`).

### Key Decisions

- **Dispatch map for text objects** instead of a `case` form, to avoid keyword reader issues with quote-containing keywords.
- **Integer character codes** for bracket sets instead of character literals containing special characters.
- **`cnt` parameter naming** throughout motion functions to avoid shadowing `clojure.core/count`.

### Testing

- Unit tests in `test/clojure/vitambo/test_all.clj` covering buffer ops, motions, operators, text objects, search, commands, and dispatch.
- `clj-kondo` linting with zero errors on final build.
