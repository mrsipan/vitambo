# Vitambo — A Vim Clone in Clojure

A terminal-based Vim editor clone built with **Clojure** and the **TamboUI** Java terminal framework.

> **Status:** Experimental / Work in Progress. Core editing features work; the TUI renderer is ready for interactive testing.

## Features

### Modes

| Mode | Trigger | Cursor Color |
|------|---------|-------------|
| **Normal** | `Esc` | White block |
| **Insert** | `i`, `a`, `I`, `A`, `o`, `O` | Green bar |
| **Visual** (char) | `v` | Magenta block |
| **Visual** (line) | `V` | Magenta block |
| **Visual** (block) | `Ctrl-V` | Magenta block |
| **Command** | `:` | Yellow prompt |
| **Operator-Pending** | `d`, `c`, `y`, `>`, `<` | Yellow block |

### Operators

| Key | Action | Example |
|-----|--------|---------|
| `d{motion}` | Delete | `dw` — delete word |
| `c{motion}` | Change (delete + insert) | `ciw` — change inner word |
| `y{motion}` | Yank (copy) | `y$` — yank to end of line |
| `D` | Delete to end of line | `D` |
| `C` | Change to end of line | `C` |
| `Y` | Yank to end of line | `Y` |
| `dd` / `cc` / `yy` | Linewise delete / change / yank | `dd` — delete line |
| `p` / `P` | Put after / before cursor | `p` — paste |
| `>` / `<` | Indent / outdent | `>j` — indent next line |
| `~` | Toggle case of character | `~` |
| `x` | Delete character | `3x` — delete 3 chars |

### Motions

| Key | Motion |
|-----|--------|
| `h` `j` `k` `l` | Left, Down, Up, Right |
| `w` `b` `e` | Word forward, back, end |
| `W` `B` `E` | WORD forward, back, end |
| `ge` `gE` | Backward to end of previous word / WORD |
| `0` `$` `^` | Line start, end, first non-blank |
| `gg` `G` | File start, end |
| `{` `}` | Paragraph back / forward |
| `%` | Jump to matching bracket |
| `f{char}` / `F{char}` | Find character forward / backward |
| `t{char}` / `T{char}` | Till character (stop before) |
| `;` / `,` | Repeat last `f`/`F`/`t`/`T` |
| `Ctrl-D` / `Ctrl-U` | Scroll half-page down / up |
| `zz` / `zt` / `zb` | Center / top / bottom current line |

### Text Objects

| Key | Object |
|-----|--------|
| `iw` / `aw` | Inner / A word |
| `iW` / `aW` | Inner / A WORD |
| `i"` / `a"` | Inner / A double-quoted string |
| `i'` / `a'` | Inner / A single-quoted string |
| `i(` / `a(` | Inner / A parentheses |
| `i[` / `a[` | Inner / A brackets |
| `i{` / `a{` | Inner / A braces |
| `i<` / `a<` | Inner / A angle brackets |

### Counts

Prefix any operator or motion with a number: `3dw`, `5j`, `2dd`.

### Visual Mode

| Key | Selection |
|-----|-----------|
| `v` | Character-wise |
| `V` | Line-wise |
| `Ctrl-V` | Block (rectangular) |

In visual mode: `d`/`x` (delete), `c` (change), `y` (yank), `>`/`<` (indent/outdent), `~` (toggle case).

### Search

| Key | Action |
|-----|--------|
| `/pattern` | Search forward (incremental, regex) |
| `?pattern` | Search backward |
| `n` / `N` | Next / previous match |
| `*` / `#` | Search word under cursor |
| `:nohl` | Clear search highlighting |

### Split Views

| Key | Action |
|-----|--------|
| `Ctrl-W v` | Vertical split |
| `Ctrl-W s` | Horizontal split |
| `Ctrl-W h/j/k/l` | Navigate splits |
| `Ctrl-W w` | Next split |
| `Ctrl-W q` | Close split |
| `:split [file]` | Horizontal split |
| `:vsplit [file]` | Vertical split |

### Other

- **Undo** (`u`) and **Redo** (Ctrl-R) — linear undo stack per split
- **Yank registers** (`""`, `"0`–`"9`) — text stored automatically
- **Key mappings** (`:map <from> <to>`)
- **File I/O** (`:w`, `:wq`, `:e <file>`)

## Requirements

- **Java 17+** (Java 26 works)
- **Clojure CLI tools** (`clj` / `clojure`)

## Quick Start

```bash
# Clone the repo
git clone https://github.com/mrsipan/vitambo.git
cd vitambo

# Run with an empty buffer
clojure -M -m vitambo.core

# Or open a file
clojure -M -m vitambo.core test_vim.txt
```

## Project Structure

```
src/vitambo/
├── buffer.clj       — Text buffer operations (insert, delete, split, join, file I/O)
├── mode.clj         — Mode definitions and display metadata
├── editor.clj       — Editor state (splits, cursors, registers, undo/redo)
├── motion.clj       — Cursor motions (hjkl, wbe, gg/G, f/t, %, etc.)
├── operator.clj     — Operators (delete, change, yank, put, indent/outdent, ~)
├── text_object.clj  — Text objects (iw, iW, i"/a", i(/a(, etc.)
├── search.clj       — Regex search (forward, backward, word under cursor)
├── command.clj      — Command-line mode (:w, :q, :e, :split, :map, etc.)
├── dispatch.clj     — Key event dispatcher (routes keys by mode)
├── render.clj       — TamboUI renderer (TuiRunner, frame drawing, key handling)
└── core.clj         — Entry point (-main)
```

## Development

The project uses **REPL-oriented development** with the nREPL server:

```bash
# Start nREPL (port 7888)
clojure -M:nrepl

# In another terminal, connect with your editor
# Then evaluate code interactively
```

### Running Tests

```bash
clojure -M -e '(load-file "test/clojure/vitambo/test_all.clj")'
```

### Linting

```bash
clj-kondo --lint src/
```

## Built With

- [TamboUI](https://github.com/tamboui/tamboui) — Java terminal UI framework (v0.4.0)
- [Clojure](https://clojure.org/) — 1.12.0
- [nREPL](https://nrepl.org/) — 1.3.0

## License

MIT
