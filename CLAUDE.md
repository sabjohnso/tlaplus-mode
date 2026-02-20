# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

An Emacs major mode for editing TLA+ specification files (`.tla`). Single-file package: `tlaplus-mode.el` (version 0.2.0).

## Language

Emacs Lisp with `lexical-binding: t`. Read `~/.claude/lang/` for Emacs Lisp guidance if available.

## Development

### Build & Test

```sh
make compile   # byte-compile tlaplus-mode.el (must be warning-free)
make check     # byte-compile + run all ERT tests in batch mode
make clean     # remove .elc files
```

Run a single test by name pattern:

```sh
emacs -Q --batch -L . -L test -l test/test-helper.el -l test/tlaplus-mode-test.el \
  -eval '(ert-run-tests-batch-and-exit "tlaplus-test-indent")'
```

### Manual testing

```elisp
;; Load in a running Emacs
(load-file "tlaplus-mode.el")
;; Open any .tla file — mode activates via auto-mode-alist
```

## Architecture

Everything lives in `tlaplus-mode.el`, organized top-down:

1. **Customization group** (`tlaplus`): `tlaplus-indent-offset`, `tlaplus-prettify-symbols`
2. **Syntax table**: Handles `(* block *)` comments, `\"` strings, brackets, operator punctuation. Backslash is punctuation (not escape) since TLA+ uses `\in`, `\cup`, etc.
3. **Syntax propertize** (`tlaplus-syntax-propertize`): Marks `\*` as line-comment starters via text properties (style-b comments). Skips matches inside strings/block-comments.
4. **Font lock**: Keywords, constants, built-in types, stdlib names, backslash operators (`\in`, `\cup`), temporal operators (`[]`, `<>`), operator definitions (`Name ==`), primed variables (`var'`).
5. **Prettify symbols**: Maps ASCII TLA+ operators to Unicode glyphs (`/\` → ∧, `\in` → ∈, `[]` → □). Custom compose predicate prevents partial matches (e.g., `\in` inside `\intersect`).
6. **Indentation** (`tlaplus-indent-line`): Rule-based via `tlaplus--calculate-indentation`. Handles module delimiters, comments, closing brackets, conjunct/disjunct alignment (`/\`, `\/`), LET/IN blocks, bracket nesting, and definition bodies (`==`).
7. **Imenu**: Indexes operators, constants, variables, theorems, assumptions.
8. **Mode definition**: `define-derived-mode` from `prog-mode`. Sets comment syntax, font-lock, indentation, prettify-symbols, imenu.

## Key Design Decisions

- TLA+ has two comment styles: block `(* ... *)` and line `\*`. The line comment uses syntax-propertize because `\*` conflicts with the block comment `(* ... *)` syntax entries.
- Backslash is syntax class `.` (punctuation), not `\\` (escape), because TLA+ operators like `\in` are not escape sequences.
- The prettify-symbols compose predicate (`tlaplus-prettify-compose-p`) uses boundary checking to avoid composing partial operator matches.
- Conjunct alignment searches backward for `/\` or `\/` lines, stopping at definition boundaries (`==`).
