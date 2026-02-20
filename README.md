# tlaplus-mode

An Emacs major mode for editing [TLA+](https://lamport.azurewebsites.net/tla/tla.html) specification files (`.tla`).

## Features

- **Syntax highlighting** for keywords, operators, constants, standard library names, and both comment styles (`(* block *)` and `\* line`)
- **Indentation** with support for conjunct/disjunct alignment (`/\`, `\/`), LET/IN blocks, bracket nesting, and definition bodies
- **Prettify symbols** displaying Unicode glyphs for ASCII operators (`/\` → ∧, `\in` → ∈, `[]` → □, `<>` → ◇, `=>` → ⇒, and many more)
- **Imenu** navigation for operator definitions, constants, variables, theorems, and assumptions
- **Comment support** with `comment-dwim` for `\*` line comments

## Installation

### Manual

Clone this repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/tlaplus-mode")
(require 'tlaplus-mode)
```

### use-package (with a local clone)

```elisp
(use-package tlaplus-mode
  :load-path "/path/to/tlaplus-mode")
```

## Usage

Opening any `.tla` file activates `tlaplus-mode` automatically.

- **Toggle prettify symbols**: `M-x prettify-symbols-mode`
- **Navigate definitions**: `M-x imenu`

## Customization

| Variable                   | Default | Description                                  |
|----------------------------|---------|----------------------------------------------|
| `tlaplus-indent-offset`    | `4`     | Number of spaces per indentation level       |
| `tlaplus-prettify-symbols` | `t`     | Enable `prettify-symbols-mode` on activation |

```elisp
(setq tlaplus-indent-offset 2)
```

## Contributing

```sh
make test    # byte-compile + run ERT tests (must pass)
make lint    # run package-lint (requires package-lint from MELPA)
```

## License

GPL-3.0-or-later. See [COPYING](COPYING).
