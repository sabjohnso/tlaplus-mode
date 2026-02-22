;;; tlaplus-mode.el --- Major mode for TLA+ specifications -*- lexical-binding: t -*-

;; Author: Samuel B. Johnson
;; URL: https://github.com/sabjohnso/tlaplus-mode
;; Keywords: languages
;; Version: 0.2.2
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides syntax highlighting, mathematical symbol display, indentation,
;; comment support, and imenu navigation for TLA+ specification files (.tla).
;;
;; Handles both comment styles:
;;   (* block comments *)
;;   \* line comments
;;
;; Mathematical symbols:
;;   When `tlaplus-prettify-symbols' is non-nil (the default),
;;   `prettify-symbols-mode' displays Unicode glyphs for TLA+ operators:
;;   \in → ∈,  /\ → ∧,  \/ → ∨,  [] → □,  <> → ◇,  => → ⇒,  etc.
;;   Toggle with M-x prettify-symbols-mode.  The underlying text is unchanged.
;;
;; Usage:
;;   (load "/path/to/tlaplus-mode.el")
;;
;; Or add to load-path and:
;;   (require 'tlaplus-mode)

;;; Code:

(defgroup tlaplus nil
  "Major mode for editing TLA+ specifications."
  :group 'languages
  :prefix "tlaplus-")

(defcustom tlaplus-indent-offset 4
  "Number of spaces for each indentation level in TLA+ mode."
  :type 'integer
  :group 'tlaplus)

(defcustom tlaplus-prettify-symbols t
  "If non-nil, enable `prettify-symbols-mode' for mathematical display."
  :type 'boolean
  :group 'tlaplus)

;;; Syntax Table

(defvar tlaplus-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Block comments: (* ... *) — style a
    (modify-syntax-entry ?\( "()1"  table)
    (modify-syntax-entry ?\) ")(4"  table)
    (modify-syntax-entry ?*  ". 23" table)
    ;; Line comments: \* — style b (handled by syntax-propertize)
    ;; Newline ends style-b comments
    (modify-syntax-entry ?\n "> b" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Backslash is punctuation in TLA+ (not escape)
    (modify-syntax-entry ?\\ "." table)
    ;; Underscore is a word constituent (for identifiers like Type_OK)
    (modify-syntax-entry ?_ "w" table)
    ;; Operator punctuation
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?~  "." table)
    (modify-syntax-entry ?#  "." table)
    (modify-syntax-entry ?@  "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?:  "." table)
    ;; Matched brackets
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table)
  "Syntax table for `tlaplus-mode'.")

;;; Syntax Propertize — \* line comments

(defun tlaplus-syntax-propertize (start end)
  "Mark \\=\\* sequences as style-b comment starters between START and END.
Skips matches that are already inside a string or block comment."
  (goto-char start)
  (while (re-search-forward "\\(\\\\\\)\\(\\*\\)" end t)
    (let ((ppss (save-excursion (syntax-ppss (match-beginning 0)))))
      (unless (or (nth 3 ppss) (nth 4 ppss))
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax ". 1b"))
        (put-text-property (match-beginning 2) (match-end 2)
                           'syntax-table (string-to-syntax ". 2b"))))))

;;; Font Lock

(defconst tlaplus--keywords
  '("MODULE" "EXTENDS" "INSTANCE" "WITH" "LOCAL"
    "CONSTANT" "CONSTANTS" "VARIABLE" "VARIABLES"
    "ASSUME" "ASSUMPTION" "AXIOM"
    "THEOREM" "LEMMA" "PROPOSITION" "COROLLARY"
    "PROOF" "BY" "OBVIOUS" "OMITTED" "QED"
    "HAVE" "TAKE" "PICK" "WITNESS" "SUFFICES"
    "DEFINE" "USE" "HIDE"
    "NEW" "STATE" "ACTION" "TEMPORAL"
    "LET" "IN" "IF" "THEN" "ELSE" "CASE" "OTHER"
    "CHOOSE" "EXCEPT" "DOMAIN"
    "SUBSET" "UNION" "ENABLED" "UNCHANGED"
    "RECURSIVE")
  "TLA+ reserved keywords.")

(defconst tlaplus--constants
  '("TRUE" "FALSE")
  "TLA+ boolean constants.")

(defconst tlaplus--builtin-types
  '("BOOLEAN" "STRING")
  "TLA+ built-in type names.")

(defconst tlaplus--stdlib-names
  '("Nat" "Int" "Real"
    "Seq" "Len" "Append" "Head" "Tail" "SubSeq" "SelectSeq"
    "Cardinality" "IsFiniteSet"
    "Print" "PrintT" "Assert" "ToString"
    "Permutations" "SortSeq"
    "BagToSet" "SetToBag" "BagIn" "EmptyBag" "BagCardinality" "CopiesIn"
    "IsABag" "SubBag" "BagOfAll")
  "Names from TLA+ standard modules.")

(defconst tlaplus--backslash-operators
  '("in" "notin"
    "subseteq" "subset" "supseteq" "supset"
    "cup" "cap" "bigcup" "bigcap"
    "union" "intersect"
    "times" "o" "circ" "oplus" "ominus" "otimes" "oslash" "odot"
    "div" "mod" "leq" "geq"
    "lnot" "land" "lor" "equiv"
    "prec" "succ" "preceq" "succeq"
    "sim" "simeq" "asymp" "approx"
    "cong" "doteq" "propto"
    "gg" "ll" "sqsubseteq" "sqsupseteq"
    "star" "bullet")
  "Named operators written with a backslash prefix (e.g. \\in, \\cup).")

(defvar tlaplus-font-lock-keywords
  `(
    ;; ---- MODULE Name ----
    ("^-\\{4,\\}\\(.*\\)-\\{4,\\}$" (0 font-lock-preprocessor-face t))
    ("^-\\{4,\\}\\s-*MODULE\\s-+\\(\\w+\\)"
     (1 font-lock-type-face t))

    ;; ==== (module footer)
    ("^=\\{4,\\}" (0 font-lock-preprocessor-face))

    ;; Operator definitions: Name == ... or Name(args) ==
    ("\\<\\([A-Za-z_]\\w*\\)\\(?:\\s-*(\\([^)]*\\))\\)?\\s-*==[^=]"
     (1 font-lock-function-name-face))

    ;; Names declared after CONSTANT(S) / VARIABLE(S)
    ("\\<\\(?:CONSTANTS?\\|VARIABLES?\\)\\>[ \t]+\\([^\n]+\\)"
     (1 font-lock-variable-name-face))

    ;; Keywords
    (,(regexp-opt tlaplus--keywords 'words) . font-lock-keyword-face)

    ;; Boolean constants
    (,(regexp-opt tlaplus--constants 'words) . font-lock-constant-face)

    ;; Built-in types
    (,(regexp-opt tlaplus--builtin-types 'words) . font-lock-type-face)

    ;; Standard library names
    (,(concat "\\<" (regexp-opt tlaplus--stdlib-names t) "\\>")
     (1 font-lock-type-face))

    ;; Quantifiers: \A  \E  \AA  \EE
    ("\\\\\\(AA?\\|EE?\\)\\>" . font-lock-builtin-face)

    ;; Temporal operators: []  <>  ~>
    ("\\(\\[\\]\\|<>\\|~>\\)" (1 font-lock-builtin-face))

    ;; Fairness prefixes: WF_  SF_
    ("\\<\\([WS]F_\\)" (1 font-lock-builtin-face))

    ;; Backslash operators: \in  \cup  \subseteq  etc.
    (,(concat "\\\\\\(?:"
              (regexp-opt tlaplus--backslash-operators)
              "\\)\\>")
     . font-lock-builtin-face)

    ;; Conjunction /\ and disjunction \/
    ("/\\\\\\|\\\\/" . font-lock-builtin-face)

    ;; Number literals
    ("\\<[0-9]+\\>" . font-lock-constant-face)

    ;; Primed variables (next-state): var'
    ("\\<\\([A-Za-z_]\\w*\\)'" (1 font-lock-variable-name-face)))
  "Font lock keywords for `tlaplus-mode'.")

;;; Prettify Symbols — mathematical display

(defconst tlaplus-prettify-symbols-alist
  '(;; Logical connectives
    ("/\\"    . ?∧)
    ("\\/"    . ?∨)
    ("~"      . ?¬)
    ("\\lnot" . ?¬)
    ("\\land" . ?∧)
    ("\\lor"  . ?∨)
    ("\\equiv" . ?≡)
    ("=>"     . ?⇒)
    ("<=>"    . ?⇔)
    ;; Quantifiers
    ("\\A"    . ?∀)
    ("\\E"    . ?∃)
    ;; Temporal
    ("[]"     . ?□)
    ("<>"     . ?◇)
    ("~>"     . ?⇝)
    ;; Set operators
    ("\\in"        . ?∈)
    ("\\notin"     . ?∉)
    ("\\subseteq"  . ?⊆)
    ("\\supseteq"  . ?⊇)
    ("\\subset"    . ?⊂)
    ("\\supset"    . ?⊃)
    ("\\cup"       . ?∪)
    ("\\cap"       . ?∩)
    ("\\union"     . ?∪)
    ("\\intersect" . ?∩)
    ("\\bigcup"    . ?⋃)
    ("\\bigcap"    . ?⋂)
    ("\\times"     . ?×)
    ("\\sqsubseteq" . ?⊑)
    ("\\sqsupseteq" . ?⊒)
    ;; Relations
    ("<="        . ?≤)
    (">="        . ?≥)
    ("=<"        . ?≤)
    ("\\leq"     . ?≤)
    ("\\geq"     . ?≥)
    ("\\prec"    . ?≺)
    ("\\succ"    . ?≻)
    ("\\preceq"  . ?≼)
    ("\\succeq"  . ?≽)
    ("\\sim"     . ?∼)
    ("\\simeq"   . ?≃)
    ("\\asymp"   . ?≍)
    ("\\approx"  . ?≈)
    ("\\cong"    . ?≅)
    ("\\doteq"   . ?≐)
    ("\\propto"  . ?∝)
    ("\\ll"      . ?≪)
    ("\\gg"      . ?≫)
    ;; Arrows
    ("->"     . ?→)
    ("<-"     . ?←)
    ("|->"    . ?↦)
    ;; Algebraic
    ("\\circ"   . ?∘)
    ("\\oplus"  . ?⊕)
    ("\\ominus" . ?⊖)
    ("\\otimes" . ?⊗)
    ("\\oslash" . ?⊘)
    ("\\odot"   . ?⊙)
    ("\\div"    . ?÷)
    ("\\star"   . ?⋆)
    ("\\bullet" . ?•)
    ;; Miscellaneous
    ("#"      . ?≠)
    ("/="     . ?≠)
    ("\\noteq" . ?≠)
    ("<<"     . ?⟨)
    (">>"     . ?⟩)
    ("=="     . ?≜))
  "Mapping of TLA+ ASCII operators to Unicode mathematical symbols.
Used by `prettify-symbols-mode'.  The buffer text is unchanged;
only the display is affected.")

(defun tlaplus-prettify-compose-p (start end _match)
  "Return non-nil if the symbol between START and END should be composed.
Blocks composition when the match boundary falls inside a word
\(e.g. \\in inside \\intersect), when a punctuation match is
adjacent to the same character (e.g. == inside ====), or inside
a string literal.  Different punctuation characters do not block
each other, so << and >> compose independently in <<>>."
  (save-excursion
    (not (or (tlaplus--boundary-extends-p (char-before start) (char-after start))
             (tlaplus--boundary-extends-p (char-after end) (char-before end))
             (nth 3 (syntax-ppss start))))))

(defun tlaplus--boundary-extends-p (adjacent boundary)
  "Return non-nil if ADJACENT would extend the token at BOUNDARY.
For word characters, any adjacent word character extends the token.
For punctuation, only the same character extends it."
  (when (and adjacent boundary)
    (if (memq (char-syntax boundary) '(?w ?_))
        (memq (char-syntax adjacent) '(?w ?_))
      (eq adjacent boundary))))

;;; Indentation

(defun tlaplus-indent-line ()
  "Indent current line as TLA+ code."
  (interactive)
  (let ((indent (tlaplus--calculate-indentation)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))

(defun tlaplus--calculate-indentation ()
  "Calculate the proper indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (let ((ppss (syntax-ppss (point))))
      (skip-chars-forward " \t")
      (cond
       ;; Module delimiters at column 0
       ((looking-at "-\\{4,\\}\\|=\\{4,\\}")
        0)
       ;; Inside a block comment — preserve current indentation
       ((nth 4 ppss)
        (current-indentation))
       ;; Line starting with (* or \* — comment line, preserve indentation
       ((looking-at "(\\*\\|\\\\\\*")
        (current-indentation))
       ;; Closing bracket — align with line of opening bracket
       ((looking-at "[])}]")
        (tlaplus--closing-bracket-indent))
       ;; Conjunct / disjunct line — align with previous conjunct
       ((looking-at "/\\\\\\|\\\\/")
        (or (tlaplus--conjunct-column) (tlaplus--context-indent)))
       ;; IN keyword — align with matching LET
       ((looking-at "IN\\>")
        (or (tlaplus--matching-let-column) (tlaplus--context-indent)))
       ;; Top-level definition (Name == ...) at bracket depth 0, not in LET
       ((and (looking-at "[A-Za-z_]\\w*\\(?:\\s-*([^)]*)\\)?\\s-*==")
             (= (car ppss) 0)
             (not (tlaplus--inside-let-p)))
        0)
       ;; Inside unclosed brackets — indent from opening line
       ((> (car ppss) 0)
        (or (tlaplus--bracket-indent) (tlaplus--context-indent)))
       ;; Default — compute from previous line
       (t
        (tlaplus--context-indent))))))

(defun tlaplus--goto-prev-code-line ()
  "Move to the previous non-blank line.  Return t if found."
  (let ((found nil))
    (while (and (not found) (not (bobp)))
      (forward-line -1)
      (unless (looking-at "^\\s-*$")
        (setq found t)))
    found))

(defun tlaplus--prev-line-in-comment-p ()
  "Return non-nil if the current line is inside a block comment."
  (let ((ppss (syntax-ppss (line-beginning-position))))
    (or (nth 4 ppss)
        (save-excursion
          (back-to-indentation)
          (looking-at "(\\*\\|\\*)")))))

(defun tlaplus--context-indent ()
  "Compute indentation from the previous non-blank line."
  (save-excursion
    (beginning-of-line)
    (if (not (tlaplus--goto-prev-code-line))
        0
      (if (tlaplus--prev-line-in-comment-p)
          ;; Previous line is a comment — just use its indentation
          (current-indentation)
        (let ((prev-indent (current-indentation)))
          (back-to-indentation)
          (cond
           ;; Previous line ends with == (definition body follows)
           ((looking-at ".*==\\s-*$")
            (+ prev-indent tlaplus-indent-offset))
           ;; Previous line ends with : (quantifier body follows)
           ((looking-at ".*[^:]:\\s-*$")
            (+ prev-indent tlaplus-indent-offset))
           ;; Previous line starts with LET (definitions follow)
           ((looking-at "LET\\>")
            (+ prev-indent tlaplus-indent-offset))
           ;; Default — same indentation as previous line
           (t prev-indent)))))))

(defun tlaplus--conjunct-column ()
  "Find column to align a /\\ or \\/ with the nearest conjunct above.
Searches backwards through non-blank lines for a line starting with
/\\ or \\/.  Stops at definition boundaries (lines containing ==)."
  (save-excursion
    (beginning-of-line)
    (catch 'done
      (while (tlaplus--goto-prev-code-line)
        (back-to-indentation)
        (cond
         ;; Previous line starts with /\ or \/ — align with it
         ((looking-at "/\\\\\\|\\\\/")
          (throw 'done (current-column)))
         ;; Hit a definition line — check for inline conjunct
         ((looking-at "[A-Za-z_]\\w*\\(?:\\s-*([^)]*)\\)?\\s-*==")
          (if (re-search-forward "/\\\\\\|\\\\/" (line-end-position) t)
              (throw 'done (progn (goto-char (match-beginning 0))
                                  (current-column)))
            (throw 'done nil)))
         ;; Hit a module delimiter — stop
         ((looking-at "-\\{4,\\}\\|=\\{4,\\}")
          (throw 'done nil))))
      nil)))

(defun tlaplus--matching-let-column ()
  "Find the indentation column of the LET matching the current IN."
  (save-excursion
    (let ((depth 1))
      (while (and (> depth 0)
                  (re-search-backward "\\<\\(LET\\|IN\\)\\>" nil t))
        (let ((ppss (syntax-ppss)))
          (unless (or (nth 3 ppss) (nth 4 ppss))
            (if (string= (match-string 1) "IN")
                (setq depth (1+ depth))
              (setq depth (1- depth))))))
      (when (= depth 0)
        (current-indentation)))))

(defun tlaplus--closing-bracket-indent ()
  "Indent a closing bracket to align with the line of its opener."
  (save-excursion
    (let ((opener (nth 1 (syntax-ppss (point)))))
      (if opener
          (progn (goto-char opener) (current-indentation))
        0))))

(defun tlaplus--bracket-indent ()
  "Indent content inside unclosed brackets from the opening line."
  (save-excursion
    (let ((opener (nth 1 (syntax-ppss (line-beginning-position)))))
      (when opener
        (goto-char opener)
        (+ (current-indentation) tlaplus-indent-offset)))))

(defun tlaplus--inside-let-p ()
  "Return non-nil if point is inside a LET block (between LET and IN)."
  (save-excursion
    (catch 'result
      (let ((depth 0))
        (while (re-search-backward "\\<\\(LET\\|IN\\)\\>" nil t)
          (let ((ppss (syntax-ppss)))
            (unless (or (nth 3 ppss) (nth 4 ppss))
              (if (string= (match-string 1) "IN")
                  (setq depth (1+ depth))
                (if (> depth 0)
                    (setq depth (1- depth))
                  (throw 'result t))))))
        nil))))

;;; Imenu

(defvar tlaplus-imenu-generic-expression
  '(("Operators"    "^\\s-*\\([A-Za-z_]\\w*\\)\\(?:\\s-*([^)]*)\\)?\\s-*==" 1)
    ("Constants"    "^\\s-*CONSTANTS?\\s-+\\(.*\\)" 1)
    ("Variables"    "^\\s-*VARIABLES?\\s-+\\(.*\\)" 1)
    ("Theorems"     "^\\s-*THEOREM\\s-+\\(\\w+\\)" 1)
    ("Assumptions"  "^\\s-*ASSUME\\s-+\\(\\w+\\)" 1))
  "Imenu generic expression for `tlaplus-mode'.
Indexes operator definitions, declarations, theorems, and assumptions.")

;;; Mode Definition

;;;###autoload
(define-derived-mode tlaplus-mode prog-mode "TLA+"
  "Major mode for editing TLA+ specifications.

Provides syntax highlighting for TLA+ keywords, operators, temporal
logic constructs, and both comment styles: block comments (* ... *)
and line comments \\=\\*.

When `tlaplus-prettify-symbols' is non-nil, mathematical Unicode
symbols are displayed for ASCII operators (toggle with
\\[prettify-symbols-mode]).

Navigation:
  \\[imenu]  Jump to operator/variable/constant definitions.

\\{tlaplus-mode-map}"
  :syntax-table tlaplus-mode-syntax-table
  :group 'tlaplus
  ;; TLA+ is case-sensitive (keywords are uppercase, \in != IN)
  (setq-local case-fold-search nil)
  ;; Line comments: \*
  (setq-local comment-start "\\* ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?:\\\\\\*\\|(\\*+\\)\\s-*")
  ;; Syntax propertize for \* line comments
  (setq-local syntax-propertize-function #'tlaplus-syntax-propertize)
  ;; Font lock
  (setq-local font-lock-defaults '(tlaplus-font-lock-keywords nil nil))
  ;; Prettify symbols
  (setq-local prettify-symbols-alist tlaplus-prettify-symbols-alist)
  (setq-local prettify-symbols-compose-predicate #'tlaplus-prettify-compose-p)
  (when tlaplus-prettify-symbols
    (prettify-symbols-mode 1))
  ;; Indentation
  (setq-local indent-line-function #'tlaplus-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width tlaplus-indent-offset)
  ;; Imenu
  (setq-local imenu-generic-expression tlaplus-imenu-generic-expression)
  ;; Paragraphs (useful for block-comment filling)
  (setq-local paragraph-start (concat "\\s-*$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tla\\'" . tlaplus-mode))

(provide 'tlaplus-mode)
;;; tlaplus-mode.el ends here
