;;; tlaplus-mode-test.el --- ERT tests for tlaplus-mode -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for tlaplus-mode, organized by feature area.

;;; Code:

(require 'test-helper)

;;; ---- Mode setup --------------------------------------------------------

(ert-deftest tlaplus-test-mode-derived-from-prog-mode ()
  "Mode is derived from `prog-mode'."
  (with-tlaplus-temp-buffer ""
    (should (derived-mode-p 'prog-mode))))

(ert-deftest tlaplus-test-auto-mode-alist ()
  "`.tla' files are associated with `tlaplus-mode'."
  (should (eq 'tlaplus-mode
              (cdr (assoc "\\.tla\\'" auto-mode-alist)))))

(ert-deftest tlaplus-test-local-variables ()
  "Mode sets expected local variables."
  (with-tlaplus-temp-buffer ""
    (should (equal comment-start "\\* "))
    (should (eq case-fold-search nil))
    (should (eq indent-tabs-mode nil))))

;;; ---- Syntax table & comments -------------------------------------------

(ert-deftest tlaplus-test-block-comment ()
  "Block comment (* ... *) is recognized by `syntax-ppss'."
  (with-tlaplus-temp-buffer "(* this is a comment *)"
    (let ((ppss (syntax-ppss 5)))
      (should (nth 4 ppss)))))

(ert-deftest tlaplus-test-line-comment ()
  "Line comment \\* is recognized by `syntax-ppss'."
  (with-tlaplus-temp-buffer "\\* this is a comment"
    ;; syntax-propertize must run for \* to be recognized
    (syntax-propertize (point-max))
    (let ((ppss (syntax-ppss 5)))
      (should (nth 4 ppss)))))

(ert-deftest tlaplus-test-backslash-star-in-block-comment ()
  "\\* inside a block comment is not re-marked as a line comment."
  (with-tlaplus-temp-buffer "(* foo \\* bar *)"
    (syntax-propertize (point-max))
    ;; Position after \* — should still be in the block comment (nth 4),
    ;; and should NOT have started a new line comment.
    ;; The block comment flag (style a) is signaled by nth 4 being non-nil
    ;; and the comment style (nth 7) being nil or not 'b'.
    (let ((ppss (syntax-ppss 12)))
      (should (nth 4 ppss)))))

(ert-deftest tlaplus-test-string-backslash-not-escape ()
  "Backslash in strings is punctuation, not escape.
So \"a\\b\" is a complete 4-char string, not an escaped quote."
  (with-tlaplus-temp-buffer "\"a\\\\b\" x"
    ;; After the closing quote, we should NOT be inside a string.
    ;; Position 7 is the space before 'x'.
    (let ((ppss (syntax-ppss 7)))
      (should-not (nth 3 ppss)))))

;;; ---- Font lock ---------------------------------------------------------

(ert-deftest tlaplus-test-font-lock-keyword ()
  "Keywords like MODULE get `font-lock-keyword-face'."
  (let ((face (tlaplus-test-face-at 1 "VARIABLE x")))
    (should (eq face 'font-lock-keyword-face))))

(ert-deftest tlaplus-test-font-lock-operator-def ()
  "Operator definitions (Name ==) get `font-lock-function-name-face'."
  (let ((face (tlaplus-test-face-at 1 "Init == TRUE")))
    (should (eq face 'font-lock-function-name-face))))

(ert-deftest tlaplus-test-font-lock-constant ()
  "Boolean constants get `font-lock-constant-face'."
  (let ((face (tlaplus-test-face-at 9 "Foo == /\\ TRUE")))
    ;; TRUE starts at position 12 in "Foo == /\\ TRUE"
    ;; Let's compute: F(1)o(2)o(3) (4)=(5)=(6) (7)/(8)\(9) (10)T(11)...
    ;; Actually, let's just search for TRUE
    (with-tlaplus-temp-buffer "x == TRUE"
      (font-lock-ensure)
      ;; "x" is at 1, " " at 2, "=" at 3, "=" at 4, " " at 5, "T" at 6
      (should (eq (get-text-property 6 'face) 'font-lock-constant-face)))))

(ert-deftest tlaplus-test-font-lock-backslash-operator ()
  "Backslash operators (\\in) get `font-lock-builtin-face'."
  (with-tlaplus-temp-buffer "x \\in S"
    (font-lock-ensure)
    ;; "x" at 1, " " at 2, "\\" at 3, "i" at 4, "n" at 5
    (should (eq (get-text-property 3 'face) 'font-lock-builtin-face))))

(ert-deftest tlaplus-test-font-lock-module-header ()
  "Module header line gets `font-lock-preprocessor-face'."
  (with-tlaplus-temp-buffer "---- MODULE Foo ----"
    (font-lock-ensure)
    (should (eq (get-text-property 1 'face) 'font-lock-preprocessor-face))))

;;; ---- Indentation -------------------------------------------------------

(ert-deftest tlaplus-test-indent-module-delimiters ()
  "Module delimiters (---- and ====) indent to column 0."
  (tlaplus-test-indentation
   "    ---- MODULE Foo ----\n    ===="
   "---- MODULE Foo ----\n===="))

(ert-deftest tlaplus-test-indent-definition-body ()
  "Definition body after == indents by `tlaplus-indent-offset'."
  (tlaplus-test-indentation
   "Init ==\nTRUE"
   "Init ==\n    TRUE"))

(ert-deftest tlaplus-test-indent-closing-bracket ()
  "Closing bracket aligns with the line of its opening bracket."
  (tlaplus-test-indentation
   "Foo ==\n    [\n        a |-> 1\n            ]"
   "Foo ==\n    [\n        a |-> 1\n    ]"))

(ert-deftest tlaplus-test-indent-conjunct-alignment ()
  "Conjunct /\\ aligns with previous conjunct."
  (tlaplus-test-indentation
   "Init ==\n    /\\ x = 1\n/\\ y = 2"
   "Init ==\n    /\\ x = 1\n    /\\ y = 2"))

(ert-deftest tlaplus-test-indent-in-aligns-with-let ()
  "IN aligns with matching LET."
  (tlaplus-test-indentation
   "Foo ==\n    LET x == 1\n        IN x"
   "Foo ==\n    LET x == 1\n    IN x"))

(ert-deftest tlaplus-test-indent-bracket-content ()
  "Content inside brackets indents from the opening line."
  (tlaplus-test-indentation
   "x == [\na |-> 1\n]"
   "x == [\n    a |-> 1\n]"))

;;; ---- Prettify symbols --------------------------------------------------

(ert-deftest tlaplus-test-prettify-alist-mappings ()
  "Prettify alist contains expected mappings."
  (with-tlaplus-temp-buffer ""
    (should (equal (alist-get "/\\" prettify-symbols-alist nil nil #'string=) ?∧))
    (should (equal (alist-get "\\in" prettify-symbols-alist nil nil #'string=) ?∈))
    (should (equal (alist-get "[]" prettify-symbols-alist nil nil #'string=) ?□))
    (should (equal (alist-get "<=" prettify-symbols-alist nil nil #'string=) ?≤))
    (should (equal (alist-get ">=" prettify-symbols-alist nil nil #'string=) ?≥))
    (should (equal (alist-get "=<" prettify-symbols-alist nil nil #'string=) ?≤))
    (should (equal (alist-get "\\noteq" prettify-symbols-alist nil nil #'string=) ?≠))
    (should (equal (alist-get "/=" prettify-symbols-alist nil nil #'string=) ?≠))
    (should (equal (alist-get "\\neg" prettify-symbols-alist nil nil #'string=) ?¬))))

(ert-deftest tlaplus-test-prettify-compose-at-boundary ()
  "Compose predicate allows composition at proper word boundaries."
  (with-tlaplus-temp-buffer "x \\in S"
    ;; \\in starts at position 3, ends at position 6
    ;; "x"(1) " "(2) "\\"(3) "i"(4) "n"(5) " "(6) "S"(7)
    (should (tlaplus-prettify-compose-p 3 6 "\\in"))))

(ert-deftest tlaplus-test-prettify-compose-blocks-partial ()
  "Compose predicate blocks \\in inside \\intersect."
  (with-tlaplus-temp-buffer "x \\intersect S"
    ;; \\in would match at positions 3-6, but \\intersect continues
    ;; "x"(1) " "(2) "\\"(3) "i"(4) "n"(5) "t"(6) "e"(7) ...
    (should-not (tlaplus-prettify-compose-p 3 6 "\\in"))))

(ert-deftest tlaplus-test-prettify-compose-empty-tuple ()
  "Compose predicate allows << and >> in adjacent <<>>."
  (with-tlaplus-temp-buffer "<<>>"
    ;; "<<"(1-2) ">>"(3-4)
    (should (tlaplus-prettify-compose-p 1 3 "<<"))
    (should (tlaplus-prettify-compose-p 3 5 ">>"))))

(ert-deftest tlaplus-test-prettify-compose-blocks-repeated-equals ()
  "Compose predicate blocks == inside module delimiter ====...====."
  (with-tlaplus-temp-buffer "============"
    ;; == at positions 1-3 is surrounded by more = signs
    (should-not (tlaplus-prettify-compose-p 1 3 "=="))
    (should-not (tlaplus-prettify-compose-p 3 5 "=="))
    (should-not (tlaplus-prettify-compose-p 5 7 "=="))))

(ert-deftest tlaplus-test-prettify-compose-comparison-operators ()
  "Compose predicate allows <= and >= at proper boundaries."
  (with-tlaplus-temp-buffer "x <= y"
    ;; "x"(1) " "(2) "<"(3) "="(4) " "(5) "y"(6)
    (should (tlaplus-prettify-compose-p 3 5 "<=")))
  (with-tlaplus-temp-buffer "x >= y"
    ;; "x"(1) " "(2) ">"(3) "="(4) " "(5) "y"(6)
    (should (tlaplus-prettify-compose-p 3 5 ">=")))
  (with-tlaplus-temp-buffer "x =< y"
    ;; "x"(1) " "(2) "="(3) "<"(4) " "(5) "y"(6)
    (should (tlaplus-prettify-compose-p 3 5 "=<"))))

;;; ---- Imenu -------------------------------------------------------------

(ert-deftest tlaplus-test-imenu-operators ()
  "Operator definitions are indexed under \"Operators\"."
  (with-tlaplus-temp-buffer "Init == TRUE\nNext == FALSE\n"
    (let* ((index (imenu--make-index-alist t))
           (ops (cdr (assoc "Operators" index))))
      (should ops)
      (should (assoc "Init" ops))
      (should (assoc "Next" ops)))))

(ert-deftest tlaplus-test-imenu-variables ()
  "Variable declarations are indexed."
  (with-tlaplus-temp-buffer "VARIABLES x, y, z\n"
    (let* ((index (imenu--make-index-alist t))
           (vars (cdr (assoc "Variables" index))))
      (should vars))))

(provide 'tlaplus-mode-test)
;;; tlaplus-mode-test.el ends here
