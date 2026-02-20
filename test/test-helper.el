;;; test-helper.el --- Shared test utilities for tlaplus-mode -*- lexical-binding: t -*-

;;; Commentary:

;; Macros and helper functions used across tlaplus-mode ERT tests.

;;; Code:

(require 'ert)
(require 'imenu)
(require 'tlaplus-mode)

(defmacro with-tlaplus-temp-buffer (content &rest body)
  "Create a temp buffer with CONTENT in `tlaplus-mode', then execute BODY.
The buffer is killed after BODY completes."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (tlaplus-mode)
     (goto-char (point-min))
     ,@body))

(defun tlaplus-test-indentation (input expected)
  "Verify that indenting INPUT produces EXPECTED.
INPUT is inserted into a `tlaplus-mode' buffer, `indent-region' is
called on the entire buffer, and the result is compared to EXPECTED."
  (with-tlaplus-temp-buffer input
    (indent-region (point-min) (point-max))
    (should (string= (buffer-string) expected))))

(defun tlaplus-test-face-at (pos content)
  "Return the font-lock face at POS after fontifying CONTENT.
POS is 1-based (same as `point').  Returns the face (or nil) at
that position."
  (with-tlaplus-temp-buffer content
    (font-lock-ensure)
    (get-text-property pos 'face)))

(provide 'test-helper)
;;; test-helper.el ends here
