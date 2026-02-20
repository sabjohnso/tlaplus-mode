.POSIX:
EMACS ?= emacs

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile tlaplus-mode.el

check: compile
	$(EMACS) -Q --batch -L . -L test \
	  -l test/test-helper.el \
	  -l test/tlaplus-mode-test.el \
	  -f ert-run-tests-batch-and-exit

test: check

lint:
	$(EMACS) -Q --batch -L . \
	  --eval '(require (quote package))' \
	  --eval '(package-initialize)' \
	  --eval '(unless (package-installed-p (quote package-lint)) (package-refresh-contents) (package-install (quote package-lint)))' \
	  -f package-lint-batch-and-exit tlaplus-mode.el

clean:
	rm -f *.elc test/*.elc

.PHONY: compile check test lint clean
