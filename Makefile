.POSIX:
EMACS ?= emacs

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile tlaplus-mode.el

check: compile
	$(EMACS) -Q --batch -L . -L test \
	  -l test/test-helper.el \
	  -l test/tlaplus-mode-test.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc

.PHONY: compile check clean
