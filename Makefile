EMACS ?= emacs

.PHONY: test compile

test:
	$(EMACS) -Q --batch -L lisp -l test/lme-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch --eval '(package-initialize)' -L lisp -f batch-byte-compile lisp/*.el
