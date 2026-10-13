EMACS ?= emacs

.PHONY: test compile

test:
	$(EMACS) -Q --batch -L lisp -l test/lme-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch --eval '(package-initialize)' -L lisp -f batch-byte-compile lisp/*.el; \
	status=$$?; rm -f lisp/*.elc; exit $$status
