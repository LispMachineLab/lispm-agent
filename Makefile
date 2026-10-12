EMACS ?= emacs
ELS = lispm-core.el lispm-transcript.el lispm-anthropic.el lispm-openai.el \
      lispm-api.el lispm-tools.el \
      lispm-memory.el lispm-runtime.el lispm-collab.el lispm.el

.PHONY: all compile test clean

all: compile test

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(ELS)

test:
	$(EMACS) -Q --batch -L . -L test -l test/lispm-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
