EMACS ?= emacs
ELS = lispm-core.el lispm-transcript.el lispm-anthropic.el lispm-openai.el \
      lispm-api.el lispm-tools.el \
      lispm-edits.el lispm-memory.el lispm-runtime.el lispm-collab.el lispm-mcp.el lispm-title.el lispm-emacs.el lispm-skills.el \
      lispm-chat.el lispm.el
# The dependencies of lispm.el (Package-Requires), installed by `make deps'.
DEPS = --eval "(progn (require 'package) (setq package-user-dir (expand-file-name \".deps\")) (package-initialize))"

.PHONY: all deps compile test clean

all: compile test

deps: .deps/.installed

.deps/.installed: lispm.el test/install-deps.el
	$(EMACS) -Q --batch -l test/install-deps.el
	touch $@

# Warnings are errors: CI compiles with every supported Emacs.
compile: deps
	$(EMACS) -Q --batch $(DEPS) -L . --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(ELS)

test: deps
	$(EMACS) -Q --batch $(DEPS) -L . -L test -l test/lispm-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
