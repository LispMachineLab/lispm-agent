EMACS ?= emacs
# The sources, in lisp/, in the order they require each other.
ELS = $(addprefix lisp/,lispm-i18n.el lispm-local-http.el lispm-websocket.el lispm-agents-context.el \
      lispm-agents.el lispm-agents-mcp.el lispm-agents-ide.el lispm-agents-claude.el lispm-agents-codex.el \
      lispm-core.el lispm-sandbox.el lispm-roles.el lispm-transcript.el lispm-anthropic.el lispm-openai.el \
      lispm-api.el lispm-tools.el \
      lispm-edits.el lispm-memory.el lispm-inbox.el lispm-journal.el lispm-runtime.el lispm-lisp.el lispm-collab.el lispm-mcp.el lispm-title.el lispm-episodes.el lispm-emacs.el lispm-develop.el lispm-skills.el lispm-ground.el lispm-inspect.el \
      lispm-chat.el lispm.el)
# The dependencies of lispm.el (Package-Requires), installed by `make deps'.
DEPS = --eval "(progn (require 'package) (setq package-user-dir (expand-file-name \".deps\")) (package-initialize))"

.PHONY: all deps compile test i18n demo clean

all: compile test

deps: .deps/.installed

.deps/.installed: lisp/lispm.el test/install-deps.el
	$(EMACS) -Q --batch -l test/install-deps.el
	touch $@

# Warnings are errors: CI compiles with every supported Emacs.
compile: deps
	$(EMACS) -Q --batch $(DEPS) -L lisp --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(ELS)

test: deps
	$(EMACS) -Q --batch $(DEPS) -L lisp -L test -l test/lispm-test.el -f ert-run-tests-batch-and-exit

# The messages each catalog of i18n/ lacks, and those it holds that the code
# no longer gives; the tests fail on either.
i18n: deps
	$(EMACS) -Q --batch $(DEPS) -L lisp -L test -l test/lispm-test.el -f lispm-test-report-catalogs

# Record assets/demo/lispm-demo.mp4 and .gif again, in your own Emacs
# (macOS, ffmpeg and a model; see demo/record.sh).
demo: deps
	demo/record.sh

clean:
	rm -f lisp/*.elc test/*.elc
