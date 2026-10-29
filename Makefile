EMACS ?= emacs
# LispM Kit, whose engine translates LispM's words, and LispM UI, which
# draws LispM's forms, libraries only git holds: checkouts beside this one
# by default.
LISPM_KIT ?= ../lispm-kit
LISPM_UI ?= ../lispm-ui
# Optional kernel checkout for the real worker integration tests.
LISPM_KERNEL ?= ../lispm-kernel
# The sources, in lisp/, in the order they require each other.
ELS = $(addprefix lisp/,lispm-i18n.el lispm-exec.el lispm-local-http.el lispm-websocket.el lispm-agents-context.el \
      lispm-agents.el lispm-agents-mcp.el lispm-agents-ide.el lispm-agents-claude.el lispm-agents-codex.el \
      lispm-core.el lispm-sandbox.el lispm-services.el lispm-roles.el lispm-transcript.el lispm-anthropic.el lispm-openai.el \
      lispm-api.el lispm-tools.el lispm-elisp-worker.el lispm-elisp.el \
      lispm-edits.el lispm-memory.el lispm-inbox.el lispm-journal.el lispm-runtime.el lispm-lisp.el lispm-collab.el lispm-mcp.el lispm-title.el lispm-episodes.el lispm-emacs.el lispm-develop.el lispm-skills.el lispm-characters.el lispm-agents-bridge.el lispm-mcp-server.el lispm-ground.el lispm-inspect.el lispm-hyperbole.el lispm-settings.el \
      lispm-agents-roster.el lispm-runtime-tools.el lispm-session-tools.el lispm-process-tools.el \
      lispm-chat.el lispm-bar.el lispm.el lispm-terminal.el)
# The dependencies of lispm.el (Package-Requires), installed by `make deps'.
DEPS = --eval "(progn (require 'package) (setq package-user-dir (expand-file-name \".deps\")) (package-initialize))" \
       -L $(LISPM_KIT) -L $(LISPM_UI)

.PHONY: all deps compile test test-kernel i18n demo clean

all: compile test

deps: .deps/.installed

.deps/.installed: lisp/lispm.el test/install-deps.el
	$(EMACS) -Q --batch -L $(LISPM_KIT) -L $(LISPM_UI) -l test/install-deps.el
	touch $@

# Warnings are errors: CI compiles with every supported Emacs.
compile: deps
	$(EMACS) -Q --batch $(DEPS) -L lisp --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(ELS)

# Hyperbole's tests run in an Emacs of their own: loading Hyperbole
# changes syntax tables, keys and Org for the whole Emacs.
test: deps
	$(EMACS) -Q --batch $(DEPS) -L lisp -L test -l test/lispm-test.el -l test/lispm-elisp-test.el \
	  -l test/lispm-runtime-tools-test.el -l test/lispm-session-tools-test.el \
	  -l test/lispm-process-tools-test.el -l test/lispm-tool-search-test.el \
	  -l test/lispm-mcp-lifecycle-test.el \
	  -f ert-run-tests-batch-and-exit
	$(EMACS) -Q --batch $(DEPS) -L lisp -L test -l test/lispm-hyperbole-test.el \
	  --eval '(ert-run-tests-batch-and-exit "^lispm-hyperbole-test-")'
	python3 test/lispm-mcp-client-test.py

test-kernel: deps
	LISPM_KERNEL_PROGRAM=$(abspath $(LISPM_KERNEL))/build/lispm-kernel \
	  $(EMACS) -Q --batch $(DEPS) -L $(LISPM_KERNEL)/lisp -L lisp -L test \
	  -l test/lispm-test.el -l test/lispm-elisp-kernel-test.el \
	  --eval '(ert-run-tests-batch-and-exit "^lispm-elisp-kernel-test-")'

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
