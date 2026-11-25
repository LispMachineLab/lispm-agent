EMACS ?= emacs
# LispM UI (including Kit and Finder), which draws LispM's forms and
# translates its words, and LispM Auth, which keeps its secrets encrypted,
# libraries only git holds: checkouts beside this one by default.
LISPM_UI ?= ../lispm-ui
LISPM_AUTH ?= ../lispm-auth
# Optional kernel checkout for the real worker integration tests.
LISPM_KERNEL ?= ../lispm-kernel
# The sources, in lisp/, in strict dependency layer order (Layers 0 -> 4).
ELS = $(addprefix lisp/, \
      lispm-i18n.el lispm-exec.el lispm-local-http.el lispm-websocket.el lispm-agents-context.el \
      lispm-util.el lispm-secrets.el lispm-core.el lispm-job.el lispm-sandbox.el lispm-services.el \
      lispm-roles.el lispm-media.el lispm-transcript.el lispm-anthropic.el lispm-openai.el lispm-api.el \
      lispm-deftool-macros.el lispm-agents-core.el lispm-agents-sessions.el lispm-agents-terminals.el \
      lispm-agents.el lispm-agents-mcp.el lispm-agents-ide.el \
      lispm-agents-claude.el lispm-agents-codex.el \
      lispm-tool-core.el lispm-tool-permissions.el lispm-tool-search.el lispm-tool-builtins.el \
      lispm-shell-classify.el lispm-tools.el lispm-elisp-worker.el lispm-elisp.el \
      lispm-memory-store.el lispm-memory.el \
      lispm-inbox.el lispm-journal.el lispm-session.el lispm-turns.el lispm-conditions.el \
      lispm-approval.el lispm-runtime.el lispm-lisp-check.el lispm-lisp.el lispm-lisp-macros.el \
      lispm-collab.el lispm-mcp.el \
      lispm-title.el lispm-episodes.el lispm-emacs.el \
      lispm-develop-core.el lispm-develop.el lispm-skills-core.el lispm-skills.el \
      lispm-characters.el lispm-character-rag.el lispm-character-comfyui.el lispm-agents-bridge.el \
      lispm-mcp-server.el lispm-ground.el lispm-presentations.el lispm-inspect.el \
      lispm-agents-roster.el lispm-settings.el \
      lispm-backend-form.el lispm-runtime-tools.el lispm-session-tools.el lispm-process-tools.el \
      lispm-stage.el lispm-edits.el lispm-spotlight.el lispm-stage-runner.el \
      lispm-shell-stage.el lispm-eval-stage.el \
      lispm-chat-items.el lispm-chat-view.el lispm-chat.el \
      lispm-agents-work.el lispm-office-scope.el lispm-office-actions.el lispm-office-inspect.el lispm-office.el \
      lispm-hyperbole.el lispm-bar-core.el lispm-bar.el \
      lispm-batch.el lispm.el lispm-terminal.el)
# The dependencies of lispm.el (Package-Requires), installed by `make deps'.
DEPS = --eval "(progn (require 'package) (setq package-user-dir (expand-file-name \".deps\")) (package-initialize))" \
       -L $(LISPM_UI) -L $(LISPM_AUTH)

.PHONY: all deps compile check-layers test test-kernel i18n demo clean

all: compile check-layers test

deps: .deps/.installed

.deps/.installed: lisp/lispm.el test/install-deps.el
	$(EMACS) -Q --batch -L $(LISPM_UI) -L $(LISPM_AUTH) -l test/install-deps.el
	touch $@

# Warnings are errors: CI compiles with every supported Emacs.
compile: deps
	$(EMACS) -Q --batch $(DEPS) -L lisp --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(ELS)

check-layers:
	$(EMACS) -Q --batch -L test -l test/check-boundaries.el -f lispm-check-boundaries-batch-and-exit

# Hyperbole's tests run in an Emacs of their own: loading Hyperbole
# changes syntax tables, keys and Org for the whole Emacs.
test: deps
	$(EMACS) -Q --batch $(DEPS) -L lisp -L test -l test/lispm-test.el -l test/lispm-elisp-test.el \
	  -l test/lispm-lisp-macros-test.el \
	  -l test/lispm-stage-test.el -l test/lispm-spotlight-test.el -l test/lispm-shell-stage-test.el -l test/lispm-eval-stage-test.el \
	  -l test/lispm-runtime-tools-test.el -l test/lispm-session-tools-test.el \
	  -l test/lispm-process-tools-test.el -l test/lispm-tool-search-test.el \
	  -l test/lispm-mcp-lifecycle-test.el -l test/lispm-security-test.el \
	  -l test/lispm-correctness-test.el -l test/lispm-basis-test.el -l test/lispm-unify-test.el \
	  -l test/lispm-layers-test.el -l test/lispm-office-test.el \
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
