EMACS ?= emacs

.PHONY: test compile native native-test webext webext-test

# Tests stub a few primitives; the code they test runs uncompiled, so the
# native compiler need not build trampolines for them, which takes seconds.
test:
	$(EMACS) -Q --batch --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -L lisp -l test/lispm-desktop-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch --eval '(package-initialize)' -L lisp -f batch-byte-compile lisp/*.el; \
	status=$$?; rm -f lisp/*.elc; exit $$status

# lispm-desktop-term's module: terminals emulated off Emacs's thread, in Rust.
# lispm-desktop builds it itself at the first start where Cargo is, and again
# at a start that finds its sources changed.
native:
	cd native/lispm-desktop-term && cargo build --release

# The module's own tests, which need no Emacs.
native-test:
	cd native/lispm-desktop-term && cargo test --locked

# lispm-desktop-browser-extensions's module: Chrome extensions in the
# browser's pages, on macOS, in Swift.  lispm-desktop builds it itself the
# first time an extension is added, and again at a start that finds its
# sources changed.
webext:
	cd native/lispm-desktop-webext && swift build -c release --product lispm_desktop_webext

# The module's own tests, which need no Emacs.
webext-test:
	cd native/lispm-desktop-webext && swift test
