// lispm-desktop-webext: Chrome extensions in lispm-desktop's browser, on macOS.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later
//
// WebKit runs Chrome extensions itself since macOS 15.4 (WKWebExtension,
// the engine Safari's extensions run on), for any page view whose
// configuration carries its controller.  This module is that controller's
// host inside Emacs: it gives Emacs's page views the controller as they are
// made (Hooks.swift), tells WebKit which pages are open and which one is in
// front (Tabs.swift), and installs, loads and runs the extensions (Host.swift,
// Popup.swift, Windows.swift).  The Chrome APIs WebKit lacks are filled in by
// a script put in every extension and answered here (ExtensionShims.swift,
// ExtensionOffscreen.swift, ExtensionSocket.swift,
// ExtensionWorkerRecovery.swift).  Most of it is ported from Search, a
// WebKit browser for macOS (https://github.com/driceroland/Search), whose
// license is in LICENSE.Search.
//
// Emacs calls the functions below; the module calls back only through one
// pipe (EmacsSwiftModule's Channel), as `lispm-desktop-webext--event' with a
// kind and a JSON payload, since Emacs gives a module no way to call Lisp
// on its own:
//
//   Lisp --functions--> module: install, run an action, which page is in front
//   WebKit --delegate--> Host --channel--> (lispm-desktop-webext--event KIND JSON)
//        questions and new tabs wait for (lispm-desktop-webext-module-answer ID VALUE)

import AppKit
import EmacsSwiftModule
import WebKit

final class WebextModule: Module {
    let isGPLCompatible = true

    func Init(_ env: Environment) throws {
        try env.defun(
            "lispm-desktop-webext-module-start",
            with: """
            Start the extension host, keeping extensions in DIRECTORY.
            Pages made from then on run them.  Return t.

            (fn DIRECTORY)
            """
        ) { (env: Environment, directory: String) throws -> Bool in
            let channel = try env.openChannel(name: "lispm-desktop-webext")
            try MainActor.assumeIsolated {
                try Host.start(directory: URL(fileURLWithPath: directory, isDirectory: true),
                               events: Events(channel: channel))
            }
            return true
        }
        try env.defun(
            "lispm-desktop-webext-module-stop",
            with: "Stop calling Emacs back, as Emacs exits."
        ) { () in
            MainActor.assumeIsolated { Host.shared?.events.stop() }
        }
        try env.defun(
            "lispm-desktop-webext-module-list",
            with: "Return the installed extensions, as a JSON array."
        ) { () -> String in
            MainActor.assumeIsolated { Host.shared?.listJSON() ?? "[]" }
        }
        try env.defun(
            "lispm-desktop-webext-module-install-store",
            with: """
            Install the Chrome Web Store extension TEXT names, a store link or an id.
            The extension is fetched and its signature checked, then the user
            is asked; events tell how it went.

            (fn TEXT)
            """
        ) { (text: String) in
            MainActor.assumeIsolated { Host.shared?.install(from: text) }
        }
        try env.defun(
            "lispm-desktop-webext-module-install-folder",
            with: """
            Install the unpacked extension in folder PATH, a copy of it.

            (fn PATH)
            """
        ) { (path: String) in
            MainActor.assumeIsolated { Host.shared?.installFolder(at: URL(fileURLWithPath: path, isDirectory: true)) }
        }
        try env.defun(
            "lispm-desktop-webext-module-reload",
            with: """
            Load extension ID again, copied afresh from its folder if it came from one.

            (fn ID)
            """
        ) { (id: String) in
            MainActor.assumeIsolated { Host.shared?.reload(id) }
        }
        try env.defun(
            "lispm-desktop-webext-module-remove",
            with: """
            Remove extension ID and what it kept.

            (fn ID)
            """
        ) { (id: String) in
            MainActor.assumeIsolated { Host.shared?.remove(id) }
        }
        try env.defun(
            "lispm-desktop-webext-module-set-enabled",
            with: """
            Turn extension ID on when ENABLED, else off.

            (fn ID ENABLED)
            """
        ) { (id: String, enabled: Bool) in
            MainActor.assumeIsolated { Host.shared?.setEnabled(id, enabled) }
        }
        try env.defun(
            "lispm-desktop-webext-module-action",
            with: """
            Press extension ID's button for the page in front: its popup opens,
            or it is told it was clicked.  Return nil when it has no button.

            (fn ID)
            """
        ) { (id: String) -> Bool in
            MainActor.assumeIsolated { Host.shared?.performAction(id) ?? false }
        }
        try env.defun(
            "lispm-desktop-webext-module-options-url",
            with: """
            Return the address of extension ID's options page, or nil.

            (fn ID)
            """
        ) { (id: String) -> String? in
            MainActor.assumeIsolated { Host.shared?.optionsURL(id) }
        }
        try env.defun(
            "lispm-desktop-webext-module-expect-page",
            with: """
            Return t when URL is the page of a running extension, which only a
            view made from that extension's configuration can show; the next
            page view Emacs makes is then made from it.

            (fn URL)
            """
        ) { (url: String) -> Bool in
            MainActor.assumeIsolated { Host.shared?.expectPage(url) ?? false }
        }
        try env.defun(
            "lispm-desktop-webext-module-activate",
            with: """
            Tell extensions the page whose view is at ADDRESS is in front.
            ADDRESS is the view's address as an xwidget prints it.

            (fn ADDRESS)
            """
        ) { (address: Int) in
            MainActor.assumeIsolated { Host.shared?.tabs.activate(address: address) }
        }
        try env.defun(
            "lispm-desktop-webext-module-answer",
            with: """
            Answer question or request ID with VALUE: 1 or nil for a question,
            the address of the new page's view or nil for a new tab.

            (fn ID VALUE)
            """
        ) { (id: Int, value: Int?) in
            MainActor.assumeIsolated { Host.shared?.answer(id, value) }
        }
        try env.defun(
            "lispm-desktop-webext-module-close-popup",
            with: "Close the extension popup that is up."
        ) { () in
            MainActor.assumeIsolated { Popup.shared.close() }
        }
        try env.funcall("provide", with: Symbol(name: "lispm-desktop-webext-module"))
    }
}

func createModule() -> Module { WebextModule() }

/// The way back to Emacs.  Each event becomes a call of
/// `lispm-desktop-webext--event' with its kind and a JSON payload, made
/// when Emacs reads the pipe.
@MainActor
final class Events {
    private let channel: Channel
    /// Set as Emacs exits: its end of the pipe is closing, and a write then
    /// would end the process.
    private(set) var stopped = false

    init(channel: Channel) {
        self.channel = channel
    }

    func send(_ kind: String, _ payload: Any = NSNull()) {
        guard !stopped else { return }
        let data = try? JSONSerialization.data(withJSONObject: payload, options: [.fragmentsAllowed])
        let json = data.flatMap { String(data: $0, encoding: .utf8) } ?? "null"
        channel.withEnvironment { env in
            try env.funcall("lispm-desktop-webext--event", with: kind, json)
        }
    }

    func message(_ text: String) {
        send("message", text)
    }

    func stop() {
        stopped = true
    }
}
