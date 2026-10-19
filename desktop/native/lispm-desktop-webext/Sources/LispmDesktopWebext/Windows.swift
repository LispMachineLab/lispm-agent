// lispm-desktop-webext: the windows an extension opens of its own.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later
//
// windows.create opens a window, most often a small one showing a page of
// the extension's own: a wallet's approval of a transaction, a password
// manager's vault, a hardware wallet's bridge.  Emacs's pages are no place
// for it: Emacs on macOS cannot hand a page its keys (they reach a field
// through JavaScript, see lispm-desktop-browser-hints.el), and a wallet's
// password wants real ones.  So such a window is a Mac window here, its page
// taking the keyboard as any Mac app's would, and to WebKit it is a window
// of its own with one tab: windows.remove closes it, closing it is
// windows.onRemoved, and bringing it in front is windows.onFocusChanged.
// A window of websites only (windows.create of a normal window) opens its
// pages as Emacs's tabs instead.
//
// Its size and place come from the extension, kept on screen as Search keeps
// them (Search, https://github.com/driceroland/Search, at 6294b6a:
// Extensions.onScreen and popupFrame in Sources/Search/Extensions.swift,
// MIT; the license is in LICENSE.Search).

import AppKit
import WebKit
import WebextCore

@MainActor
final class ExtensionWindows {
    static let shared = ExtensionWindows()

    /// Every window an extension opened, oldest first.
    private(set) var open: [PopupWindow] = []

    /// Whether a window asked for opens as a window here: a popup, or one
    /// showing only the extension's own pages.
    static func wanted(_ configuration: WKWebExtension.WindowConfiguration, by context: WKWebExtensionContext) -> Bool {
        configuration.windowType == .popup
            || (!configuration.tabURLs.isEmpty && configuration.tabURLs.allSatisfy { own($0, of: context) })
    }

    static func own(_ url: URL, of context: WKWebExtensionContext) -> Bool {
        url.scheme?.lowercased() == Rules.scheme && url.host()?.lowercased() == context.uniqueIdentifier.lowercased()
    }

    /// A window for the first page `configuration` names, or a blank one.
    func make(_ configuration: WKWebExtension.WindowConfiguration, for context: WKWebExtensionContext,
              host: Host) -> PopupWindow {
        let url = configuration.tabURLs.first ?? URL(string: "about:blank")!
        // A page of its own can only be shown by a view made from its
        // configuration; anything else is a page like the browser's, with
        // the extensions running in it.
        let views: WKWebViewConfiguration
        if ExtensionWindows.own(url, of: context), let own = context.webViewConfiguration {
            views = own
        } else {
            views = WKWebViewConfiguration()
            views.webExtensionController = host.controller
            views.applicationNameForUserAgent = Rules.userAgentName
        }
        ExtensionWorkerRecovery.shared.attach(views.userContentController)
        let screen = (NSApp.keyWindow?.screen ?? NSScreen.main)?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1280, height: 800)
        let asked = configuration.frame
        let frame = ExtensionWindows.usable(asked).map(ExtensionWindows.onScreen)
            ?? ExtensionWindows.popupFrame(asked: asked, on: screen)
            ?? NSRect(x: screen.midX - 200, y: screen.midY - 320, width: 400, height: 640)
        let window = PopupWindow(url: url, configuration: views, type: configuration.windowType,
                                 frame: frame, extensionID: context.uniqueIdentifier, host: host)
        open.append(window)
        host.controller.didOpenWindow(window)
        host.controller.didOpenTab(window.tab)
        window.show(focused: configuration.shouldBeFocused)
        return window
    }

    func closed(_ window: PopupWindow) {
        open.removeAll { $0 === window }
    }

    /// The extension window that has the keyboard, if one has.
    var key: PopupWindow? { open.first { $0.window.isKeyWindow } }

    /// The window a frame from windows.getAll() names: WebKit turns a
    /// window's frame over from the bottom of the first screen, and is
    /// turned back here, give or take two points.
    func window(frame: CGRect) -> PopupWindow? {
        let height = NSScreen.screens.first?.frame.height ?? 0
        let found = open.filter { popup in
            let window = popup.window.frame
            return abs(window.minX - frame.minX) <= 2 && abs((height - window.maxY) - frame.minY) <= 2
                && abs(window.width - frame.width) <= 2 && abs(window.height - frame.height) <= 2
        }
        return found.count == 1 ? found[0] : nil
    }

    /// A frame asked for, when it is one: parts left unset come as numbers
    /// that aren't (NaN), and AppKit traps on a frame made of them.
    static func usable(_ asked: CGRect) -> CGRect? {
        guard !asked.isNull, [asked.minX, asked.minY, asked.width, asked.height].allSatisfy(\.isFinite),
              asked.width >= 200, asked.height >= 150 else { return nil }
        return asked
    }

    /// A frame an extension asked for, kept where you can see it: no larger
    /// than the screen it is mostly on, and with a good part of it on one —
    /// never a window pushed off every screen, or one bigger than all of them.
    static func onScreen(_ asked: NSRect) -> NSRect {
        let screens = NSScreen.screens.map(\.visibleFrame)
        func area(_ rect: NSRect) -> CGFloat { rect.isNull ? 0 : rect.width * rect.height }
        guard let home = screens.max(by: { area($0.intersection(asked)) < area($1.intersection(asked)) })
                ?? NSScreen.main?.visibleFrame
        else { return asked }
        var frame = asked
        frame.size.width = min(frame.width, home.width)
        frame.size.height = min(frame.height, home.height)
        let seen = screens.map { $0.intersection(frame) }.max { area($0) < area($1) } ?? .zero
        if seen.width < min(160, frame.width) || seen.height < min(120, frame.height) {
            frame.origin.x = min(max(frame.minX, home.minX), home.maxX - frame.width)
            frame.origin.y = min(max(frame.minY, home.minY), home.maxY - frame.height)
        }
        return frame
    }

    /// A popup given its size without a place: that size, in the middle of
    /// the screen. Nil when the size isn't one either.
    static func popupFrame(asked: CGRect, on screen: CGRect) -> CGRect? {
        guard !asked.isNull, asked.width.isFinite, asked.height.isFinite, asked.width >= 200, asked.height >= 150 else { return nil }
        let width = min(asked.width, screen.width), height = min(asked.height, screen.height)
        return CGRect(x: screen.midX - width / 2, y: screen.midY - height / 2, width: width, height: height)
    }
}

/// One window an extension opened, with its one page.
@MainActor
final class PopupWindow: NSObject, WKWebExtensionWindow, NSWindowDelegate, WKUIDelegate {
    let window: NSWindow
    private(set) var tab: PopupWindowTab!
    let type: WKWebExtension.WindowType
    let extensionID: String
    unowned let host: Host
    private var titled: NSKeyValueObservation?

    init(url: URL, configuration: WKWebViewConfiguration, type: WKWebExtension.WindowType, frame: NSRect,
         extensionID: String, host: Host) {
        self.type = type
        self.extensionID = extensionID
        self.host = host
        window = NSWindow(contentRect: NSRect(origin: .zero, size: frame.size),
                          styleMask: [.titled, .closable, .miniaturizable, .resizable],
                          backing: .buffered, defer: false)
        window.isReleasedWhenClosed = false
        window.setFrame(frame, display: false)
        let web = WKWebView(frame: window.contentLayoutRect, configuration: configuration)
        web.autoresizingMask = [.width, .height]
        web.isInspectable = true
        window.contentView = web
        super.init()
        tab = PopupWindowTab(view: web, owner: self)
        window.delegate = self
        web.uiDelegate = self
        window.title = Host.shared?.contexts[extensionID]?.webExtension.displayName ?? ""
        titled = web.observe(\.title) { [weak self] web, _ in
            MainActor.assumeIsolated {
                guard let self, let title = web.title, !title.isEmpty else { return }
                self.window.title = title
            }
        }
        web.load(URLRequest(url: url))
    }

    func show(focused: Bool = true) {
        if focused {
            NSApp.activate()
            window.makeKeyAndOrderFront(nil)
            window.makeFirstResponder(tab.view)
        } else {
            window.orderFront(nil)
        }
    }

    // MARK: to WebKit, a window

    func tabs(for context: WKWebExtensionContext) -> [any WKWebExtensionTab] { [tab] }
    func activeTab(for context: WKWebExtensionContext) -> (any WKWebExtensionTab)? { tab }
    func windowType(for context: WKWebExtensionContext) -> WKWebExtension.WindowType { type }
    func isPrivate(for context: WKWebExtensionContext) -> Bool { false }

    func windowState(for context: WKWebExtensionContext) -> WKWebExtension.WindowState {
        if window.isMiniaturized { return .minimized }
        if window.styleMask.contains(.fullScreen) { return .fullscreen }
        return window.isZoomed ? .maximized : .normal
    }

    func setWindowState(_ state: WKWebExtension.WindowState, for context: WKWebExtensionContext) async throws {
        switch state {
        case .minimized: window.miniaturize(nil)
        case .maximized: if !window.isZoomed { window.zoom(nil) }
        case .fullscreen: if !window.styleMask.contains(.fullScreen) { window.toggleFullScreen(nil) }
        default:
            if window.isMiniaturized { window.deminiaturize(nil) }
            if window.styleMask.contains(.fullScreen) { window.toggleFullScreen(nil) }
        }
    }

    func frame(for context: WKWebExtensionContext) -> CGRect { window.frame }
    func screenFrame(for context: WKWebExtensionContext) -> CGRect { window.screen?.frame ?? NSScreen.main?.frame ?? .null }

    func focus(for context: WKWebExtensionContext) async throws { show() }

    func close(for context: WKWebExtensionContext) async throws { window.close() }

    /// windows.update with a position or size; unset parts come as NaN and
    /// stay as they are, and the window never goes below its smallest size.
    func setFrame(_ frame: CGRect, for context: WKWebExtensionContext) async throws {
        let now = window.frame
        func or(_ value: CGFloat, _ current: CGFloat) -> CGFloat { value.isFinite ? value : current }
        let size = NSSize(width: max(window.minSize.width, or(frame.size.width, now.width)),
                          height: max(window.minSize.height, or(frame.size.height, now.height)))
        let origin = NSPoint(x: or(frame.origin.x, now.origin.x), y: or(frame.origin.y, now.origin.y))
        window.setFrame(ExtensionWindows.onScreen(NSRect(origin: origin, size: size)), display: true, animate: false)
    }

    // MARK: the Mac window

    func windowDidBecomeKey(_ notification: Notification) {
        host.controller.didFocusWindow(self)
    }

    func windowDidResignKey(_ notification: Notification) {
        // Back to Emacs: its window is the one in front.
        if ExtensionWindows.shared.key == nil { host.controller.didFocusWindow(host.tabs.window) }
    }

    func windowWillClose(_ notification: Notification) {
        tab.stopObserving()
        titled?.invalidate()
        host.controller.didCloseTab(tab, windowIsClosing: true)
        host.controller.didCloseWindow(self)
        ExtensionWindows.shared.closed(self)
        if let web = tab.view {
            web.uiDelegate = nil
            web.stopLoading()
        }
    }

    // MARK: its page asking

    func webViewDidClose(_ webView: WKWebView) { window.close() }

    /// A link that asks for a new window opens as a tab of Emacs's browser.
    func webView(_ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration,
                 for action: WKNavigationAction, windowFeatures: WKWindowFeatures) -> WKWebView? {
        if let url = action.request.url, (try? Rules.mayOpen(url)) != nil {
            host.events.send("open", ["url": url.absoluteString, "active": true])
        }
        return nil
    }

    func webView(_ webView: WKWebView, runJavaScriptAlertPanelWithMessage message: String,
                 initiatedByFrame frame: WKFrameInfo, completionHandler: @escaping @MainActor () -> Void) {
        let alert = NSAlert()
        alert.messageText = message
        alert.beginSheetModal(for: window) { _ in completionHandler() }
    }

    func webView(_ webView: WKWebView, runJavaScriptConfirmPanelWithMessage message: String,
                 initiatedByFrame frame: WKFrameInfo, completionHandler: @escaping @MainActor (Bool) -> Void) {
        let alert = NSAlert()
        alert.messageText = message
        alert.addButton(withTitle: "OK")
        alert.addButton(withTitle: "Cancel")
        alert.beginSheetModal(for: window) { response in completionHandler(response == .alertFirstButtonReturn) }
    }
}

/// The one page of an extension's window.
@MainActor
final class PopupWindowTab: NSObject, WKWebExtensionTab, ShimTab {
    weak var view: WKWebView?
    weak var owner: PopupWindow?
    let address: Int
    private var observations: [NSKeyValueObservation] = []

    init(view: WKWebView, owner: PopupWindow) {
        self.view = view
        self.owner = owner
        address = Int(bitPattern: Unmanaged.passUnretained(view).toOpaque())
        super.init()
        let changed = { [weak self] (properties: WKWebExtension.TabChangedProperties) in
            guard let self, let host = self.owner?.host else { return }
            host.controller.didChangeTabProperties(properties, for: self)
        }
        observations = [
            view.observe(\.title) { _, _ in MainActor.assumeIsolated { changed(.title) } },
            view.observe(\.url) { _, _ in MainActor.assumeIsolated { changed(.URL) } },
            view.observe(\.isLoading) { _, _ in MainActor.assumeIsolated { changed(.loading) } },
        ]
    }

    func stopObserving() {
        observations.forEach { $0.invalidate() }
        observations = []
    }

    func bringForward() { owner?.show() }

    private func sealed(_ context: WKWebExtensionContext) -> Bool { Rules.othersPage(view?.url, for: context) }

    func window(for context: WKWebExtensionContext) -> (any WKWebExtensionWindow)? { owner }
    func indexInWindow(for context: WKWebExtensionContext) -> Int { 0 }
    func webView(for context: WKWebExtensionContext) -> WKWebView? { sealed(context) ? nil : view }
    func title(for context: WKWebExtensionContext) -> String? { view?.title }
    func url(for context: WKWebExtensionContext) -> URL? { sealed(context) ? nil : view?.url }
    func isLoadingComplete(for context: WKWebExtensionContext) -> Bool { !(view?.isLoading ?? false) }
    func isSelected(for context: WKWebExtensionContext) -> Bool { true }
    func zoomFactor(for context: WKWebExtensionContext) -> Double { Double(view?.pageZoom ?? 1) }
    func size(for context: WKWebExtensionContext) -> CGSize { view?.bounds.size ?? .zero }
    func shouldGrantPermissionsOnUserGesture(for context: WKWebExtensionContext) -> Bool { true }

    func setZoomFactor(_ zoomFactor: Double, for context: WKWebExtensionContext) async throws {
        view?.pageZoom = CGFloat(zoomFactor)
    }

    func loadURL(_ url: URL, for context: WKWebExtensionContext) async throws {
        try Rules.mayOpen(url)
        view?.load(URLRequest(url: url))
    }

    func reload(fromOrigin: Bool, for context: WKWebExtensionContext) async throws {
        if fromOrigin { view?.reloadFromOrigin() } else { view?.reload() }
    }

    func goBack(for context: WKWebExtensionContext) async throws { view?.goBack() }
    func goForward(for context: WKWebExtensionContext) async throws { view?.goForward() }
    func activate(for context: WKWebExtensionContext) async throws { owner?.show() }
    func close(for context: WKWebExtensionContext) async throws { owner?.window.close() }

    func takeSnapshot(using configuration: WKSnapshotConfiguration, for context: WKWebExtensionContext) async throws -> NSImage? {
        guard let view, !sealed(context) else { return nil }
        return try await view.takeSnapshot(configuration: configuration)
    }
}
