// Emacs's pages, as WebKit's extension engine sees them: tabs in a window.
//
// From Search (https://github.com/driceroland/Search) at
// 6294b6aed35299c7ab91399a4b2374dd726b1f39, Sources/Search/Extensions.swift:
// the ExtensionTab and ExtensionWindow adapters.  There a tab is Search's
// own object, here it is a page view Emacs made: each one becomes a tab as
// it is made (Hooks.swift), in the order made, and the browser's one
// window holds them all.  Which one is in front Emacs says, as its windows
// change (`lispm-desktop-webext-module-activate').
//
// Copyright (c) 2026 Office Commun
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: MIT
// (The license is in LICENSE.Search, beside Package.swift.)

import AppKit
import WebKit
import WebextCore

@MainActor
final class Tabs {
    unowned let host: Host
    private(set) lazy var window = ExtensionWindow(tabs: self)
    /// Every page, in the order made.
    private(set) var order: [ExtensionTab] = []
    /// The page in front.
    private(set) var active: ExtensionTab?

    init(host: Host) {
        self.host = host
    }

    func adopt(_ view: WKWebView) {
        let tab = ExtensionTab(view: view, tabs: self)
        // A view at the address of one that went without being killed.
        if let stale = self.tab(address: tab.address) { closed(stale) }
        order.append(tab)
        host.controller.didOpenTab(tab)
        if active == nil { activate(tab) }
    }

    func closed(address: Int) {
        if let tab = tab(address: address) { closed(tab) }
    }

    private func closed(_ tab: ExtensionTab) {
        order.removeAll { $0 === tab }
        tab.stopObserving()
        if active === tab { active = nil }
        host.controller.didCloseTab(tab, windowIsClosing: false)
    }

    func tab(address: Int) -> ExtensionTab? {
        order.first { $0.address == address }
    }

    /// The browser's pages and the pages of the extensions' own windows.
    var everyTab: [any ShimTab] { order + ExtensionWindows.shared.open.map(\.tab) }

    /// Whether `tab` is the one in front of its window.
    func isFront(_ tab: any ShimTab) -> Bool {
        tab is PopupWindowTab || tab === active
    }

    /// The tab at `index` of the window an extension sees with this frame —
    /// `left, top, width, height` from windows.getAll(). An extension's own
    /// window has its one page; any other frame is the browser's.
    func tab(windowFrame frame: CGRect, index: Int) -> (any ShimTab)? {
        if let window = ExtensionWindows.shared.window(frame: frame) { return index == 0 ? window.tab : nil }
        return order.indices.contains(index) ? order[index] : nil
    }

    func activate(address: Int) {
        if let tab = tab(address: address) { activate(tab) }
    }

    private func activate(_ tab: ExtensionTab) {
        guard tab !== active else { return }
        let previous = active
        active = tab
        host.controller.didActivateTab(tab, previousActiveTab: previous)
        // Buttons and badges are each page's.
        host.changed()
    }

    func changed(_ properties: WKWebExtension.TabChangedProperties, of tab: ExtensionTab) {
        guard order.contains(where: { $0 === tab }) else { return }
        host.controller.didChangeTabProperties(properties, for: tab)
    }
}

@MainActor
final class ExtensionTab: NSObject, WKWebExtensionTab, ShimTab {
    weak var view: WKWebView?
    /// The view's address, as Emacs prints its xwidget.
    let address: Int
    unowned let tabs: Tabs
    private var observations: [NSKeyValueObservation] = []

    init(view: WKWebView, tabs: Tabs) {
        self.view = view
        self.address = Int(bitPattern: Unmanaged.passUnretained(view).toOpaque())
        self.tabs = tabs
        super.init()
        observations = [
            view.observe(\.title) { [weak self] _, _ in
                MainActor.assumeIsolated { self.map { $0.tabs.changed(.title, of: $0) } }
            },
            view.observe(\.url) { [weak self] _, _ in
                MainActor.assumeIsolated { self.map { $0.tabs.changed(.URL, of: $0) } }
            },
            view.observe(\.isLoading) { [weak self] _, _ in
                MainActor.assumeIsolated { self.map { $0.tabs.changed(.loading, of: $0) } }
            },
        ]
    }

    func stopObserving() {
        observations.forEach { $0.invalidate() }
        observations = []
    }

    private var host: Host { tabs.host }

    /// A tab showing another extension's page gives an extension nothing of
    /// it: no view to run a script in, no address, no picture — whatever it
    /// was granted (see Rules.reachesExtensions).
    private func sealed(_ context: WKWebExtensionContext) -> Bool {
        Rules.othersPage(view?.url, for: context)
    }

    func window(for context: WKWebExtensionContext) -> (any WKWebExtensionWindow)? { tabs.window }

    func indexInWindow(for context: WKWebExtensionContext) -> Int {
        tabs.order.firstIndex { $0 === self } ?? NSNotFound
    }

    func webView(for context: WKWebExtensionContext) -> WKWebView? { sealed(context) ? nil : view }
    func title(for context: WKWebExtensionContext) -> String? { view?.title }
    func url(for context: WKWebExtensionContext) -> URL? { sealed(context) ? nil : view?.url }
    func isLoadingComplete(for context: WKWebExtensionContext) -> Bool { !(view?.isLoading ?? false) }
    func isSelected(for context: WKWebExtensionContext) -> Bool { tabs.active === self }
    func zoomFactor(for context: WKWebExtensionContext) -> Double { Double(view?.pageZoom ?? 1) }
    func size(for context: WKWebExtensionContext) -> CGSize { view?.bounds.size ?? .zero }
    func shouldGrantPermissionsOnUserGesture(for context: WKWebExtensionContext) -> Bool { true }

    func setZoomFactor(_ zoomFactor: Double, for context: WKWebExtensionContext) async throws {
        view?.pageZoom = CGFloat(zoomFactor)
        tabs.changed(.zoomFactor, of: self)
    }

    /// An extension's own page can only be shown by a view made from its
    /// configuration, which this one wasn't: it opens in a tab of its own,
    /// in place of this one.
    func loadURL(_ url: URL, for context: WKWebExtensionContext) async throws {
        try Rules.mayOpen(url)
        if url.scheme?.lowercased() == Rules.scheme, view?.url?.host() != url.host() {
            host.events.send("open", ["url": url.absoluteString, "active": true])
            host.events.send("close", ["view": address])
            return
        }
        view?.load(URLRequest(url: url))
    }

    func reload(fromOrigin: Bool, for context: WKWebExtensionContext) async throws {
        if fromOrigin { view?.reloadFromOrigin() } else { view?.reload() }
    }

    func goBack(for context: WKWebExtensionContext) async throws { view?.goBack() }
    func goForward(for context: WKWebExtensionContext) async throws { view?.goForward() }

    /// Brought in front by Emacs, which then says so (see Tabs.activate).
    func activate(for context: WKWebExtensionContext) async throws {
        bringForward()
    }

    func bringForward() {
        host.events.send("activate", ["view": address])
    }

    func close(for context: WKWebExtensionContext) async throws {
        host.events.send("close", ["view": address])
    }

    func takeSnapshot(using configuration: WKSnapshotConfiguration, for context: WKWebExtensionContext) async throws -> NSImage? {
        guard let view, !sealed(context) else { return nil }
        return try await view.takeSnapshot(configuration: configuration)
    }
}

/// The browser's one window: Emacs's, wherever its page in front is.
@MainActor
final class ExtensionWindow: NSObject, WKWebExtensionWindow {
    unowned let tabs: Tabs

    init(tabs: Tabs) {
        self.tabs = tabs
    }

    private var nsWindow: NSWindow? { tabs.active?.view?.window ?? NSApp.mainWindow }

    func tabs(for context: WKWebExtensionContext) -> [any WKWebExtensionTab] { tabs.order }
    func activeTab(for context: WKWebExtensionContext) -> (any WKWebExtensionTab)? { tabs.active }
    func windowType(for context: WKWebExtensionContext) -> WKWebExtension.WindowType { .normal }
    func isPrivate(for context: WKWebExtensionContext) -> Bool { false }

    func windowState(for context: WKWebExtensionContext) -> WKWebExtension.WindowState {
        guard let window = nsWindow else { return .normal }
        if window.isMiniaturized { return .minimized }
        if window.styleMask.contains(.fullScreen) { return .fullscreen }
        return window.isZoomed ? .maximized : .normal
    }

    func frame(for context: WKWebExtensionContext) -> CGRect { nsWindow?.frame ?? .null }
    func screenFrame(for context: WKWebExtensionContext) -> CGRect {
        nsWindow?.screen?.frame ?? NSScreen.main?.frame ?? .null
    }

    func focus(for context: WKWebExtensionContext) async throws {
        if let tab = tabs.active { tabs.host.events.send("activate", ["view": tab.address]) }
    }
}
