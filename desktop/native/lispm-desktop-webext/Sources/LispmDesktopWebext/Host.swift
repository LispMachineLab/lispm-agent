// The extension host: installing, loading and answering for extensions.
//
// From Search (https://github.com/driceroland/Search) at
// 6294b6aed35299c7ab91399a4b2374dd726b1f39, Sources/Search/Extensions.swift:
// the controller and its delegate, loading, the Chrome Web Store and folder
// installs, reloading, removing, and starting a dead worker again.  What is
// Search's own is left out: its windows, its settings and buttons, the APIs
// it answers itself (ExtensionShims.swift), native messaging and updates.
// Emacs stands where Search's windows and dialogs stood: it is told what
// happens, opens tabs and asks the questions (see Module.swift).
//
// Copyright (c) 2026 Office Commun
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: MIT
// (The license is in LICENSE.Search, beside Package.swift.)

import AppKit
import WebKit
import WebextCore

@MainActor
final class Host: NSObject {
    private(set) static var shared: Host?

    /// Starts the host once; a second start keeps the first.
    static func start(directory: URL, events: Events) throws {
        guard shared == nil else { return }
        try Hooks.install()
        let host = Host(directory: directory, events: events)
        shared = host
        host.loadAll()
    }

    let controller: WKWebExtensionController
    let directory: URL
    let events: Events
    private(set) var tabs: Tabs!
    private(set) var installed: [Installed] = []
    /// The loaded ones, by id.
    private(set) var contexts: [String: WKWebExtensionContext] = [:]
    /// The extension whose page the next page view shows (see expectPage).
    private var expected: WKWebExtensionContext?
    /// Ids being installed or reloaded.
    private var busy: Set<String> = []

    private init(directory: URL, events: Events) {
        self.directory = directory
        self.events = events
        WKWebExtension.MatchPattern.registerCustomURLScheme(Rules.scheme)
        let configuration = WKWebExtensionController.Configuration.default()
        let views = configuration.webViewConfiguration ?? WKWebViewConfiguration()
        views.applicationNameForUserAgent = Rules.userAgentName
        configuration.webViewConfiguration = views
        controller = WKWebExtensionController(configuration: configuration)
        super.init()
        tabs = Tabs(host: self)
        controller.delegate = self
        controller.didOpenWindow(tabs.window)
        installed = (try? JSONDecoder().decode([Installed].self, from: Data(contentsOf: listFile))) ?? []
    }

    private var listFile: URL { directory.appendingPathComponent("installed.json") }

    private func folder(for id: String) -> URL { directory.appendingPathComponent(id, isDirectory: true) }

    private func stagingFolder(for id: String) -> URL {
        directory.appendingPathComponent(".staging-\(id)-\(UUID().uuidString)", isDirectory: true)
    }

    private func save() {
        try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        try? JSONEncoder().encode(installed).write(to: listFile, options: .atomic)
    }

    // MARK: - Emacs's page views

    /// A page view about to be made from `configuration`: given the
    /// controller, or made from an extension's own configuration when it is
    /// to show that extension's page.
    func prepare(_ configuration: WKWebViewConfiguration) -> WKWebViewConfiguration {
        if let context = expected, let own = context.webViewConfiguration {
            expected = nil
            return own
        }
        configuration.webExtensionController = controller
        configuration.applicationNameForUserAgent = Rules.userAgentName
        return configuration
    }

    /// A page view Emacs has made.  It names an old Safari as its browser;
    /// every view reports the same one instead (see Rules.userAgentName).
    func adopt(_ view: WKWebView) {
        view.customUserAgent = nil
        tabs.adopt(view)
    }

    func closed(_ view: UnsafeMutableRawPointer) {
        tabs.closed(address: Int(bitPattern: view))
    }

    /// Whether `text` is a running extension's page; if so, the next page
    /// view is made to show it.
    func expectPage(_ text: String) -> Bool {
        expected = nil
        guard let url = URL(string: text), url.scheme?.lowercased() == Rules.scheme,
              let id = url.host()?.lowercased(), let context = contexts[id] else { return false }
        expected = context
        return true
    }

    // MARK: - telling Emacs

    private var changePending = false

    /// The list, a button or a badge changed: Emacs is told once this turn.
    func changed() {
        guard !changePending else { return }
        changePending = true
        DispatchQueue.main.async { [weak self] in
            MainActor.assumeIsolated {
                guard let self else { return }
                self.changePending = false
                self.events.send("changed")
            }
        }
    }

    func listJSON() -> String {
        let list: [[String: Any]] = installed.map { item in
            var entry: [String: Any] = [
                "id": item.id, "name": item.name, "version": item.version, "enabled": item.enabled,
                "fromStore": item.fromStore, "loaded": contexts[item.id] != nil,
            ]
            if let source = item.source { entry["source"] = source }
            if let context = contexts[item.id] {
                if let action = context.action(for: tabs.active) {
                    entry["label"] = action.label
                    entry["badge"] = action.badgeText
                    entry["actionEnabled"] = action.isEnabled
                    entry["popup"] = action.presentsPopup
                }
                entry["options"] = context.optionsPageURL != nil
                entry["errors"] = context.errors.map(\.localizedDescription)
            }
            return entry
        }
        let data = try? JSONSerialization.data(withJSONObject: list)
        return data.flatMap { String(data: $0, encoding: .utf8) } ?? "[]"
    }

    // MARK: - asking Emacs

    private var nextRequest = 1
    private var waiting: [Int: CheckedContinuation<Int?, Never>] = [:]

    /// Sends Emacs `kind` with a request id, and waits for its answer.
    func request(_ kind: String, _ payload: [String: Any]) async -> Int? {
        guard !events.stopped else { return nil }
        let id = nextRequest
        nextRequest += 1
        var payload = payload
        payload["request"] = id
        return await withCheckedContinuation { done in
            waiting[id] = done
            events.send(kind, payload)
        }
    }

    func answer(_ id: Int, _ value: Int?) {
        waiting.removeValue(forKey: id)?.resume(returning: value)
    }

    /// The last question asked, so the next waits for its answer.
    private var question: Task<Bool, Never>?

    /// One question at a time, asked by Emacs.
    func ask(_ question: String, details: [String]) async -> Bool {
        let before = self.question
        let task = Task { @MainActor [weak self] () -> Bool in
            _ = await before?.value
            guard let self else { return false }
            return await self.request("ask", ["question": question, "details": details]) == 1
        }
        self.question = task
        return await task.value
    }

    func ask(install name: String, wants: [String]) async -> Bool {
        await ask("Add “\(name)” to the browser?",
                  details: wants.isEmpty ? ["It doesn't ask for anything special."] : wants)
    }

    func ask(_ question: String, details: [String], context: WKWebExtensionContext) async -> Bool {
        await ask("\(context.webExtension.displayName ?? "An extension") \(question)", details: details)
    }

    // MARK: - loading

    private func loadAll() {
        Task { @MainActor in
            // One after another, a moment apart: started all at once, WebKit
            // fails some of their workers and never tries them again.
            let enabled = installed.filter(\.enabled)
            for (n, item) in enabled.enumerated() {
                await load(item)
                if n < enabled.count - 1, contexts[item.id]?.webExtension.hasBackgroundContent == true {
                    try? await Task.sleep(for: .milliseconds(400))
                }
            }
            changed()
        }
    }

    @discardableResult
    private func load(_ item: Installed) async -> Bool {
        let folder = folder(for: item.id)
        // One installed from a folder keeps no link leading out of it.
        if item.source != nil {
            do { try await Task.detached(priority: .userInitiated) { try ExtensionFiles.unlinkOutside(folder) }.value }
            catch { return false }
        }
        do {
            let found = try await WKWebExtension(resourceBaseURL: folder)
            let context = WKWebExtensionContext(for: found)
            context.uniqueIdentifier = item.id
            // The same origin every start. WebKit picks a fresh one
            // otherwise, and everything an extension keeps in its own pages
            // — localStorage, IndexedDB — is filed under its origin.
            if let stable = URL(string: "\(Rules.scheme)://\(item.id)/") { context.baseURL = stable }
            context.isInspectable = true
            // Installing was the consent: everything it asked for then is
            // granted each time it loads. Optional ones are asked for when
            // the extension asks.
            for permission in found.requestedPermissions {
                context.setPermissionStatus(.grantedExplicitly, for: permission)
            }
            Rules.grantSites(context)
            Rules.fence(context)
            try controller.load(context)
            watch(context)
            contexts[item.id] = context
            changed()
            return true
        } catch {
            events.message("\(item.name) couldn't start: \(error.localizedDescription)")
            return false
        }
    }

    @discardableResult
    private func unload(_ id: String) -> Bool {
        guard let context = contexts[id] else { return false }
        var unloaded = false
        do { try controller.unload(context); unloaded = true } catch {}
        if Popup.shared.extensionID == id { Popup.shared.close() }
        if let watcher = errorWatchers.removeValue(forKey: id) { NotificationCenter.default.removeObserver(watcher) }
        contexts[id] = nil
        changed()
        return unloaded
    }

    // MARK: - workers that won't start

    private var revived: [String: Date] = [:]
    private var errorWatchers: [String: NSObjectProtocol] = [:]

    /// WebKit records a worker that failed to start as an error on its
    /// context, and then doesn't try again: the extension would be dead
    /// until someone noticed. It is taken up afresh as soon as that shows.
    private func watch(_ context: WKWebExtensionContext) {
        let id = context.uniqueIdentifier
        if let old = errorWatchers[id] { NotificationCenter.default.removeObserver(old) }
        errorWatchers[id] = NotificationCenter.default.addObserver(
            forName: WKWebExtensionContext.errorsDidUpdateNotification, object: context, queue: .main
        ) { [weak self, weak context] _ in
            MainActor.assumeIsolated {
                guard let self, let context, self.contexts[id] === context else { return }
                let failed = context.errors.contains { error in
                    let e = error as NSError
                    return e.domain == WKWebExtensionContext.errorDomain
                        && e.code == WKWebExtensionContext.Error.backgroundContentFailedToLoad.rawValue
                }
                guard failed else { return }
                DispatchQueue.main.asyncAfter(deadline: .now() + 2) { [weak self] in
                    MainActor.assumeIsolated {
                        guard let self, self.contexts[id] === context else { return }
                        self.revive(id)
                    }
                }
            }
        }
    }

    /// An extension whose worker won't start again: unloaded and loaded,
    /// as a restart would — at most once a minute, so one that can never
    /// start doesn't go round in circles. The same context is loaded again,
    /// as the pages already holding it address it.
    private func revive(_ id: String) {
        guard let context = contexts[id], Date().timeIntervalSince(revived[id] ?? .distantPast) > 60 else { return }
        do { try controller.unload(context) } catch { return }
        revived[id] = Date()
        do {
            try controller.load(context)
            watch(context)
        } catch {
            contexts[id] = nil
            events.message("\(context.webExtension.displayName ?? id) couldn't start again: \(error.localizedDescription)")
        }
        changed()
    }

    // MARK: - installing

    /// A store link or an id.
    func install(from text: String) {
        guard let id = Crx.id(in: text) else {
            events.message(Crx.Refused.notAnID.localizedDescription)
            return
        }
        guard !installed.contains(where: { $0.id == id }) else {
            events.message("That extension is already installed")
            return
        }
        guard busy.insert(id).inserted else { return }
        let staged = stagingFolder(for: id)
        Task { @MainActor in
            defer {
                busy.remove(id)
                try? FileManager.default.removeItem(at: staged)
            }
            do {
                events.message("Fetching the extension from the Chrome Web Store…")
                let crx = try await Crx.fetch(id)
                let zip = try Crx.verifiedZip(crx, id: id)
                try await Task.detached(priority: .userInitiated) { try Crx.unpack(zip, into: staged) }.value
                try await admit(staged, as: id, fromStore: true)
            } catch {
                events.message(error.localizedDescription)
            }
        }
    }

    /// An unpacked extension from disk — a developer's own, or one exported
    /// from another browser. Copied in, so moving the original breaks nothing.
    func installFolder(at given: URL) {
        guard FileManager.default.fileExists(atPath: given.appendingPathComponent("manifest.json").path) else {
            events.message("That folder has no manifest.json")
            return
        }
        let source = URL(fileURLWithPath: Crx.realPath(of: given), isDirectory: true)
        let id = Crx.unpackedID(for: source)
        guard !installed.contains(where: { $0.id == id }) else {
            events.message("That extension is already installed: reload it to take its changes")
            return
        }
        guard busy.insert(id).inserted else { return }
        let staged = stagingFolder(for: id)
        Task { @MainActor in
            defer {
                busy.remove(id)
                try? FileManager.default.removeItem(at: staged)
            }
            do {
                try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
                // The folder itself, wherever a link to it leads.
                try FileManager.default.copyItem(at: source.resolvingSymlinksInPath(), to: staged)
                try ExtensionFiles.unlinkOutside(staged)
                try await admit(staged, as: id, fromStore: false, source: source)
            } catch {
                events.message("Couldn't install the extension: \(error.localizedDescription)")
            }
        }
    }

    /// Reads what was unpacked, asks, and — on yes — moves it into place and
    /// loads it. On no, nothing is left behind.
    private func admit(_ staged: URL, as id: String, fromStore: Bool, source: URL? = nil) async throws {
        let found = try await WKWebExtension(resourceBaseURL: staged)
        let name = found.displayName ?? id
        let version = found.version ?? "?"
        guard await ask(install: "\(name) \(version)", wants: Rules.describe(found)),
              !installed.contains(where: { $0.id == id }) else {
            events.message("\(name) wasn't added")
            return
        }
        try ExtensionFiles.replace(staged, at: folder(for: id))
        unload(id)
        let item = Installed(id: id, name: name, version: version, enabled: true, fromStore: fromStore,
                             permissions: Rules.grants(found), source: source?.path)
        installed.removeAll { $0.id == id }
        installed.append(item)
        save()
        events.message(await load(item) ? "\(name) is installed" : "\(name) is installed, but WebKit couldn't start it")
    }

    /// Takes the extension up again — the way Chrome's reload button does
    /// in developer mode. One loaded from a folder is copied in afresh from
    /// that folder first, so what its author just saved is what runs.
    func reload(_ id: String) {
        guard let original = installed.first(where: { $0.id == id }), busy.insert(id).inserted else { return }
        let target = folder(for: id)
        let staged = original.source.map { _ in stagingFolder(for: id) }
        Task { @MainActor in
            defer {
                busy.remove(id)
                if let staged { try? FileManager.default.removeItem(at: staged) }
            }
            if let path = original.source, let staged {
                let source = URL(fileURLWithPath: path, isDirectory: true)
                guard FileManager.default.fileExists(atPath: source.appendingPathComponent("manifest.json").path) else {
                    events.message("The folder \(original.name) was loaded from is gone")
                    return
                }
                guard original.sourceKeepsID else {
                    events.message("\(original.name) wasn't reloaded: its folder now leads somewhere else")
                    return
                }
                do {
                    try FileManager.default.copyItem(at: source.resolvingSymlinksInPath(), to: staged)
                    try ExtensionFiles.unlinkOutside(staged)
                } catch {
                    events.message("Couldn't copy \(original.name) again: \(error.localizedDescription)")
                    return
                }
            }
            guard let found = try? await WKWebExtension(resourceBaseURL: staged ?? target) else {
                events.message("\(original.name) wasn't reloaded: its manifest couldn't be read")
                return
            }
            let wants = Rules.grants(found)
            if !Set(wants).isSubset(of: Set(original.permissions)) {
                let name = [found.displayName ?? original.name, found.version].compactMap { $0 }.joined(separator: " ")
                guard await ask(install: name, wants: Rules.describe(found)) else {
                    events.message("\(original.name) wasn't reloaded: it asks for more than before")
                    return
                }
            }
            // Loading and asking can both suspend. A removed extension must
            // not be put back by the work that was already in flight.
            guard installed.contains(where: { $0.id == id }) else { return }
            if let staged {
                do { try ExtensionFiles.replace(staged, at: target) } catch {
                    events.message("Couldn't replace \(original.name): \(error.localizedDescription)")
                    return
                }
            }
            unload(id)
            if let index = installed.firstIndex(where: { $0.id == id }) {
                installed[index].name = found.displayName ?? installed[index].name
                installed[index].version = found.version ?? installed[index].version
                installed[index].permissions = wants
                save()
            }
            guard let item = installed.first(where: { $0.id == id }), item.enabled else { return }
            events.message(await load(item) ? "\(item.name) reloaded" : "\(item.name) couldn't start")
        }
    }

    func setEnabled(_ id: String, _ on: Bool) {
        guard let index = installed.firstIndex(where: { $0.id == id }), installed[index].enabled != on else { return }
        installed[index].enabled = on
        save()
        if on {
            let item = installed[index]
            Task { @MainActor in await load(item) }
        } else {
            unload(id)
        }
    }

    func remove(_ id: String) {
        guard installed.contains(where: { $0.id == id }) else { return }
        unload(id)
        installed.removeAll { $0.id == id }
        save()
        try? FileManager.default.removeItem(at: folder(for: id))
        Task { @MainActor in await forgetData(of: id) }
    }

    /// Its chrome.storage, which WebKit keeps by id, and what its pages
    /// kept at chrome-extension://<id>. WebKit lists no website records
    /// for that origin, so the second is cleared from a blank page at that
    /// origin, which only an extension context can host: a stand-in with no
    /// worker, scripts or pages of its own, so nothing of the removed
    /// extension runs again.
    private func forgetData(of id: String) async {
        let types = WKWebExtensionController.allExtensionDataTypes
        let records = await controller.dataRecords(ofTypes: types).filter { $0.uniqueIdentifier == id }
        if !records.isEmpty { await controller.removeData(ofTypes: types, from: records) }
        guard let base = URL(string: "\(Rules.scheme)://\(id)/") else { return }
        let folder = FileManager.default.temporaryDirectory
            .appendingPathComponent("lispm-desktop-webext-forget-\(UUID().uuidString)", isDirectory: true)
        defer { try? FileManager.default.removeItem(at: folder) }
        do {
            try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
            try Data(#"{"manifest_version": 3, "name": "lispm-desktop", "version": "1"}"#.utf8)
                .write(to: folder.appendingPathComponent("manifest.json"))
            let standIn = WKWebExtensionContext(for: try await WKWebExtension(resourceBaseURL: folder))
            standIn.uniqueIdentifier = id
            standIn.baseURL = base
            try controller.load(standIn)
            defer { try? controller.unload(standIn) }
            guard let configuration = standIn.webViewConfiguration else { return }
            let page = HiddenPage(configuration)
            guard await page.loadBlank(at: base) else { return }
            _ = try await page.web.callAsyncJavaScript(Host.forgetScript, contentWorld: .defaultClient)
        } catch {
            NSLog("lispm-desktop-webext: couldn't clear what %@ kept: %@", id, error.localizedDescription)
        }
    }

    private static let forgetScript = """
        localStorage.clear();
        for (const db of await indexedDB.databases()) {
            await new Promise(done => { const q = indexedDB.deleteDatabase(db.name); q.onsuccess = q.onerror = q.onblocked = done; });
        }
        for (const key of await caches.keys()) await caches.delete(key);
        """

    // MARK: - its button and pages

    /// Presses extension `id`'s button for the page in front.
    func performAction(_ id: String) -> Bool {
        guard let context = contexts[id], context.action(for: tabs.active) != nil else { return false }
        context.performAction(for: tabs.active)
        return true
    }

    func optionsURL(_ id: String) -> String? {
        contexts[id]?.optionsPageURL?.absoluteString
    }
}

// MARK: - WebKit asking the browser

extension Host: WKWebExtensionControllerDelegate {
    func webExtensionController(_ controller: WKWebExtensionController,
                                openWindowsFor extensionContext: WKWebExtensionContext) -> [any WKWebExtensionWindow] {
        [tabs.window]
    }

    func webExtensionController(_ controller: WKWebExtensionController,
                                focusedWindowFor extensionContext: WKWebExtensionContext) -> (any WKWebExtensionWindow)? {
        tabs.window
    }

    /// tabs.create: a browser tab, opened by Emacs, which answers with its view.
    func webExtensionController(_ controller: WKWebExtensionController,
                                openNewTabUsing configuration: WKWebExtension.TabConfiguration,
                                for extensionContext: WKWebExtensionContext) async throws -> (any WKWebExtensionTab)? {
        let url = configuration.url ?? URL(string: "about:blank")!
        try Rules.mayOpen(url)
        let address = await request("open", ["url": url.absoluteString, "active": configuration.shouldBeActive])
        return address.flatMap { tabs.tab(address: $0) }
    }

    /// windows.create: the browser has one window, Emacs's, so its pages
    /// open as tabs there.  A private window isn't something an extension
    /// can have here: refused, as Chrome refuses when incognito isn't allowed.
    func webExtensionController(_ controller: WKWebExtensionController,
                                openNewWindowUsing configuration: WKWebExtension.WindowConfiguration,
                                for extensionContext: WKWebExtensionContext) async throws -> (any WKWebExtensionWindow)? {
        if configuration.shouldBePrivate {
            throw NSError(domain: "lispm-desktop-webext", code: 2,
                          userInfo: [NSLocalizedDescriptionKey: "Private windows can't be opened by extensions."])
        }
        for url in configuration.tabURLs { try Rules.mayOpen(url) }
        for (index, url) in configuration.tabURLs.enumerated() {
            _ = await request("open", ["url": url.absoluteString, "active": index == 0 && configuration.shouldBeFocused])
        }
        return tabs.window
    }

    func webExtensionController(_ controller: WKWebExtensionController,
                                openOptionsPageFor extensionContext: WKWebExtensionContext) async throws {
        guard let url = extensionContext.optionsPageURL else { return }
        events.send("open", ["url": url.absoluteString, "active": true])
    }

    func webExtensionController(_ controller: WKWebExtensionController,
                                promptForPermissions permissions: Set<WKWebExtension.Permission>,
                                in tab: (any WKWebExtensionTab)?,
                                for extensionContext: WKWebExtensionContext) async -> (Set<WKWebExtension.Permission>, Date?) {
        let names = permissions.map(\.rawValue).sorted()
        return await ask("asks for more access", details: names, context: extensionContext) ? (permissions, nil) : ([], nil)
    }

    /// WebKit asks this the way Safari does: whenever an extension reaches
    /// for a page it has no host permission for — listing tabs, running a
    /// script in one — often with nobody having touched anything. Chrome
    /// never asks there: the extension has the sites its manifest named,
    /// the page it was clicked on (activeTab), and the ones it asked for
    /// through permissions.request. So neither does the browser here.
    func webExtensionController(_ controller: WKWebExtensionController,
                                promptForPermissionToAccess urls: Set<URL>,
                                in tab: (any WKWebExtensionTab)?,
                                for extensionContext: WKWebExtensionContext) async -> (Set<URL>, Date?) {
        ([], nil)
    }

    func webExtensionController(_ controller: WKWebExtensionController,
                                promptForPermissionMatchPatterns matchPatterns: Set<WKWebExtension.MatchPattern>,
                                in tab: (any WKWebExtensionTab)?,
                                for extensionContext: WKWebExtensionContext) async -> (Set<WKWebExtension.MatchPattern>, Date?) {
        // Extension pages and files are never given, so never asked about.
        let wanted = matchPatterns.filter { !Rules.withheld($0) }
        guard !wanted.isEmpty else { return ([], nil) }
        let all = wanted.contains { $0.matchesAllHosts || $0.matchesAllURLs }
        let what = all ? "every website" : wanted.map(\.string).sorted().joined(separator: ", ")
        guard await ask("wants to read and change \(what)", details: ["Until you remove the extension."],
                        context: extensionContext) else { return ([], nil) }
        // Given only once this returns: fenced again right after.
        DispatchQueue.main.async { MainActor.assumeIsolated { Rules.fence(extensionContext) } }
        return (wanted, nil)
    }

    func webExtensionController(_ controller: WKWebExtensionController, didUpdate action: WKWebExtension.Action,
                                forExtensionContext context: WKWebExtensionContext) {
        changed()
    }

    /// The popup page, in a popover of the module's own (Popup says why):
    /// WebKit's view is only asked which page it would show.
    func webExtensionController(_ controller: WKWebExtensionController, presentActionPopup action: WKWebExtension.Action,
                                for context: WKWebExtensionContext) async throws {
        let url = action.popupWebView?.url ?? Rules.popupURL(for: context)
        action.closePopup()
        guard let url else { return }
        Popup.shared.show(url, for: context, from: tabs.active?.view)
    }

    /// runtime.sendNativeMessage: no app on this Mac is spoken to.
    func webExtensionController(_ controller: WKWebExtensionController, sendMessage message: Any,
                                toApplicationWithIdentifier applicationIdentifier: String?,
                                for extensionContext: WKWebExtensionContext) async throws -> Any? {
        throw NSError(domain: "lispm-desktop-webext", code: 3,
                      userInfo: [NSLocalizedDescriptionKey: "Native messaging is not available in this browser."])
    }

    func webExtensionController(_ controller: WKWebExtensionController, connectUsing port: WKWebExtension.MessagePort,
                                for extensionContext: WKWebExtensionContext) async throws {
        throw NSError(domain: "lispm-desktop-webext", code: 3,
                      userInfo: [NSLocalizedDescriptionKey: "Native messaging is not available in this browser."])
    }
}

/// A page nobody sees, loaded only to run a script at an origin.
@MainActor
private final class HiddenPage: NSObject, WKNavigationDelegate {
    let web: WKWebView
    private var loaded: CheckedContinuation<Bool, Never>?

    init(_ configuration: WKWebViewConfiguration) {
        web = WKWebView(frame: .zero, configuration: configuration)
        super.init()
        web.navigationDelegate = self
    }

    /// Whether an empty page at `base` loaded within `limit` seconds.
    func loadBlank(at base: URL, within limit: TimeInterval = 10) async -> Bool {
        await withCheckedContinuation { done in
            loaded = done
            web.loadHTMLString("<!doctype html>", baseURL: base)
            DispatchQueue.main.asyncAfter(deadline: .now() + limit) { [weak self] in
                MainActor.assumeIsolated { self?.finish(false) }
            }
        }
    }

    private func finish(_ ok: Bool) {
        loaded?.resume(returning: ok)
        loaded = nil
    }

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) { finish(true) }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) { finish(false) }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        finish(false)
    }
}
