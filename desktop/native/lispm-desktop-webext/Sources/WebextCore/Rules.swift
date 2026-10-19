// What an extension is given, what it may reach, and how it is described.
//
// From Search (https://github.com/driceroland/Search) at
// 6294b6aed35299c7ab91399a4b2374dd726b1f39: `Installed`, the rules on extension
// pages and files, `mayOpen`, `grants`, `describe` and `popupURL` are from
// Sources/Search/Extensions.swift, `userAgentName` from Sources/Search/Tab.swift.
// What Search answers itself (bookmarks, history and the other APIs WebKit
// lacks) is left out, as lispm-desktop-webext doesn't answer them.
//
// Copyright (c) 2026 Office Commun
// SPDX-License-Identifier: MIT
// (The license is in LICENSE.Search, beside Package.swift.)

import Foundation
import WebKit

/// One installed extension, as installed.json lists it.
public struct Installed: Codable, Equatable {
    /// The Chrome Web Store id, or for one loaded from a folder the id
    /// Chrome would give it.
    public let id: String
    public var name: String
    public var version: String
    public var enabled: Bool
    public var fromStore: Bool
    /// The permissions it was installed with, so an update that asks for more
    /// is asked about rather than slipped through.
    public var permissions: [String]
    /// For one loaded from a folder: where that folder is, so Reload can
    /// bring the author's latest edits in. Kept with its links resolved:
    /// the path its id was worked out from.
    public var source: String?

    public init(id: String, name: String, version: String, enabled: Bool, fromStore: Bool,
                permissions: [String], source: String? = nil) {
        self.id = id
        self.name = name
        self.version = version
        self.enabled = enabled
        self.fromStore = fromStore
        self.permissions = permissions
        self.source = source
    }

    /// Whether Reload may copy from `source` under this id: a link put on
    /// the way since would bring in another folder, which has another id.
    public var sourceKeepsID: Bool {
        guard let source, !fromStore else { return true }
        return Crx.unpackedID(for: URL(fileURLWithPath: source, isDirectory: true)) == id
    }
}

@MainActor
public enum Rules {
    /// An extension's pages are served from chrome-extension://<id>/, the
    /// address they have in Chrome, with the ids Chrome gives. Servers allow
    /// their own extension in by that origin, and sites look for an
    /// extension at it.
    public static let scheme = "chrome-extension"
    /// WebKit's own scheme for extension pages.
    public static let webkitScheme = "webkit-extension"

    /// The application name every page view and extension view reports, so
    /// that they all report one user agent: WebKit gives workers the user
    /// agent of the last page that loaded and, when it differs, stops the
    /// running workers to apply it, and extension workers it then never
    /// starts again.
    ///
    /// The version is the Safari this Mac has, since its WebKit is the one
    /// every page runs on.
    public static let userAgentName = "Version/\(safariVersion) Safari/605.1.15"

    private static var safariVersion: String {
        for path in ["/System/Cryptexes/App/System/Applications/Safari.app", "/Applications/Safari.app"] {
            if let version = Bundle(path: path)?.infoDictionary?["CFBundleShortVersionString"] as? String {
                return version
            }
        }
        // Safari can't be read: say the Safari this macOS shipped with, the
        // oldest its WebKit can be. From macOS 26 Safari shares its number;
        // before, it was 3 ahead (14 -> 17, 15 -> 18).
        let os = ProcessInfo.processInfo.operatingSystemVersion.majorVersion
        return os >= 26 ? "\(os).0" : "\(os + 3).0"
    }

    /// Extension pages, as a pattern names them: another extension's
    /// (chrome-extension://<its id>/*) or every one's. No extension is ever
    /// given them — not from its manifest, not asked for later — whatever
    /// the question said; its own pages are its own without asking.
    public static func reachesExtensions(_ pattern: WKWebExtension.MatchPattern) -> Bool {
        guard let scheme = pattern.scheme?.lowercased() else { return false }
        return [Rules.scheme, Rules.webkitScheme].contains(scheme)
    }

    /// Files on this Mac aren't an extension's to reach either, whatever its
    /// manifest names: Chrome keeps them from every extension until "Allow
    /// access to file URLs" is turned on for it, and there is no such switch
    /// here. WebKit's <all_urls> leaves file: out; a pattern naming it
    /// (file:///*) would be granted with the rest.
    public static func reachesFiles(_ pattern: WKWebExtension.MatchPattern) -> Bool {
        pattern.scheme?.lowercased() == "file"
    }

    /// What no extension is given or asked about: extension pages and files.
    public static func withheld(_ pattern: WKWebExtension.MatchPattern) -> Bool {
        reachesExtensions(pattern) || reachesFiles(pattern)
    }

    /// The sites its manifest names, granted as it loads: never one for
    /// extension pages, another's or all of them, nor for files on this Mac
    /// (see `fence`, which is set after).
    public static func grantSites(_ context: WKWebExtensionContext) {
        for pattern in context.webExtension.allRequestedMatchPatterns where !withheld(pattern) {
            context.setPermissionStatus(.grantedExplicitly, for: pattern)
        }
    }

    /// Other extensions' pages are never among "all sites" either: with
    /// chrome-extension registered as a scheme, WebKit counts them in
    /// <all_urls>, which Chrome doesn't, so they are refused outright. A
    /// refusal for all hosts doesn't outweigh a grant naming one — WebKit
    /// looks at those first — which is why none is ever made (see
    /// `reachesExtensions`). Set again after anything is granted.
    public static func fence(_ context: WKWebExtensionContext) {
        for scheme in [Rules.scheme, Rules.webkitScheme] {
            if let pages = try? WKWebExtension.MatchPattern(string: "\(scheme)://*/*") {
                context.setPermissionStatus(.deniedExplicitly, for: pages)
            }
        }
        // Nor files (see `reachesFiles`).
        for string in ["file:///*", "file://*/*"] {
            if let files = try? WKWebExtension.MatchPattern(string: string) {
                context.setPermissionStatus(.deniedExplicitly, for: files)
            }
        }
    }

    /// Whether this address is a page of an extension other than the one
    /// asking — which it never gets to see into.
    public static func othersPage(_ url: URL?, for context: WKWebExtensionContext) -> Bool {
        guard let url, let scheme = url.scheme?.lowercased(), [Rules.scheme, Rules.webkitScheme].contains(scheme)
        else { return false }
        return url.host()?.lowercased() != context.uniqueIdentifier.lowercased()
    }

    /// Where an extension may send a tab. Not to javascript:, which would run
    /// its code in whatever page the tab shows — an extension with no access
    /// to that site at all — nor to a file on this Mac. Chrome refuses both.
    public nonisolated static func mayOpen(_ url: URL) throws {
        let scheme = url.scheme?.lowercased() ?? ""
        guard scheme != "javascript", scheme != "file" else {
            throw NSError(domain: "lispm-desktop-webext", code: 1,
                          userInfo: [NSLocalizedDescriptionKey: "Cannot navigate to a \(scheme): URL."])
        }
    }

    /// What the shim added to a prepared extension's manifest (its
    /// .search-added, see ExtensionShims.prepare), which the extension
    /// never asked for.
    static func added(in folder: URL) -> Set<String> {
        Set((try? JSONSerialization.jsonObject(with: Data(contentsOf: folder.appendingPathComponent(".search-added")))) as? [String] ?? [])
    }

    /// The permissions its manifest names.
    static func declared(in folder: URL) -> [String] {
        ((try? JSONSerialization.jsonObject(with: Data(contentsOf: folder.appendingPathComponent("manifest.json")))) as? [String: Any])?["permissions"] as? [String] ?? []
    }

    /// Chrome's own APIs, which the shim answers itself, and what each lets
    /// an extension do.
    public static let shimAnswered: [(String, String)] = [
        ("userScripts", "Run scripts you add to it on websites"),
        ("privacy", "Change your privacy settings"),
        ("browsingData", "Clear your browsing data"),
        ("notifications", "Show notifications"),
    ]

    /// What an extension was allowed, as it is written down and compared on
    /// every update: WebKit's permissions, the sites it reaches, and the
    /// APIs the shim answers for it — an update that adds any of them is
    /// asked about again. What the shim added to its manifest is left out.
    public static func grants(_ found: WKWebExtension, in folder: URL) -> [String] {
        let added = added(in: folder)
        let ours = Set(shimAnswered.map(\.0))
        var out = Set(found.requestedPermissions.map(\.rawValue).filter { !added.contains($0) })
        out.formUnion(found.allRequestedMatchPatterns.map { "site:" + $0.string })
        out.formUnion(declared(in: folder).filter { ours.contains($0) && !added.contains($0) }.map { "shim:" + $0 })
        return out.sorted()
    }

    /// What an extension wants, in words, leaving out what the shim added
    /// to its manifest.
    public static func describe(_ found: WKWebExtension, in folder: URL) -> [String] {
        var out: [String] = []
        let added = added(in: folder)
        let declared = Set(declared(in: folder))
        let patterns = found.allRequestedMatchPatterns
        if patterns.contains(where: { $0.matchesAllHosts || $0.matchesAllURLs }) {
            out.append("Read and change everything on every website")
        } else if !patterns.isEmpty {
            let hosts = patterns.compactMap(\.host).filter { !$0.isEmpty }
            out.append("Read and change what's on " + hosts.prefix(4).joined(separator: ", ")
                + (hosts.count > 4 ? " and \(hosts.count - 4) more" : ""))
        }
        let words: [(WKWebExtension.Permission, String)] = [
            (.tabs, "See your open tabs and their addresses"),
            (.cookies, "Read and change cookies"),
            (.webNavigation, "See where you go"),
            (.webRequest, "See the requests pages make"),
            (.declarativeNetRequest, "Block or change requests pages make"),
            (.clipboardWrite, "Write to the clipboard"),
            (.nativeMessaging, "Talk to apps on this Mac"),
            (.scripting, "Run scripts in pages"),
        ]
        for (permission, sentence) in words
        where found.requestedPermissions.contains(permission) && !added.contains(permission.rawValue) {
            out.append(sentence)
        }
        for (name, sentence) in shimAnswered where declared.contains(name) && !added.contains(name) {
            out.append(sentence)
        }
        return out
    }

    /// The page the manifest names for the button, when WebKit hasn't said.
    public static func popupURL(for context: WKWebExtensionContext) -> URL? {
        let manifest = context.webExtension.manifest
        let action = (manifest["action"] ?? manifest["browser_action"]) as? [String: Any]
        guard let path = action?["default_popup"] as? String, !path.isEmpty else { return nil }
        // Relative to the extension, and keeping a query it may carry.
        return URL(string: path, relativeTo: context.baseURL)?.absoluteURL
    }
}
