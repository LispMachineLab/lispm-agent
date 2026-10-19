// lispm-desktop-webext: tests of what extensions are given and told.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later

import Foundation
import WebKit
import XCTest
@testable import WebextCore

@MainActor
final class RulesTests: XCTestCase {
    private var folders: [URL] = []

    override func tearDown() async throws {
        for folder in folders { try? FileManager.default.removeItem(at: folder) }
        folders = []
    }

    /// An extension folder holding only `manifest`.
    private func extensionFolder(_ manifest: String) throws -> URL {
        let folder = FileManager.default.temporaryDirectory
            .appendingPathComponent("lispm-desktop-webext-test-\(UUID().uuidString)", isDirectory: true)
        try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
        try Data(manifest.utf8).write(to: folder.appendingPathComponent("manifest.json"))
        folders.append(folder)
        return folder
    }

    func testWhatItWillDoIsSaidAndWrittenDown() async throws {
        let folder = try extensionFolder("""
            {"manifest_version": 3, "name": "Blocker", "version": "1",
             "permissions": ["declarativeNetRequest", "tabs"],
             "host_permissions": ["<all_urls>"]}
            """)
        let found = try await WKWebExtension(resourceBaseURL: folder)
        XCTAssertEqual(Rules.describe(found, in: folder), [
            "Read and change everything on every website",
            "See your open tabs and their addresses",
            "Block or change requests pages make",
        ])
        let grants = Rules.grants(found, in: folder)
        XCTAssertTrue(grants.contains("declarativeNetRequest"))
        XCTAssertTrue(grants.contains("tabs"))
        XCTAssertTrue(grants.contains { $0.hasPrefix("site:") })
        XCTAssertEqual(grants, grants.sorted())
    }

    func testAFewSitesAreNamed() async throws {
        let folder = try extensionFolder("""
            {"manifest_version": 3, "name": "Sites", "version": "1",
             "host_permissions": ["https://example.com/*"]}
            """)
        let found = try await WKWebExtension(resourceBaseURL: folder)
        XCTAssertEqual(Rules.describe(found, in: folder), ["Read and change what's on example.com"])
    }

    func testWhatTheShimAddedIsNotSaid() async throws {
        let folder = try extensionFolder("""
            {"manifest_version": 3, "name": "Wallet", "version": "1",
             "permissions": ["storage", "notifications", "nativeMessaging"]}
            """)
        try Data(#"["nativeMessaging"]"#.utf8).write(to: folder.appendingPathComponent(".search-added"))
        let found = try await WKWebExtension(resourceBaseURL: folder)
        XCTAssertEqual(Rules.describe(found, in: folder), ["Show notifications"])
        let grants = Rules.grants(found, in: folder)
        XCTAssertFalse(grants.contains("nativeMessaging"))
        XCTAssertTrue(grants.contains("shim:notifications"))
    }

    func testExtensionPagesAndFilesAreWithheld() throws {
        WKWebExtension.MatchPattern.registerCustomURLScheme(Rules.scheme)
        XCTAssertTrue(Rules.withheld(try WKWebExtension.MatchPattern(string: "chrome-extension://*/*")))
        XCTAssertTrue(Rules.withheld(try WKWebExtension.MatchPattern(string: "file:///*")))
        XCTAssertFalse(Rules.withheld(try WKWebExtension.MatchPattern(string: "https://*/*")))
    }

    func testNoTabIsSentToAScriptOrAFile() {
        XCTAssertThrowsError(try Rules.mayOpen(URL(string: "javascript:alert(1)")!))
        XCTAssertThrowsError(try Rules.mayOpen(URL(string: "file:///etc/passwd")!))
        XCTAssertNoThrow(try Rules.mayOpen(URL(string: "https://example.com/")!))
        XCTAssertNoThrow(try Rules.mayOpen(URL(string: "chrome-extension://abc/page.html")!))
    }

    func testTheUserAgentNamesASafari() {
        XCTAssertTrue(Rules.userAgentName.hasPrefix("Version/"))
        XCTAssertTrue(Rules.userAgentName.hasSuffix(" Safari/605.1.15"))
    }
}
