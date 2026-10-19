// lispm-desktop-webext: tests of what needs no Emacs.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later

import Foundation
import XCTest
@testable import WebextCore

final class CrxTests: XCTestCase {
    /// uBlock Origin Lite's id.
    let id = "ddkjiahejlhfcafbddmgiahcphecmpfh"

    func testIdIsFoundInStoreLinksAndAlone() {
        XCTAssertEqual(Crx.id(in: id), id)
        XCTAssertEqual(Crx.id(in: "https://chromewebstore.google.com/detail/ublock-origin-lite/\(id)"), id)
        XCTAssertEqual(Crx.id(in: "https://chrome.google.com/webstore/detail/\(id.uppercased())?hl=en"), id)
        XCTAssertNil(Crx.id(in: "https://example.com/"))
        // Thirty-three letters are not an id.
        XCTAssertNil(Crx.id(in: id + "a"))
    }

    func testStoreIDOnlyOnTheStoreOverHTTPS() {
        let page = "chromewebstore.google.com/detail/ublock-origin-lite/\(id)/reviews"
        XCTAssertEqual(Crx.storeID(of: URL(string: "https://" + page)!), id)
        XCTAssertNil(Crx.storeID(of: URL(string: "http://" + page)!))
        XCTAssertNil(Crx.storeID(of: URL(string: "https://evil.example/detail/\(id)")!))
        XCTAssertNil(Crx.storeID(of: URL(string: "https://user@" + page)!))
    }

    func testLettersAreHalfBytesFromAToP() {
        XCTAssertEqual(Crx.letters([0x00, 0x1f, 0xff]), "aabppp")
    }

    func testUnpackedIDFollowsTheRealPath() throws {
        let base = FileManager.default.temporaryDirectory
            .appendingPathComponent("lispm-desktop-webext-test-\(UUID().uuidString)", isDirectory: true)
        let folder = base.appendingPathComponent("extension", isDirectory: true)
        try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: base) }
        let link = base.appendingPathComponent("link")
        try FileManager.default.createSymbolicLink(at: link, withDestinationURL: folder)
        let found = Crx.unpackedID(for: folder)
        XCTAssertEqual(found.count, 32)
        XCTAssertTrue(found.allSatisfy { ("a"..."p").contains($0) })
        XCTAssertEqual(Crx.unpackedID(for: link), found)
    }

    func testAnythingButASignedCRX3IsRefused() {
        XCTAssertThrowsError(try Crx.verifiedZip(Data("PK\u{3}\u{4}".utf8), id: id))
        var header = Data("Cr24".utf8)
        header.append(contentsOf: [3, 0, 0, 0, 0, 0, 0, 0])
        XCTAssertThrowsError(try Crx.verifiedZip(header + Data("PK".utf8), id: id))
    }

    func testDownloadAsksForCRX3() {
        let url = Crx.downloadURL(for: id).absoluteString
        XCTAssertTrue(url.hasPrefix("https://clients2.google.com/service/update2/crx?"))
        XCTAssertTrue(url.contains("acceptformat=crx3"))
        XCTAssertTrue(url.contains(id))
    }
}

final class FilesTests: XCTestCase {
    func testLinksLeadingOutAreTakenOutAndInsideKept() throws {
        let base = FileManager.default.temporaryDirectory
            .appendingPathComponent("lispm-desktop-webext-test-\(UUID().uuidString)", isDirectory: true)
        let root = base.appendingPathComponent("extension", isDirectory: true)
        try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: base) }
        try Data("{}".utf8).write(to: root.appendingPathComponent("manifest.json"))
        try Data("secret".utf8).write(to: base.appendingPathComponent("outside"))
        try FileManager.default.createSymbolicLink(at: root.appendingPathComponent("in"),
                                                   withDestinationURL: root.appendingPathComponent("manifest.json"))
        try FileManager.default.createSymbolicLink(at: root.appendingPathComponent("out"),
                                                   withDestinationURL: base.appendingPathComponent("outside"))
        try ExtensionFiles.unlinkOutside(root)
        XCTAssertNotNil(try? FileManager.default.destinationOfSymbolicLink(atPath: root.appendingPathComponent("in").path))
        XCTAssertNil(try? FileManager.default.destinationOfSymbolicLink(atPath: root.appendingPathComponent("out").path))
    }

    func testReplaceSwapsAnExistingFolder() throws {
        let base = FileManager.default.temporaryDirectory
            .appendingPathComponent("lispm-desktop-webext-test-\(UUID().uuidString)", isDirectory: true)
        let staged = base.appendingPathComponent("staged", isDirectory: true)
        let target = base.appendingPathComponent("target", isDirectory: true)
        try FileManager.default.createDirectory(at: staged, withIntermediateDirectories: true)
        try FileManager.default.createDirectory(at: target, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: base) }
        try Data("new".utf8).write(to: staged.appendingPathComponent("v"))
        try Data("old".utf8).write(to: target.appendingPathComponent("v"))
        try ExtensionFiles.replace(staged, at: target)
        XCTAssertEqual(try String(contentsOf: target.appendingPathComponent("v"), encoding: .utf8), "new")
        // The old one is where the staged one was, for the caller to remove.
        XCTAssertEqual(try String(contentsOf: staged.appendingPathComponent("v"), encoding: .utf8), "old")
    }
}
