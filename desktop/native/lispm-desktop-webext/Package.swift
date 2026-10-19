// swift-tools-version: 6.0
//
// lispm-desktop-webext: Chrome extensions in lispm-desktop's browser, on
// macOS, as an Emacs dynamic module.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later

import PackageDescription

let package = Package(
    name: "lispm-desktop-webext",
    // WKWebExtension, WebKit's extension engine, came with macOS 15.4.
    platforms: [.macOS("15.4")],
    products: [
        // liblispm_desktop_webext.dylib, which lispm-desktop-browser-extensions.el loads.
        .library(name: "lispm_desktop_webext", type: .dynamic, targets: ["LispmDesktopWebext"]),
    ],
    dependencies: [
        // The community's Swift binding of Emacs's module API.
        .package(url: "https://github.com/SavchenkoValeriy/emacs-swift-module.git", exact: "1.3.5"),
    ],
    targets: [
        // What needs no Emacs: Chrome Web Store packages, ids and words.
        .target(
            name: "WebextCore",
            path: "Sources/WebextCore",
            // WebKit's and AppKit's objects belong to the main thread, which
            // is also the thread Emacs runs Lisp on; Swift 6's strict
            // isolation would only restate that.
            swiftSettings: [.swiftLanguageMode(.v5)]
        ),
        .target(
            name: "LispmDesktopWebext",
            dependencies: [
                "WebextCore",
                .product(name: "EmacsSwiftModule", package: "emacs-swift-module"),
            ],
            path: "Sources/LispmDesktopWebext",
            swiftSettings: [.swiftLanguageMode(.v5)],
            plugins: [.plugin(name: "ModuleFactoryPlugin", package: "emacs-swift-module")]
        ),
        .testTarget(
            name: "WebextCoreTests",
            dependencies: ["WebextCore"],
            path: "Tests/WebextCoreTests",
            swiftSettings: [.swiftLanguageMode(.v5)]
        ),
    ]
)
