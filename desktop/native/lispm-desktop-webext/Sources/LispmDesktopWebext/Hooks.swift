// lispm-desktop-webext: the two places it reaches into Emacs's page views.
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Emacs on macOS makes each page as an XwWebView, its WKWebView subclass
// in src/nsxwidget.m, from a configuration it has just made.  WebKit reads
// the extension controller from that configuration once, as the view is
// made, so it can't be handed over later; and nothing tells a module when
// a page goes.  Both are learnt from the Objective-C runtime:
//
//   -[XwWebView initWithFrame:configuration:xwidget:]
//       the configuration is given the controller (Host.prepare), then the
//       view made from it becomes a tab (Host.adopt)
//   -[XwWebView removeFromSuperviewWithoutNeedingDisplay]
//       which only killing the xwidget sends it (nsxwidget_kill): the tab
//       is closed (Host.closed)

import AppKit
import ObjectiveC
import WebKit

struct HookError: LocalizedError {
    let errorDescription: String?
}

@MainActor
enum Hooks {
    private static var installed = false

    static func install() throws {
        guard !installed else { return }
        guard let view: AnyClass = NSClassFromString("XwWebView") else {
            throw HookError(errorDescription: "This Emacs makes no WebKit pages (built without xwidgets)")
        }
        try hookInit(of: view)
        hookRemoval(of: view)
        installed = true
    }

    private static func hookInit(of view: AnyClass) throws {
        let selector = NSSelectorFromString("initWithFrame:configuration:xwidget:")
        guard let method = class_getInstanceMethod(view, selector) else {
            throw HookError(errorDescription: "This Emacs's WebKit pages are not made as lispm-desktop-webext knows")
        }
        // Self and the result are passed through untouched: an init method
        // takes and returns ownership, which ARC must not see.
        typealias Init = @convention(c) (UnsafeMutableRawPointer, Selector, CGRect, WKWebViewConfiguration,
                                         UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer?
        let original = unsafeBitCast(method_getImplementation(method), to: Init.self)
        let replacement: @convention(block) (UnsafeMutableRawPointer, CGRect, WKWebViewConfiguration,
                                             UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer? = { this, frame, configuration, xwidget in
            let configuration = MainActor.assumeIsolated { Host.shared?.prepare(configuration) ?? configuration }
            let made = original(this, selector, frame, configuration, xwidget)
            if let made {
                MainActor.assumeIsolated {
                    Host.shared?.adopt(Unmanaged<WKWebView>.fromOpaque(made).takeUnretainedValue())
                }
            }
            return made
        }
        method_setImplementation(method, imp_implementationWithBlock(replacement))
    }

    private static func hookRemoval(of view: AnyClass) {
        let selector = #selector(NSView.removeFromSuperviewWithoutNeedingDisplay)
        typealias Remove = @convention(c) (UnsafeMutableRawPointer, Selector) -> Void
        // XwWebView has none of its own: one is added that tells the host,
        // then does what its superclass does.  Should a later Emacs give it
        // one, that one is wrapped instead.
        if let own = class_getInstanceMethod(view, selector),
           method_getImplementation(own) != class_getMethodImplementation(class_getSuperclass(view), selector) {
            let previous = unsafeBitCast(method_getImplementation(own), to: Remove.self)
            let wrapped: @convention(block) (UnsafeMutableRawPointer) -> Void = { this in
                MainActor.assumeIsolated { Host.shared?.closed(this) }
                previous(this, selector)
            }
            method_setImplementation(own, imp_implementationWithBlock(wrapped))
            return
        }
        guard let inherited = class_getMethodImplementation(class_getSuperclass(view), selector) else { return }
        let next = unsafeBitCast(inherited, to: Remove.self)
        let added: @convention(block) (UnsafeMutableRawPointer) -> Void = { this in
            MainActor.assumeIsolated { Host.shared?.closed(this) }
            next(this, selector)
        }
        class_addMethod(view, selector, imp_implementationWithBlock(added), "v@:")
    }
}
