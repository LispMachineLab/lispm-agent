// An extension's folder: put in place whole, and kept inside itself.
//
// From Search (https://github.com/driceroland/Search) at
// 6294b6aed35299c7ab91399a4b2374dd726b1f39: `replace` is
// Sources/Search/ExtensionFiles.swift, `unlinkOutside` is
// Extensions.unlinkOutside in Sources/Search/Extensions.swift.
//
// Copyright (c) 2026 Office Commun
// SPDX-License-Identifier: MIT
// (The license is in LICENSE.Search, beside Package.swift.)

import Darwin
import Foundation

public enum ExtensionFiles {
    /// On success, an existing target is atomically swapped onto `staged`;
    /// callers own cleaning up that old folder after releasing its consumers.
    public static func replace(_ staged: URL, at target: URL) throws {
        if let error = renameError(staged, target, flags: RENAME_EXCL) {
            guard error == EEXIST else { throw replacementError(error, staged: staged, target: target) }
            if let error = renameError(staged, target, flags: RENAME_SWAP) {
                throw replacementError(error, staged: staged, target: target)
            }
        }
    }

    /// A nil result means the rename worked. On failure, errno is read before
    /// doing anything else so the error describes the syscall that failed.
    private static func renameError(_ staged: URL, _ target: URL, flags: Int32) -> Int32? {
        staged.withUnsafeFileSystemRepresentation { source in
            target.withUnsafeFileSystemRepresentation { destination in
                guard let source, let destination else { return EINVAL }
                guard renamex_np(source, destination, UInt32(flags)) != 0 else { return nil }
                let error = errno
                return error
            }
        }
    }

    private static func replacementError(_ code: Int32, staged: URL, target: URL) -> NSError {
        let posix = NSError(domain: NSPOSIXErrorDomain, code: Int(code))
        return NSError(
            domain: NSPOSIXErrorDomain,
            code: Int(code),
            userInfo: [
                NSLocalizedDescriptionKey: "Couldn't replace extension at \(target.path): \(posix.localizedDescription)",
                NSFilePathErrorKey: target.path,
                "ExtensionStagedPath": staged.path,
            ]
        )
    }

    /// One loaded from a folder is copied in links and all, and WebKit,
    /// serving its files, goes where a link points. So a link stays only if
    /// what it names is there and inside the package; one leading anywhere
    /// else, or nowhere, is taken out. (A package from the store with any
    /// link in it is refused whole: see Crx.) Throws if one couldn't be
    /// taken out.
    public static func unlinkOutside(_ root: URL) throws {
        guard let base = realpath(root.path, nil) else { return }
        let inside = String(cString: base) + "/"
        free(base)
        let files = FileManager.default
        let found = files.enumerator(at: root, includingPropertiesForKeys: [.isSymbolicLinkKey])
        while let item = found?.nextObject() as? URL {
            guard (try? item.resourceValues(forKeys: [.isSymbolicLinkKey]).isSymbolicLink) == true else { continue }
            var kept = false
            if let real = realpath(item.path, nil) {
                kept = String(cString: real).hasPrefix(inside)
                free(real)
            }
            if !kept { try files.removeItem(at: item) }
        }
    }
}
