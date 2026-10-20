// capture-window.swift --- Record one window of a process, until interrupted
//
// Copyright (C) 2026  Ryan.K
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Usage: capture-window PID OUTPUT X Y WIDTH HEIGHT
//
// Records the largest window of process PID to the movie OUTPUT: the
// rectangle X Y WIDTH HEIGHT of it, in points from its top left corner,
// as the window server draws the window, whatever covers it on screen,
// so nothing else on the screen is ever recorded.  Prints "started"
// once recording starts; on SIGINT, finishes the movie and exits.
// demo/record.sh builds it with swiftc; it needs Screen Recording access.

import AVFoundation
import AppKit
import ScreenCaptureKit

func fail(_ message: String) -> Never {
    FileHandle.standardError.write(Data("capture-window: \(message)\n".utf8))
    exit(1)
}

final class Recorder: NSObject, SCRecordingOutputDelegate {
    var stream: SCStream?
    var output: SCRecordingOutput?

    func recordingOutputDidStartRecording(_ recordingOutput: SCRecordingOutput) {
        print("started")
        fflush(stdout)
    }

    func recordingOutputDidFinishRecording(_ recordingOutput: SCRecordingOutput) {
        exit(0)
    }

    func recordingOutput(_ recordingOutput: SCRecordingOutput, didFailWithError error: Error) {
        fail("\(error)")
    }

    func stop() {
        guard let stream = stream, let output = output else { exit(0) }
        // Removing the output finishes the movie; the delegate then exits.
        do { try stream.removeRecordingOutput(output) } catch { fail("\(error)") }
    }
}

let arguments = CommandLine.arguments
guard arguments.count == 7, let pid = pid_t(arguments[1]),
      let x = Double(arguments[3]), let y = Double(arguments[4]),
      let width = Double(arguments[5]), let height = Double(arguments[6])
else { fail("usage: capture-window PID OUTPUT X Y WIDTH HEIGHT") }
let movie = URL(fileURLWithPath: arguments[2])
let recorder = Recorder()
// Recording puts an indicator in the menu bar, which takes an application
// running on the main thread; it shows no window and no Dock icon.
let application = NSApplication.shared
application.setActivationPolicy(.accessory)

signal(SIGINT, SIG_IGN)
let interrupt = DispatchSource.makeSignalSource(signal: SIGINT, queue: .main)
interrupt.setEventHandler { recorder.stop() }
interrupt.resume()

Task {
    do {
        let content = try await SCShareableContent.excludingDesktopWindows(
            true, onScreenWindowsOnly: true)
        guard let window = content.windows
            .filter({ $0.owningApplication?.processID == pid && $0.windowLayer == 0 })
            .max(by: { $0.frame.width * $0.frame.height < $1.frame.width * $1.frame.height })
        else { fail("no window of process \(pid)") }
        let filter = SCContentFilter(desktopIndependentWindow: window)
        let scale = Double(filter.pointPixelScale)
        let configuration = SCStreamConfiguration()
        configuration.sourceRect = CGRect(x: x, y: y, width: width, height: height)
        configuration.width = Int(width * scale)
        configuration.height = Int(height * scale)
        configuration.minimumFrameInterval = CMTime(value: 1, timescale: 30)
        configuration.showsCursor = false
        configuration.ignoreShadowsSingleWindow = true
        let stream = SCStream(filter: filter, configuration: configuration, delegate: nil)
        let settings = SCRecordingOutputConfiguration()
        settings.outputURL = movie
        settings.outputFileType = .mov
        settings.videoCodecType = .h264
        let output = SCRecordingOutput(configuration: settings, delegate: recorder)
        try stream.addRecordingOutput(output)
        recorder.stream = stream
        recorder.output = output
        try await stream.startCapture()
    } catch {
        fail("\(error)")
    }
}
application.run()
