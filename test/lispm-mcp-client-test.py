#!/usr/bin/env python3
"""Exercise the stdio client against a real, local HTTP server."""

# Copyright (C) 2026 Ryan.K
# SPDX-License-Identifier: AGPL-3.0-or-later

import http.server
import json
import os
from pathlib import Path
import queue
import select
import signal
import socket
import subprocess
import tempfile
import threading
import time
import unittest


CLIENT = Path(__file__).resolve().parents[1] / "bin" / "lispm-mcp"


class ClientTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="lispm-mcp-test-")
        self.root = Path(self.temp.name)
        self.requests = []
        self.generation = 0
        self.blocked = threading.Event()
        self.disconnected = threading.Event()
        self.deleted = threading.Event()
        self.release = threading.Event()
        self.hold_delete = False
        self.token = "private-fixture-token"
        fixture = self

        class Handler(http.server.BaseHTTPRequestHandler):
            def log_message(self, *_args):
                pass

            def reply(self, status, message=None, session=None):
                data = json.dumps(message).encode() if message is not None else b""
                self.send_response(status)
                if session:
                    self.send_header("Mcp-Session-Id", session)
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                if data:
                    self.wfile.write(data)

            def await_disconnect(self):
                while not fixture.release.wait(0.01):
                    readable, _, _ = select.select([self.connection], [], [], 0.05)
                    if readable and not self.connection.recv(1, socket.MSG_PEEK):
                        fixture.disconnected.set()
                        return

            def do_POST(self):
                message = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
                fixture.requests.append((message, dict(self.headers)))
                session = f"session-{fixture.generation}"
                if self.headers.get("Authorization") != f"Bearer {fixture.token}":
                    self.reply(401)
                elif message["method"] == "initialize":
                    self.reply(200, {"jsonrpc": "2.0", "id": message["id"], "result": {}}, session)
                elif self.headers.get("Mcp-Session-Id") != session:
                    self.reply(404, {"jsonrpc": "2.0", "id": message.get("id"), "error": {}})
                elif message["method"] == "tools/call":
                    fixture.blocked.set()
                    self.await_disconnect()
                elif "id" not in message:
                    self.reply(202)
                else:
                    self.reply(200, {"jsonrpc": "2.0", "id": message["id"], "result": {}})

            def do_DELETE(self):
                fixture.requests.append(({"method": "DELETE"}, dict(self.headers)))
                fixture.deleted.set()
                if fixture.hold_delete:
                    self.await_disconnect()
                else:
                    self.reply(200)

        self.server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.server_thread = threading.Thread(
            target=self.server.serve_forever, kwargs={"poll_interval": 0.05}, daemon=True
        )
        self.server_thread.start()
        self.descriptor = self.root / "server"
        self.descriptor.write_text(
            f"http://127.0.0.1:{self.server.server_port}\n{self.token}\n"
        )
        self.process = subprocess.Popen(
            [str(CLIENT)],
            cwd=self.root,
            env={**os.environ, "LISPM_MCP_SERVER": str(self.descriptor),
                 "TMPDIR": str(self.root)},
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=True, start_new_session=True,
        )
        self.output = queue.Queue()
        self.output_thread = threading.Thread(target=self.collect_output, daemon=True)
        self.output_thread.start()

    def collect_output(self):
        for line in self.process.stdout:
            self.output.put(line)

    def tearDown(self):
        self.release.set()
        if self.process.poll() is None:
            os.killpg(self.process.pid, signal.SIGKILL)
            self.process.wait(timeout=3)
        for stream in (self.process.stdin, self.process.stdout, self.process.stderr):
            stream.close()
        self.server.shutdown()
        self.server.server_close()
        self.server_thread.join(timeout=1)
        self.output_thread.join(timeout=1)
        self.temp.cleanup()

    def send(self, method, request_id=None, **params):
        message = {"jsonrpc": "2.0", "method": method}
        if request_id is not None:
            message["id"] = request_id
        if params:
            message["params"] = params
        self.process.stdin.write(json.dumps(message) + "\n")
        self.process.stdin.flush()

    def response(self, request_id):
        self.assertEqual(json.loads(self.output.get(timeout=3))["id"], request_id)

    def initialize(self):
        self.send("initialize", 1, clientInfo={"name": "fixture"})
        self.response(1)
        self.send("notifications/initialized")

    def call_waiting_for_approval(self):
        self.initialize()
        self.send("tools/call", 2, name="waiting")
        self.assertTrue(self.blocked.wait(3))

    def assert_stopped(self, status=0):
        self.assertEqual(self.process.wait(timeout=4), status, self.process.stderr.read())
        self.output_thread.join(timeout=1)
        # The wrapper owns its process group; curl and the reader must be reaped.
        with self.assertRaises(ProcessLookupError):
            os.killpg(self.process.pid, 0)
        self.assertFalse(list(self.root.glob("lispm-mcp.*")))

    def test_eof_cancels_pending_call_even_with_queued_input(self):
        self.call_waiting_for_approval()
        # More input than a pipe holds must not stop the reader reaching EOF.
        for number in range(40):
            self.send("ping", 100 + number, padding="x" * 20000)
        self.process.stdin.close()
        self.assert_stopped()
        self.assertTrue(self.disconnected.wait(1))
        self.assertTrue(self.deleted.is_set())
        self.assertFalse(any(message["method"] == "ping" for message, _ in self.requests))

    def test_term_cancels_pending_call_and_reader(self):
        self.call_waiting_for_approval()
        self.process.send_signal(signal.SIGTERM)
        self.assert_stopped()
        self.assertTrue(self.disconnected.wait(1))
        self.assertTrue(self.deleted.is_set())

    def test_delete_has_a_deadline(self):
        self.initialize()
        self.hold_delete = True
        started = time.monotonic()
        self.process.stdin.close()
        self.assert_stopped()
        self.assertLess(time.monotonic() - started, 3.5)
        self.assertTrue(self.deleted.is_set())
        self.assertTrue(self.disconnected.wait(1))

    def test_complete_lines_and_restart_reinitialize(self):
        self.process.stdin.write('{"jsonrpc":"2.0","id":1,"method":')
        self.process.stdin.flush()
        time.sleep(0.1)
        self.assertEqual(self.requests, [])
        self.process.stdin.write('"initialize"}\n\n')
        self.process.stdin.flush()
        self.response(1)
        self.send("notifications/initialized")
        self.send("ping", 2)
        self.response(2)
        self.generation += 1
        self.send("ping", 3)
        self.response(3)
        self.process.stdin.close()
        self.assert_stopped()
        methods = [message["method"] for message, _ in self.requests]
        self.assertEqual(methods, ["initialize", "notifications/initialized", "ping",
                                  "ping", "initialize", "notifications/initialized",
                                  "ping", "DELETE"])
        for _, headers in self.requests:
            self.assertEqual(headers["Authorization"], f"Bearer {self.token}")
            self.assertEqual(headers["X-LispM-Directory"], str(self.root.resolve()))
        self.assertEqual(self.requests[-1][1]["Mcp-Session-Id"], "session-1")
        self.assertTrue(self.output.empty())

    def test_failure_stops_reader_with_stdin_open(self):
        self.descriptor.unlink()
        self.send("initialize", 1)
        self.assert_stopped(2)
        self.assertFalse(self.deleted.is_set())


if __name__ == "__main__":
    unittest.main()
