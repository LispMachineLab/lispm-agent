#!/usr/bin/env python3
# Copyright (C) 2026  Ryan.K
# SPDX-License-Identifier: GPL-3.0-or-later
"""A fake Messages API endpoint for lispm's tests.

Prints the port it listens on, appends every request it receives (path,
headers, body) as a JSON line to the log file given as argument, and
answers with a canned event stream dribbled out in uneven chunks, so the
client has to reassemble events and split UTF-8 sequences.  A request
with "stream": false gets a whole JSON message; the model name
"overloaded" gets an HTTP 529 error.
"""
import json
import sys
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

LOG = sys.argv[1]


def sse(event):
    return f"event: {event['type']}\ndata: {json.dumps(event, ensure_ascii=False)}\n\n".encode()


EVENTS = [
    {"type": "message_start",
     "message": {"id": "msg_1", "type": "message", "role": "assistant", "content": [],
                 "model": "fake", "stop_reason": None, "usage": {"input_tokens": 12, "output_tokens": 0}}},
    {"type": "content_block_start", "index": 0, "content_block": {"type": "text", "text": ""}},
    {"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "Hello, "}},
    {"type": "ping"},
    {"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "λ→∀"}},
    {"type": "content_block_stop", "index": 0},
    {"type": "content_block_start", "index": 1,
     "content_block": {"type": "tool_use", "id": "toolu_1", "name": "read_file", "input": {}}},
    {"type": "content_block_delta", "index": 1, "delta": {"type": "input_json_delta", "partial_json": "{\"path\":"}},
    {"type": "content_block_delta", "index": 1, "delta": {"type": "input_json_delta", "partial_json": " \"a.txt\"}"}},
    {"type": "content_block_stop", "index": 1},
    {"type": "message_delta", "delta": {"stop_reason": "tool_use", "stop_sequence": None},
     "usage": {"output_tokens": 9}},
    {"type": "message_stop"},
]


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers["content-length"])))
        with open(LOG, "a") as log:
            log.write(json.dumps({"path": self.path,
                                  "headers": {k.lower(): v for k, v in self.headers.items()},
                                  "body": body}) + "\n")
        if body.get("model") == "overloaded":
            data = json.dumps({"type": "error",
                               "error": {"type": "overloaded_error", "message": "Overloaded"}}).encode()
            self.send_response(529)
            self.send_header("content-type", "application/json")
            self.send_header("content-length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
            return
        if body.get("stream") is False:
            data = json.dumps({"id": "msg_2", "type": "message", "role": "assistant", "model": "fake",
                               "content": [{"type": "text", "text": "whole"},
                                           {"type": "tool_use", "id": "c1", "name": "a", "input": {"x": 1}},
                                           {"type": "tool_use", "id": "c2", "name": "b", "input": {}}],
                               "stop_reason": "tool_use",
                               "usage": {"input_tokens": 3, "output_tokens": 4}}).encode()
            self.send_response(200)
            self.send_header("content-type", "application/json")
            self.send_header("content-length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
            return
        self.send_response(200)
        self.send_header("content-type", "text/event-stream")
        self.end_headers()
        stream = b"".join(sse(e) for e in EVENTS)
        # Uneven chunks, one of them ending inside a multibyte character.
        middle_of_arrow = stream.index("→".encode()) + 1
        cuts = sorted(set(range(37, len(stream), 37)) | {middle_of_arrow})
        for start, end in zip([0] + cuts, cuts + [len(stream)]):
            self.wfile.write(stream[start:end])
            self.wfile.flush()
            time.sleep(0.002)


server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
print(server.server_address[1], flush=True)
server.serve_forever()
