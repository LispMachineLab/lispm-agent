#!/usr/bin/env python3
# Copyright (C) 2026  Ryan.K
# SPDX-License-Identifier: AGPL-3.0-or-later
"""A fake Messages API endpoint for lispm's tests.

Prints the port it listens on, appends every request it receives (path,
headers, body) as a JSON line to the log file given as argument, and
answers in the shape asked for.  Messages requests (/v1/messages) get a
canned event stream, a whole JSON message when "stream" is false, or an
HTTP 529 error for the model name "overloaded".  Chat Completions
requests (/chat/completions) play a scripted agent turn, see chat_reply.
Streams are written in uneven chunks, one of them ending inside a
multibyte character, so clients must reassemble events and UTF-8.
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


def chat_reply(body):
    """The next step of a scripted agent turn in Chat Completions shape.

    Without tool results yet: some reasoning, a sentence and two parallel
    tool calls.  With them: a final answer quoting every result."""
    results = [m["content"] for m in body["messages"] if m.get("role") == "tool"]
    if results:
        return {"content": "Read: " + " | ".join(results), "finish": "stop", "calls": []}
    return {"reasoning": "Plan: read, then list.", "content": "Looking λ→∀.",
            "finish": "tool_calls",
            "calls": [("call_a", "read_file", '{"path": "a.txt"}'),
                      ("call_b", "list_directory", '{"path": "."}')]}


def chat_chunks(reply, usage):
    """REPLY as a Chat Completions event stream, tool arguments in fragments."""
    def chunk(delta, finish=None):
        return {"id": "chatcmpl-1", "object": "chat.completion.chunk", "model": "fake",
                "choices": [{"index": 0, "delta": delta, "finish_reason": finish}]}
    events = [chunk({"role": "assistant", "content": ""})]
    if "reasoning" in reply:
        half = len(reply["reasoning"]) // 2
        events += [chunk({"reasoning_content": reply["reasoning"][:half]}),
                   chunk({"reasoning_content": reply["reasoning"][half:]})]
    events += [chunk({"content": reply["content"][:4]}), chunk({"content": reply["content"][4:]})]
    for index, (call_id, name, arguments) in enumerate(reply["calls"]):
        events.append(chunk({"tool_calls": [{"index": index, "id": call_id, "type": "function",
                                             "function": {"name": name, "arguments": arguments[:5]}}]}))
        events.append(chunk({"tool_calls": [{"index": index,
                                             "function": {"arguments": arguments[5:]}}]}))
    events.append(chunk({}, reply["finish"]))
    if usage:
        events.append({"id": "chatcmpl-1", "object": "chat.completion.chunk", "choices": [],
                       "usage": {"prompt_tokens": 20, "completion_tokens": 7,
                                 "prompt_tokens_details": {"cached_tokens": 5}}})
    return b"".join(f"data: {json.dumps(e, ensure_ascii=False)}\n\n".encode() for e in events) \
        + b"data: [DONE]\n\n"


def chat_whole(reply):
    """REPLY as a Chat Completions response that is not streamed."""
    message = {"role": "assistant", "content": reply["content"],
               "tool_calls": [{"id": call_id, "type": "function",
                               "function": {"name": name, "arguments": arguments}}
                              for call_id, name, arguments in reply["calls"]] or None}
    if "reasoning" in reply:
        message["reasoning_content"] = reply["reasoning"]
    return {"id": "chatcmpl-2", "object": "chat.completion", "model": "fake",
            "choices": [{"index": 0, "message": message, "finish_reason": reply["finish"]}],
            "usage": {"prompt_tokens": 20, "completion_tokens": 7}}


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def send(self, status, content_type, data, chunked=False):
        self.send_response(status)
        self.send_header("content-type", content_type)
        if not chunked:
            self.send_header("content-length", str(len(data)))
        self.end_headers()
        if not chunked:
            self.wfile.write(data)
            return
        # Uneven chunks, one of them ending inside a multibyte character.
        cuts = set(range(37, len(data), 37))
        if "→".encode() in data:
            cuts.add(data.index("→".encode()) + 1)
        cuts = sorted(cuts)
        for start, end in zip([0] + cuts, cuts + [len(data)]):
            self.wfile.write(data[start:end])
            self.wfile.flush()
            time.sleep(0.002)

    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers["content-length"])))
        with open(LOG, "a") as log:
            log.write(json.dumps({"path": self.path,
                                  "headers": {k.lower(): v for k, v in self.headers.items()},
                                  "body": body}) + "\n")
        if self.path.endswith("/chat/completions"):
            reply = chat_reply(body)
            if body.get("stream"):
                usage = (body.get("stream_options") or {}).get("include_usage")
                self.send(200, "text/event-stream", chat_chunks(reply, usage), chunked=True)
            else:
                self.send(200, "application/json", json.dumps(chat_whole(reply)).encode())
            return
        if body.get("model") == "overloaded":
            self.send(529, "application/json", json.dumps(
                {"type": "error", "error": {"type": "overloaded_error", "message": "Overloaded"}}).encode())
        elif body.get("stream") is False:
            self.send(200, "application/json", json.dumps(
                {"id": "msg_2", "type": "message", "role": "assistant", "model": "fake",
                 "content": [{"type": "text", "text": "whole"},
                             {"type": "tool_use", "id": "c1", "name": "a", "input": {"x": 1}},
                             {"type": "tool_use", "id": "c2", "name": "b", "input": {}}],
                 "stop_reason": "tool_use",
                 "usage": {"input_tokens": 3, "output_tokens": 4}}).encode())
        else:
            self.send(200, "text/event-stream", b"".join(sse(e) for e in EVENTS), chunked=True)

server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
print(server.server_address[1], flush=True)
server.serve_forever()
