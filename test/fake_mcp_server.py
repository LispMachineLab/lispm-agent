#!/usr/bin/env python3
# fake_mcp_server.py --- a Model Context Protocol server for LispM's tests
#
# Copyright (C) 2026  Ryan.K
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Speaks MCP over stdio (newline-delimited JSON-RPC) with three tools:
# echo (text back, with the arguments as received), snap (a text and
# a PNG image) and fail (an error result).

import base64
import json
import struct
import sys
import zlib


def png(width, height, rgb):
    """A PNG of WIDTH x HEIGHT pixels of the colour RGB, base64-encoded."""
    def chunk(kind, data):
        return (struct.pack(">I", len(data)) + kind + data
                + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF))
    rows = b"".join(b"\x00" + bytes(rgb) * width for _ in range(height))
    data = (b"\x89PNG\r\n\x1a\n"
            + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(rows)) + chunk(b"IEND", b""))
    return base64.b64encode(data).decode()


PNG = png(2, 2, (255, 0, 0))

TOOLS = [
    {"name": "echo", "description": "Echo the text back.",
     "inputSchema": {"type": "object",
                     "properties": {"text": {"type": "string"},
                                    "loud": {"type": "boolean"}},
                     "required": ["text"]}},
    {"name": "snap", "description": "Take a picture.",
     "inputSchema": {"type": "object"}},
    {"name": "fail", "description": "Always fails.",
     "inputSchema": {"type": "object", "properties": {}}},
]


def send(message):
    sys.stdout.write(json.dumps(message) + "\n")
    sys.stdout.flush()


def call(name, args):
    if name == "echo":
        text = args["text"].upper() if args.get("loud") is True else args["text"]
        return {"content": [{"type": "text",
                             "text": text + " " + json.dumps(args, sort_keys=True)}]}
    if name == "snap":
        return {"content": [{"type": "text", "text": "Here it is."},
                            {"type": "image", "data": PNG, "mimeType": "image/png"}]}
    return {"content": [{"type": "text", "text": "it broke"}], "isError": True}


for line in sys.stdin:
    if not line.strip():
        continue
    message = json.loads(line)
    method, ident = message.get("method"), message.get("id")
    if ident is None:
        continue                # a notification
    if method == "initialize":
        result = {"protocolVersion": message["params"]["protocolVersion"],
                  "capabilities": {"tools": {}},
                  "serverInfo": {"name": "fake", "version": "1"}}
    elif method == "tools/list":
        result = {"tools": TOOLS}
    elif method == "tools/call":
        result = call(message["params"]["name"], message["params"].get("arguments") or {})
    else:
        send({"jsonrpc": "2.0", "id": ident,
              "error": {"code": -32601, "message": "Method not found"}})
        continue
    send({"jsonrpc": "2.0", "id": ident, "result": result})
