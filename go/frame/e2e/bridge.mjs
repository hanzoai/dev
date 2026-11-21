// bridge.mjs fronts a real Hanzo Dev web client with the Go adapter.
//
//   node bridge.mjs <framed binary> <port> <dev server url> [framed flags...]
//
// Everything but the socket goes to a running Dev server, which serves the
// client and its sign-in. The socket at /ws is a framed process, so the client
// that loads speaks to the dev loop and never to the Dev server's own.
import http from "node:http";
import crypto from "node:crypto";
import { spawn } from "node:child_process";

const [framed, port, upstream, ...flags] = process.argv.slice(2);
const target = new URL(upstream);

// The three questions a client asks before it opens the stream are the
// adapter's to answer, as they are a ZAP session's (frame.Method*).
const calls = { "/.well-known/hanzo/environment": 1, "/v1/auth/session": 2, "/v1/event/shell": 3 };

const server = http.createServer((req, res) => {
  const method = calls[new URL(req.url, "http://x").pathname];
  if (method) {
    const child = spawn(framed, ["-call", String(method), ...flags], { stdio: ["ignore", "pipe", "inherit"] });
    const chunks = [];
    child.stdout.on("data", (d) => chunks.push(d));
    child.on("close", () => res.writeHead(200, { "content-type": "application/json" }).end(Buffer.concat(chunks)));
    return;
  }
  const up = http.request({ host: target.hostname, port: target.port, path: req.url, method: req.method, headers: req.headers }, (r) => {
    res.writeHead(r.statusCode, r.headers);
    r.pipe(res);
  });
  up.on("error", () => res.writeHead(502).end());
  req.pipe(up);
});

// A WebSocket frame of text, as a server writes it: unmasked, never split.
function text(s) {
  const b = Buffer.from(s);
  const h = b.length < 126 ? Buffer.from([0x81, b.length]) : b.length < 65536
    ? Buffer.from([0x81, 126, b.length >> 8, b.length & 255])
    : Buffer.concat([Buffer.from([0x81, 127]), Buffer.alloc(8)]);
  if (b.length >= 65536) h.writeBigUInt64BE(BigInt(b.length), 2);
  return Buffer.concat([h, b]);
}

server.on("upgrade", (req, socket) => {
  const url = new URL(req.url, "http://x");
  if (url.pathname !== "/ws") return socket.destroy();
  const accept = crypto.createHash("sha1").update(req.headers["sec-websocket-key"] + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").digest("base64");
  socket.write(`HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${accept}\r\n\r\n`);
  const child = spawn(framed, ["-query", url.search.slice(1), ...flags], { stdio: ["pipe", "pipe", "inherit"] });
  let out = Buffer.alloc(0);
  child.stdout.on("data", (d) => {
    out = Buffer.concat([out, d]);
    while (out.length >= 4 && out.length >= 4 + out.readUInt32LE(0)) {
      const n = out.readUInt32LE(0);
      socket.write(text(out.subarray(4, 4 + n).toString("utf8")));
      out = out.subarray(4 + n);
    }
  });
  let inb = Buffer.alloc(0);
  socket.on("data", (d) => {
    inb = Buffer.concat([inb, d]);
    for (;;) {
      if (inb.length < 2) return;
      const op = inb[0] & 15;
      let len = inb[1] & 127, at = 2;
      if (len === 126) { if (inb.length < 4) return; len = inb.readUInt16BE(2); at = 4; }
      else if (len === 127) { if (inb.length < 10) return; len = Number(inb.readBigUInt64BE(2)); at = 10; }
      if (inb.length < at + 4 + len) return;
      const mask = inb.subarray(at, at + 4);
      const body = Buffer.from(inb.subarray(at + 4, at + 4 + len));
      for (let i = 0; i < body.length; i++) body[i] ^= mask[i & 3];
      inb = inb.subarray(at + 4 + len);
      if (op === 8) return socket.end();
      if (op === 9) { socket.write(Buffer.concat([Buffer.from([0x8a, body.length]), body])); continue; }
      if (op !== 1) continue;
      const h = Buffer.alloc(4);
      h.writeUInt32LE(body.length);
      child.stdin.write(Buffer.concat([h, body]));
    }
  });
  socket.on("close", () => child.kill());
  socket.on("error", () => child.kill());
  child.on("exit", () => socket.end());
});
server.listen(Number(port), "127.0.0.1", () => console.log("bridge on", port));
