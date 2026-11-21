// @effect-diagnostics nodeBuiltinImport:off - a ZAP router is a unix socket; the fake speaks it for real.
import * as NodeFS from "node:fs";
import * as NodeNet from "node:net";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";

import {
  ERROR,
  HELLO,
  PROVIDERS,
  PROVIDERS_LIST,
  RESPONSE,
  ROUTE,
  WELCOME,
  encodeZapFrame,
} from "@hanzo/browser";
import * as Effect from "effect/Effect";

export interface FakeBrowser {
  readonly id: string;
  readonly caps: ReadonlyArray<string>;
}

export interface FakeCall {
  readonly to: string;
  readonly method: string;
  readonly params: Record<string, string>;
}

type Handle = (call: FakeCall) => unknown;

const str16 = (s: string) => {
  const b = Buffer.from(s, "utf8");
  const n = Buffer.alloc(2);
  n.writeUInt16LE(b.length, 0);
  return Buffer.concat([n, b]);
};

const u16 = (n: number) => {
  const b = Buffer.alloc(2);
  b.writeUInt16LE(n, 0);
  return b;
};

/** PROVIDERS body, as the router sends it: id, role, brand, caps, attrs per node. */
const providersBody = (browsers: ReadonlyArray<FakeBrowser>) =>
  Buffer.concat([
    u16(browsers.length),
    ...browsers.flatMap((b) => [
      str16(b.id),
      Buffer.from([1]),
      str16("hanzo"),
      u16(b.caps.length),
      ...b.caps.map(str16),
      u16(0),
    ]),
  ]);

/** The extension's command body: method, then params as key and u32-length value. */
const parseCommand = (payload: Buffer) => {
  let o = 0;
  const ml = payload.readUInt16LE(o);
  o += 2;
  const method = payload.toString("utf8", o, o + ml);
  o += ml;
  const n = payload.readUInt16LE(o);
  o += 2;
  const params: Record<string, string> = {};
  for (let i = 0; i < n; i++) {
    const kl = payload.readUInt16LE(o);
    o += 2;
    const key = payload.toString("utf8", o, o + kl);
    o += kl;
    const vl = payload.readUInt32LE(o);
    o += 4;
    params[key] = payload.toString("utf8", o, o + vl);
    o += vl;
  }
  return { method, params };
};

/**
 * A ZAP router on a unix socket, speaking HIP-0069 frames. `answer` returns what the extension
 * would; a thrown error reaches the caller as the extension's `ERR:` reply.
 */
export const fakeZapRouter = (browsers: ReadonlyArray<FakeBrowser>, answer: Handle) =>
  Effect.acquireRelease(
    Effect.promise(async () => {
      const dir = NodeFS.mkdtempSync(
        NodePath.join(NodeOS.tmpdir().startsWith("/var") ? "/tmp" : NodeOS.tmpdir(), "zr-"),
      );
      const socketPath = NodePath.join(dir, "zapd.sock");
      const calls: FakeCall[] = [];
      const server = NodeNet.createServer((socket) => {
        let buffered = Buffer.alloc(0);
        socket.on("error", () => socket.destroy());
        socket.on("data", (chunk: Buffer) => {
          buffered = Buffer.concat([buffered, chunk]);
          while (buffered.length >= 4 && buffered.length >= 4 + buffered.readUInt32LE(0)) {
            const frame = buffered.subarray(0, 4 + buffered.readUInt32LE(0));
            buffered = buffered.subarray(frame.length);
            const type = frame.readUInt8(4);
            const fromLen = frame.readUInt16LE(7);
            const toLen = frame.readUInt16LE(9);
            const payloadLen = frame.readUInt32LE(11);
            const from = frame.toString("utf8", 15, 15 + fromLen);
            const to = frame.toString("utf8", 15 + fromLen, 15 + fromLen + toLen);
            const payload = frame.subarray(15 + fromLen + toLen, 15 + fromLen + toLen + payloadLen);
            if (type === HELLO) socket.write(encodeZapFrame(WELCOME, "router", from));
            else if (type === PROVIDERS_LIST) {
              socket.write(encodeZapFrame(PROVIDERS, "router", from, providersBody(browsers)));
            } else if (type === ROUTE) {
              const command = parseCommand(payload);
              const call = { to, ...command };
              calls.push(call);
              try {
                socket.write(
                  encodeZapFrame(RESPONSE, to, from, Buffer.from(JSON.stringify(answer(call)))),
                );
              } catch (cause) {
                socket.write(
                  encodeZapFrame(
                    RESPONSE,
                    to,
                    from,
                    Buffer.from(`ERR:${cause instanceof Error ? cause.message : String(cause)}`),
                  ),
                );
              }
            } else socket.write(encodeZapFrame(ERROR, "router", from, Buffer.from("unexpected")));
          }
        });
      });
      await new Promise<void>((resolve) => server.listen(socketPath, resolve));
      return { dir, socketPath, server, calls };
    }),
    ({ dir, server }) =>
      Effect.promise(
        () =>
          new Promise<void>((resolve) =>
            server.close(() => {
              NodeFS.rmSync(dir, { recursive: true, force: true });
              resolve();
            }),
          ),
      ),
  );
