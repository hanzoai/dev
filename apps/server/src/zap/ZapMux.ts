// @effect-diagnostics nodeBuiltinImport:off -- The mux rides a descriptor the daemon passed to this process.
import * as NodeNet from "node:net";

import type { ZapStream } from "./ZapSession.ts";

/**
 * hanzod starts this server as a worker with one end of a socketpair on a file
 * descriptor, and carries every ZAP session to it over that one stream. A record
 * is `[u32 LE session][u8 kind][u32 LE length][payload]`; kind 0 is data, 1 opens
 * a session and 2 closes one. The same records go back the other way.
 */
const KIND_DATA = 0;
const KIND_OPEN = 1;
const KIND_CLOSE = 2;
const HEAD = 9;
const MAX_RECORD = 33 * 1024 * 1024;

type Session = {
  readonly queue: Array<Uint8Array>;
  waiting: ((chunk: Uint8Array | null) => void) | undefined;
  ended: boolean;
};

const record = (session: number, kind: number, payload: Uint8Array): Uint8Array => {
  const out = new Uint8Array(HEAD + payload.byteLength);
  const view = new DataView(out.buffer);
  view.setUint32(0, session, true);
  view.setUint8(4, kind);
  view.setUint32(5, payload.byteLength, true);
  out.set(payload, HEAD);
  return out;
};

/**
 * Reads mux records off the descriptor and hands each opened session to
 * `onStream` as a ZAP stream. Returns a function that closes the descriptor.
 */
export const serveMux = (fd: number, onStream: (stream: ZapStream) => void): (() => void) => {
  const socket = new NodeNet.Socket({ fd, readable: true, writable: true });
  const sessions = new Map<number, Session>();
  let buffered: Uint8Array = new Uint8Array(0);

  const end = (session: Session) => {
    session.ended = true;
    session.waiting?.(null);
    session.waiting = undefined;
  };

  const streamFor = (id: number, session: Session): ZapStream => ({
    read: () =>
      new Promise<Uint8Array | null>((resolve) => {
        const next = session.queue.shift();
        if (next !== undefined) return resolve(next);
        if (session.ended) return resolve(null);
        session.waiting = resolve;
      }),
    write: (bytes) =>
      new Promise<void>((resolve, reject) =>
        socket.write(record(id, KIND_DATA, bytes), (error) => (error ? reject(error) : resolve())),
      ),
    close: () => {
      if (sessions.delete(id)) {
        end(session);
        socket.write(record(id, KIND_CLOSE, new Uint8Array(0)));
      }
    },
  });

  socket.on("data", (chunk: Uint8Array) => {
    const joined = new Uint8Array(buffered.byteLength + chunk.byteLength);
    joined.set(buffered);
    joined.set(chunk, buffered.byteLength);
    let at = 0;
    while (joined.byteLength - at >= HEAD) {
      const view = new DataView(joined.buffer, joined.byteOffset + at, HEAD);
      const id = view.getUint32(0, true);
      const kind = view.getUint8(4);
      const length = view.getUint32(5, true);
      if (length > MAX_RECORD) return void socket.destroy();
      if (joined.byteLength - at - HEAD < length) break;
      const payload = joined.slice(at + HEAD, at + HEAD + length);
      at += HEAD + length;
      if (kind === KIND_OPEN) {
        const session: Session = { queue: [], waiting: undefined, ended: false };
        sessions.set(id, session);
        onStream(streamFor(id, session));
      } else if (kind === KIND_DATA) {
        const session = sessions.get(id);
        if (session === undefined) continue;
        if (session.waiting !== undefined) {
          const resolve = session.waiting;
          session.waiting = undefined;
          resolve(payload);
        } else session.queue.push(payload);
      } else if (kind === KIND_CLOSE) {
        const session = sessions.get(id);
        if (session !== undefined) {
          sessions.delete(id);
          end(session);
        }
      }
    }
    buffered = joined.slice(at);
  });
  const finish = () => {
    for (const session of sessions.values()) end(session);
    sessions.clear();
  };
  socket.on("close", finish);
  socket.on("error", () => socket.destroy());
  return () => socket.destroy();
};
