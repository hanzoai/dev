import { buildRequest, parseResponse } from "@zap-proto/zap";
import * as Duration from "effect/Duration";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Socket from "effect/socket/Socket";

// The Dev session service's one method: a frame of the environment protocol.
// The ordinal is the contract with the runtime's ZAP endpoint (apps/web/src/embed/runtime.ts).
export const ZAP_FRAME_METHOD = 0;

const NO_TARGET = 0;
const NO_CAP = new Uint8Array(0);
const encoder = new TextEncoder();
const decoder = new TextDecoder();

/** A ZAP message on a stream: u32 little-endian length, then the router envelope. */
export function lengthPrefixed(envelope: Uint8Array): Uint8Array {
  const out = new Uint8Array(4 + envelope.byteLength);
  new DataView(out.buffer).setUint32(0, envelope.byteLength, true);
  out.set(envelope, 4);
  return out;
}

/** Splits `buffered` into whole messages; returns them with what is left over. */
export function splitMessages(buffered: Uint8Array): {
  readonly messages: ReadonlyArray<Uint8Array>;
  readonly rest: Uint8Array;
} {
  const messages: Array<Uint8Array> = [];
  let at = 0;
  while (buffered.byteLength - at >= 4) {
    const size = new DataView(buffered.buffer, buffered.byteOffset + at, 4).getUint32(0, true);
    if (buffered.byteLength - at - 4 < size) break;
    messages.push(buffered.subarray(at + 4, at + 4 + size));
    at += 4 + size;
  }
  return { messages, rest: buffered.subarray(at) };
}

const readError = (cause: unknown) =>
  new Socket.SocketError({ reason: new Socket.SocketReadError({ cause }) });
const writeError = (cause: unknown) =>
  new Socket.SocketError({ reason: new Socket.SocketWriteError({ cause }) });
const openError = (cause: unknown) =>
  new Socket.SocketError({ reason: new Socket.SocketOpenError({ kind: "Unknown", cause }) });

interface Session {
  readonly writer: WritableStreamDefaultWriter<Uint8Array>;
  readonly next: () => Promise<Uint8Array>;
  readonly close: () => void;
}

/** One WebTransport session with one bidirectional stream, read into whole ZAP messages. */
async function open(url: string): Promise<Session> {
  const transport = new WebTransport(url);
  await transport.ready;
  const stream = await transport.createBidirectionalStream();
  const reader = stream.readable.getReader();
  let rest: Uint8Array = new Uint8Array(0);
  const queue: Array<Uint8Array> = [];
  return {
    writer: stream.writable.getWriter(),
    next: async () => {
      while (queue.length === 0) {
        const { value, done } = await reader.read();
        if (done) throw new Error("The ZAP stream ended.");
        const joined = new Uint8Array(rest.byteLength + value.byteLength);
        joined.set(rest);
        joined.set(value, rest.byteLength);
        const split = splitMessages(joined);
        rest = split.rest;
        queue.push(...split.messages);
      }
      return queue.shift()!;
    },
    close: () => transport.close(),
  };
}

/**
 * The environment protocol over ZAP on WebTransport (HTTP/3): the browser's native ZAP
 * carrier, with no WebSocket anywhere. Each frame the client writes is a ZAP request
 * envelope whose payload is the frame; each envelope the runtime sends back carries one
 * frame as its body. One bidirectional stream per session.
 */
export const layerZapSocket = (url: string, options: { readonly openTimeout: Duration.Input; readonly bearer: string | null }) =>
  Layer.sync(Socket.Socket, () => {
    let session: Promise<Session> | null = null;
    const connect = () => (session ??= open(url));
    let promiseID = 0;
    // The bearer is presented once, on the session's first envelope.
    const bearerCap = options.bearer === null ? NO_CAP : encoder.encode(options.bearer);
    const frame = (chunk: string | Uint8Array) =>
      lengthPrefixed(
        buildRequest({
          method: ZAP_FRAME_METHOD,
          promiseID: ++promiseID,
          target: NO_TARGET,
          cap: promiseID === 1 ? bearerCap : NO_CAP,
          payload: typeof chunk === "string" ? encoder.encode(chunk) : chunk,
        }),
      );
    return Socket.make({
      reader: Effect.acquireRelease(
        Effect.tryPromise({ try: connect, catch: openError }).pipe(
          Effect.timeout(options.openTimeout),
          Effect.mapError(openError),
        ),
        (live) => Effect.sync(() => live.close()),
      ).pipe(
        Effect.map((live) => ({
          upgrade: () => Effect.void,
          pull: Effect.tryPromise({
            try: async () => [decoder.decode(parseResponse(await live.next()).body)] as [string],
            catch: readError,
          }),
        })),
      ),
      writer: Effect.succeed({
        write: (chunk) =>
          typeof chunk === "object" && "code" in chunk
            ? Effect.promise(connect).pipe(Effect.map((live) => live.close()))
            : Effect.tryPromise({
                try: async () => (await connect()).writer.write(frame(chunk)),
                catch: writeError,
              }),
        writeAll: (chunks) =>
          Effect.tryPromise({
            try: async () => {
              const live = await connect();
              for (const chunk of chunks) await live.writer.write(frame(chunk));
            },
            catch: writeError,
          }),
      }),
    });
  });
