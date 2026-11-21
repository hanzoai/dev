import {
  lengthPrefixed,
  splitMessages,
  ZAP_CALL_ID_BASE,
  ZAP_FRAME_METHOD,
} from "@hanzodev/shared/zapFrames";
import { buildRequest, parseResponse, Status } from "@zap-proto/zap";
import * as Duration from "effect/Duration";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Socket from "effect/socket/Socket";

const NO_TARGET = 0;
const NO_CAP = new Uint8Array(0);
const encoder = new TextEncoder();
const decoder = new TextDecoder();

const readError = (cause: unknown) =>
  new Socket.SocketError({ reason: new Socket.SocketReadError({ cause }) });
const writeError = (cause: unknown) =>
  new Socket.SocketError({ reason: new Socket.SocketWriteError({ cause }) });
const openError = (cause: unknown) =>
  new Socket.SocketError({ reason: new Socket.SocketOpenError({ kind: "Unknown", cause }) });

interface Pending {
  readonly resolve: (body: Uint8Array) => void;
  readonly reject: (cause: unknown) => void;
}

/**
 * One WebTransport session with one bidirectional stream. Frames of the environment protocol and
 * calls (the descriptor, the session, the shell snapshot) share it: a response to a call carries
 * a call id, and every other message is a frame.
 */
interface Session {
  /** The next frame the runtime sent. */
  readonly next: () => Promise<Uint8Array>;
  readonly write: (method: number, payload: Uint8Array, promiseID?: number) => Promise<void>;
  /** One call, answered by the response that names it. */
  readonly call: (method: number, payload: Uint8Array) => Promise<Uint8Array>;
  /** The bearer goes to the runtime on the next envelope, once. */
  readonly present: (bearer: string | null) => void;
  readonly close: () => void;
}

async function open(url: string, forget: () => void): Promise<Session> {
  const transport = new WebTransport(url);
  await transport.ready;
  const stream = await transport.createBidirectionalStream();
  const reader = stream.readable.getReader();
  const writer = stream.writable.getWriter();
  const frames: Array<Uint8Array> = [];
  const waiting: Array<Pending> = [];
  const calls = new Map<number, Pending>();
  let ended: unknown = null;
  let promiseID = 0;
  let callID = ZAP_CALL_ID_BASE;
  let bearer: Uint8Array = NO_CAP;
  let presented = false;

  const end = (cause: unknown) => {
    ended ??= cause;
    forget();
    for (const pending of [...waiting, ...calls.values()]) pending.reject(ended);
    waiting.length = 0;
    calls.clear();
  };
  void (async () => {
    let rest: Uint8Array = new Uint8Array(0);
    try {
      for (;;) {
        const { value, done } = await reader.read();
        if (done) throw new Error("The ZAP stream ended.");
        const joined = new Uint8Array(rest.byteLength + value.byteLength);
        joined.set(rest);
        joined.set(value, rest.byteLength);
        const split = splitMessages(joined);
        rest = split.rest;
        for (const message of split.messages) {
          const response = parseResponse(message.slice());
          const call = calls.get(response.promiseID);
          if (call !== undefined) {
            calls.delete(response.promiseID);
            if (response.status === Status.OK) call.resolve(response.body);
            else call.reject(new Error(`The ZAP call failed with status ${response.status}.`));
            continue;
          }
          const waiter = waiting.shift();
          if (waiter === undefined) frames.push(response.body);
          else waiter.resolve(response.body);
        }
      }
    } catch (cause) {
      end(cause);
    }
  })();

  const write = async (method: number, payload: Uint8Array, id: number = ++promiseID) => {
    if (ended !== null) throw ended;
    const cap = bearer;
    bearer = NO_CAP;
    if (cap.byteLength > 0) presented = true;
    await writer.write(
      lengthPrefixed(buildRequest({ method, promiseID: id, target: NO_TARGET, cap, payload })),
    );
  };
  return {
    next: () =>
      frames.length > 0
        ? Promise.resolve(frames.shift()!)
        : ended !== null
          ? Promise.reject(ended)
          : new Promise((resolve, reject) => waiting.push({ resolve, reject })),
    write,
    call: (method, payload) =>
      new Promise((resolve, reject) => {
        const id = ++callID;
        calls.set(id, { resolve, reject });
        write(method, payload, id).catch((cause) => {
          calls.delete(id);
          reject(cause);
        });
      }),
    present: (token) => {
      if (token !== null && !presented) bearer = encoder.encode(token);
    },
    close: () => {
      transport.close();
      end(new Error("The ZAP session was closed."));
    },
  };
}

// A runtime's calls and its socket share one session, by its address.
const sessions = new Map<string, Promise<Session>>();
const sessionOf = (url: string, bearer: string | null): Promise<Session> => {
  let session = sessions.get(url);
  if (session === undefined) {
    session = open(url, () => sessions.delete(url));
    sessions.set(url, session);
    session.catch(() => sessions.delete(url));
  }
  return session.then((live) => {
    live.present(bearer);
    return live;
  });
};

/**
 * One call on a runtime's ZAP session, answered with the JSON body the Dev server's HTTP route
 * returns for it. `url` is the runtime's `zaps:` address, as the connection holds it.
 */
export const callZap = (
  url: string,
  method: number,
  options: { readonly bearer: string | null; readonly query?: Record<string, unknown> },
): Promise<unknown> =>
  sessionOf(url.replace(/^zaps:/, "https:"), options.bearer)
    .then((live) => live.call(method, encoder.encode(JSON.stringify(options.query ?? {}))))
    .then((body) => JSON.parse(decoder.decode(body)) as unknown);

/**
 * The environment protocol over ZAP on WebTransport (HTTP/3): the browser's native ZAP
 * carrier, with no WebSocket anywhere. Each frame the client writes is a ZAP request
 * envelope whose payload is the frame; each envelope the runtime sends back carries one
 * frame as its body. One bidirectional stream per session.
 */
export const layerZapSocket = (
  url: string,
  options: { readonly openTimeout: Duration.Input; readonly bearer: string | null },
) =>
  Layer.sync(Socket.Socket, () => {
    const connect = () => sessionOf(url, options.bearer);
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
            try: async () => [decoder.decode(await live.next())] as [string],
            catch: readError,
          }),
        })),
      ),
      writer: Effect.succeed({
        write: (chunk) =>
          typeof chunk === "object" && "code" in chunk
            ? Effect.promise(connect).pipe(Effect.map((live) => live.close()))
            : Effect.tryPromise({
                try: async () => (await connect()).write(ZAP_FRAME_METHOD, bytes(chunk)),
                catch: writeError,
              }),
        writeAll: (chunks) =>
          Effect.tryPromise({
            try: async () => {
              const live = await connect();
              for (const chunk of chunks) await live.write(ZAP_FRAME_METHOD, bytes(chunk));
            },
            catch: writeError,
          }),
      }),
    });
  });

const bytes = (chunk: string | Uint8Array) =>
  typeof chunk === "string" ? encoder.encode(chunk) : chunk;
