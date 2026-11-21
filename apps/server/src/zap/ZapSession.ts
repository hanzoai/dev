import { lengthPrefixed, splitMessages, ZAP_FRAME_METHOD } from "@hanzodev/shared/zapFrames";
import { buildResponse, parseRequest, Status } from "@zap-proto/zap";
import * as Clock from "effect/Clock";
import * as Duration from "effect/Duration";
import * as Effect from "effect/Effect";
import * as Queue from "effect/Queue";
import * as Socket from "effect/socket/Socket";
import * as DateTime from "effect/DateTime";

import type { AuthenticatedSession } from "../auth/EnvironmentAuth.ts";
import { ZapAuth } from "./ZapAuth.ts";

/** The largest ZAP message a client may send: bounds what a stream can make the server buffer. */
export const ZAP_MAX_MESSAGE_BYTES = 32 * 1024 * 1024;
/** How long a new stream has to present its bearer. */
export const ZAP_AUTH_TIMEOUT = Duration.seconds(10);

const decoder = new TextDecoder();
const encoder = new TextEncoder();

/** One bidirectional byte stream: a WebTransport stream, or anything shaped like one. */
export interface ZapStream {
  /** The next chunk, or `null` once the peer finished. */
  readonly read: () => Promise<Uint8Array | null>;
  readonly write: (bytes: Uint8Array) => Promise<void>;
  readonly close: () => void;
}

const closed = () =>
  new Socket.SocketError({ reason: new Socket.SocketCloseError({ code: 1000 }) });
const readError = (cause: unknown) =>
  new Socket.SocketError({ reason: new Socket.SocketReadError({ cause }) });
const writeError = (cause: unknown) =>
  new Socket.SocketError({ reason: new Socket.SocketWriteError({ cause }) });

/** Reads whole ZAP messages off a stream; throws when the stream ends or a message is too large. */
const messageReader = (stream: ZapStream) => {
  let rest: Uint8Array = new Uint8Array(0);
  const ready: Array<Uint8Array> = [];
  return async (): Promise<Uint8Array | null> => {
    while (ready.length === 0) {
      const chunk = await stream.read();
      if (chunk === null) return null;
      const joined = new Uint8Array(rest.byteLength + chunk.byteLength);
      joined.set(rest);
      joined.set(chunk, rest.byteLength);
      const split = splitMessages(joined);
      if (split.rest.byteLength > ZAP_MAX_MESSAGE_BYTES + 4)
        throw new Error("ZAP message too large");
      rest = split.rest;
      ready.push(...split.messages);
    }
    return ready.shift()!;
  };
};

/** A frame request: the method must be the Dev frame method, its payload is the frame. */
const frameOf = (message: Uint8Array) => {
  const call = parseRequest(message);
  if (call.method !== ZAP_FRAME_METHOD) throw new Error("unknown ZAP method");
  return call;
};

/**
 * Serves one ZAP stream. The first envelope's cap is the IAM bearer; it is verified before any
 * frame reaches the environment protocol. `serve` runs the protocol over the socket this returns
 * to it, and the session ends when the stream ends, the token expires, or `serve` finishes.
 */
export const serveZapStream = Effect.fn("ZapSession.serve")(function* <E, R>(
  stream: ZapStream,
  serve: (session: AuthenticatedSession, socket: Socket.Socket) => Effect.Effect<void, E, R>,
) {
  const auth = yield* ZapAuth;
  const next = messageReader(stream);
  const refuse = (status: number) =>
    Effect.promise(() => stream.write(lengthPrefixed(buildResponse(status, 0, new Uint8Array(0)))));

  const first = yield* Effect.tryPromise({ try: next, catch: readError }).pipe(
    Effect.timeout(ZAP_AUTH_TIMEOUT),
    Effect.map((message) => (message === null ? null : frameOf(message))),
    Effect.orElseSucceed(() => null),
  );
  if (first === null || first.cap.byteLength === 0) return yield* refuse(Status.Unauthorized);
  const session = yield* auth.authenticate(decoder.decode(first.cap)).pipe(Effect.option);
  if (session._tag === "None") return yield* refuse(Status.Unauthorized);

  const run = Effect.runPromiseWith(yield* Effect.context<never>());
  const frames = yield* Queue.bounded<string, Socket.SocketError>(64);
  yield* Queue.offer(frames, decoder.decode(first.payload));
  let promiseID = first.promiseID;
  yield* Effect.tryPromise({
    try: async () => {
      for (;;) {
        const message = await next();
        if (message === null) return;
        const call = frameOf(message);
        promiseID = call.promiseID;
        // A full queue stalls this loop, which stalls the stream's flow control.
        await run(Queue.offer(frames, decoder.decode(call.payload)));
      }
    },
    catch: readError,
  }).pipe(
    Effect.matchCauseEffect({
      onSuccess: () => Queue.fail(frames, closed()),
      onFailure: (cause) => Queue.failCause(frames, cause),
    }),
    Effect.forkScoped,
  );

  const send = (chunk: string | Uint8Array) =>
    Effect.tryPromise({
      try: () =>
        stream.write(
          lengthPrefixed(
            buildResponse(
              Status.OK,
              promiseID,
              typeof chunk === "string" ? encoder.encode(chunk) : chunk,
            ),
          ),
        ),
      catch: writeError,
    });
  const socket = Socket.make({
    reader: Effect.succeed({
      upgrade: () => Effect.void,
      pull: Queue.takeAll(frames),
    }),
    writer: Effect.succeed({
      write: (chunk) =>
        typeof chunk === "object" && "code" in chunk
          ? Effect.sync(() => stream.close())
          : send(chunk),
      writeAll: (chunks) => Effect.forEach(chunks, send, { discard: true }),
    }),
  });

  const now = yield* Clock.currentTimeMillis;
  const expiresAt = session.value.expiresAt;
  const expiry =
    expiresAt === undefined
      ? Effect.never
      : Effect.sleep(Duration.millis(Math.max(0, DateTime.toEpochMillis(expiresAt) - now)));
  yield* Effect.raceFirst(serve(session.value, socket), expiry).pipe(
    Effect.ensuring(Effect.sync(() => stream.close())),
  );
});
