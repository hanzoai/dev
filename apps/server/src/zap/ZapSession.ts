import {
  lengthPrefixed,
  splitMessages,
  ZAP_DESCRIPTOR_METHOD,
  ZAP_FRAME_METHOD,
  ZAP_SESSION_METHOD,
  ZAP_SHELL_METHOD,
} from "@hanzodev/shared/zapFrames";
import { buildResponse, parseRequest, Status } from "@zap-proto/zap";
import * as Clock from "effect/Clock";
import * as Deferred from "effect/Deferred";
import * as Duration from "effect/Duration";
import * as Effect from "effect/Effect";
import * as Queue from "effect/Queue";
import * as Schema from "effect/Schema";
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
      ready.push(...split.messages.map((message) => message.slice()));
    }
    return ready.shift()!;
  };
};

/** The shell snapshot could not be built; the caller is told the call failed. */
export class ZapShellError extends Schema.TaggedError<ZapShellError>()("ZapShellError", {
  cause: Schema.Defect(),
}) {}

/**
 * What a session answers besides frames: the three questions a client asks before it opens the
 * stream, as the environment's HTTP routes answer them (`/.well-known/hanzo/environment`,
 * `/v1/auth/session`, `/v1/event/shell`). Each is the route's JSON body.
 */
export interface ZapCalls {
  /** Public: answered before the stream holds a credential. */
  readonly descriptor: Effect.Effect<unknown>;
  readonly session: (session: AuthenticatedSession) => Effect.Effect<unknown>;
  /** `query` is the route's query as a JSON object. */
  readonly shell: (
    session: AuthenticatedSession,
    query: Record<string, unknown>,
  ) => Effect.Effect<unknown, ZapShellError>;
}

/**
 * Serves one ZAP stream. The bearer is the `cap` of an envelope, any envelope: it is verified
 * as an IAM bearer, and until it has been, only the descriptor is answered. `serve` runs the
 * protocol over the socket this returns to it, and the session ends when the stream ends, the
 * token expires, or `serve` finishes.
 */
export const serveZapStream = Effect.fn("ZapSession.serve")(function* <E, R>(
  stream: ZapStream,
  calls: ZapCalls,
  serve: (session: AuthenticatedSession, socket: Socket.Socket) => Effect.Effect<void, E, R>,
) {
  const auth = yield* ZapAuth;
  const next = messageReader(stream);
  const reply = (status: number, id: number, body: Uint8Array) =>
    Effect.promise(() => stream.write(lengthPrefixed(buildResponse(status, id, body))));
  const refuse = (status: number, id = 0) => reply(status, id, new Uint8Array(0));

  const frames = yield* Queue.bounded<string, Socket.SocketError>(64);
  const authenticated = yield* Deferred.make<AuthenticatedSession | null>();
  let session: AuthenticatedSession | null = null;
  // Responses to frames name the last frame; responses to calls name their own call.
  let promiseID = 0;

  const answer = Effect.fn("ZapSession.answer")(function* (
    call: ReturnType<typeof parseRequest>,
    caller: AuthenticatedSession | null,
  ) {
    const json = (value: unknown) =>
      reply(Status.OK, call.promiseID, encoder.encode(JSON.stringify(value)));
    if (call.method === ZAP_DESCRIPTOR_METHOD)
      return yield* calls.descriptor.pipe(Effect.flatMap(json));
    if (caller === null) return yield* refuse(Status.Unauthorized, call.promiseID);
    if (call.method === ZAP_SESSION_METHOD)
      return yield* calls.session(caller).pipe(Effect.flatMap(json));
    if (call.method === ZAP_SHELL_METHOD) {
      const query = JSON.parse(decoder.decode(call.payload) || "{}") as Record<string, unknown>;
      return yield* calls.shell(caller, query).pipe(
        Effect.flatMap(json),
        Effect.catch(() => refuse(Status.Internal, call.promiseID)),
      );
    }
    return yield* refuse(Status.NotFound, call.promiseID);
  });

  // One envelope: authenticates the stream when it carries a bearer, then is a frame or a call.
  const dispatch = Effect.fn("ZapSession.dispatch")(function* (message: Uint8Array) {
    const call = parseRequest(message);
    if (call.method === ZAP_FRAME_METHOD) promiseID = call.promiseID;
    if (session === null && call.cap.byteLength > 0) {
      const verified = yield* auth.authenticate(decoder.decode(call.cap)).pipe(Effect.option);
      if (verified._tag === "None") {
        yield* refuse(Status.Unauthorized, call.promiseID);
        return yield* Effect.fail(closed());
      }
      session = verified.value;
      yield* Deferred.succeed(authenticated, session);
    }
    if (call.method !== ZAP_FRAME_METHOD) return yield* answer(call, session);
    if (session === null) {
      yield* refuse(Status.Unauthorized, call.promiseID);
      return yield* Effect.fail(closed());
    }
    // A full queue stalls the reader, which stalls the stream's flow control.
    yield* Queue.offer(frames, decoder.decode(call.payload));
  });

  yield* Effect.gen(function* () {
    for (;;) {
      const message = yield* Effect.tryPromise({ try: next, catch: readError });
      if (message === null) return;
      yield* dispatch(message);
    }
  }).pipe(
    Effect.matchCauseEffect({
      onSuccess: () => Queue.fail(frames, closed()),
      onFailure: (cause) => Queue.failCause(frames, cause),
    }),
    Effect.ensuring(Deferred.succeed(authenticated, null)),
    Effect.forkScoped,
  );

  // The bearer has this long to arrive, on whichever envelope carries it.
  const verified = yield* Deferred.await(authenticated).pipe(
    Effect.timeout(ZAP_AUTH_TIMEOUT),
    Effect.orElseSucceed(() => null),
  );
  if (verified === null) {
    yield* Effect.sync(() => stream.close());
    return;
  }

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
  const expiresAt = verified.expiresAt;
  const expiry =
    expiresAt === undefined
      ? Effect.never
      : Effect.sleep(Duration.millis(Math.max(0, DateTime.toEpochMillis(expiresAt) - now)));
  yield* Effect.raceFirst(serve(verified, socket), expiry).pipe(
    Effect.ensuring(Effect.sync(() => stream.close())),
  );
});
