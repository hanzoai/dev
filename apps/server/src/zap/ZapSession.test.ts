import { AuthSessionId } from "@hanzodev/contracts";
import {
  lengthPrefixed,
  splitMessages,
  ZAP_CALL_ID_BASE,
  ZAP_DESCRIPTOR_METHOD,
  ZAP_FRAME_METHOD,
  ZAP_SESSION_METHOD,
  ZAP_SHELL_METHOD,
} from "@hanzodev/shared/zapFrames";
import { setTimeout as scheduler } from "node:timers/promises";
import { assert, it } from "@effect/vitest";
import { buildRequest, parseResponse, Status } from "@zap-proto/zap";
import * as Deferred from "effect/Deferred";
import * as Effect from "effect/Effect";
import * as Fiber from "effect/Fiber";
import * as Layer from "effect/Layer";
import type * as Scope from "effect/Scope";

import type { AuthenticatedSession } from "../auth/EnvironmentAuth.ts";
import { ZapAuth, ZapAuthError } from "./ZapAuth.ts";
import { serveZapStream, type ZapCalls, type ZapStream } from "./ZapSession.ts";

const encoder = new TextEncoder();
const decoder = new TextDecoder();

const session: AuthenticatedSession = {
  sessionId: AuthSessionId.make("s"),
  subject: "hanzo/z",
  method: "bearer-access-token",
  scopes: ["orchestration:read"],
};

const auth = Layer.succeed(ZapAuth, {
  authenticate: (bearer: string) =>
    bearer === "good"
      ? Effect.succeed(session)
      : Effect.fail(new ZapAuthError({ reason: "invalid_token" })),
});

const calls: ZapCalls = {
  descriptor: Effect.succeed({ environmentId: "e" }),
  session: () => Effect.succeed({ authenticated: true }),
  shell: (_session, query) => Effect.succeed({ snapshotSequence: 7, query }),
};

const envelope = (method: number, promiseID: number, cap: string, payload: unknown) =>
  lengthPrefixed(
    buildRequest({
      method,
      promiseID,
      target: 0,
      cap: encoder.encode(cap),
      payload: encoder.encode(typeof payload === "string" ? payload : JSON.stringify(payload)),
    }),
  );

/** A stream the test feeds, and reads what the session wrote. */
const fakeStream = () => {
  const incoming: Array<Uint8Array | null> = [];
  const waiters: Array<(chunk: Uint8Array | null) => void> = [];
  const written: Array<ReturnType<typeof parseResponse>> = [];
  const stream: ZapStream = {
    read: () =>
      incoming.length > 0
        ? Promise.resolve(incoming.shift()!).then((c) => (c === null ? c : new Uint8Array(c)))
        : new Promise((resolve) => waiters.push(resolve)),
    write: async (bytes) => {
      for (const message of splitMessages(bytes).messages) written.push(parseResponse(message));
    },
    close: () => {},
  };
  const feed = (chunk: Uint8Array | null) => {
    const waiter = waiters.shift();
    if (waiter === undefined) incoming.push(chunk);
    else waiter(chunk === null ? chunk : new Uint8Array(chunk));
  };
  const until = (count: number) =>
    Effect.promise(async () => {
      for (let i = 0; i < 200 && written.length < count; i++) await scheduler(5);
    });
  return { stream, feed, written, until };
};

const run = <A, E>(effect: Effect.Effect<A, E, ZapAuth | Scope.Scope>) =>
  effect.pipe(Effect.provide(auth), Effect.scoped);

it.live("answers the descriptor before the stream holds a bearer, and the rest after", () =>
  run(
    Effect.gen(function* () {
      const { stream, feed, written, until } = fakeStream();
      const done = yield* Deferred.make<void>();
      const served = yield* Effect.forkScoped(
        serveZapStream(stream, calls, () => Deferred.await(done)),
      );
      const id = (n: number) => ZAP_CALL_ID_BASE + n;
      feed(envelope(ZAP_DESCRIPTOR_METHOD, id(1), "", {}));
      feed(envelope(ZAP_SHELL_METHOD, id(2), "", {}));
      feed(envelope(ZAP_SESSION_METHOD, id(3), "good", {}));
      feed(envelope(ZAP_SHELL_METHOD, id(4), "", { omitOlderSettled: "1" }));
      yield* until(4);
      const answers = written.map((r) => [r.promiseID, r.status, decoder.decode(r.body)]);
      assert.deepStrictEqual(answers, [
        [id(1), Status.OK, '{"environmentId":"e"}'],
        [id(2), Status.Unauthorized, ""],
        [id(3), Status.OK, '{"authenticated":true}'],
        [id(4), Status.OK, '{"snapshotSequence":7,"query":{"omitOlderSettled":"1"}}'],
      ]);
      yield* Deferred.succeed(done, undefined);
      yield* Fiber.join(served);
    }),
  ),
);

it.live("answers a frame with the id of the frame, and refuses a frame before a bearer", () =>
  run(
    Effect.gen(function* () {
      const { stream, feed, written, until } = fakeStream();
      const done = yield* Deferred.make<void>();
      yield* Effect.forkScoped(
        serveZapStream(stream, calls, (_session, socket) =>
          Effect.gen(function* () {
            const write = yield* socket.writer;
            yield* write.write('{"_tag":"Pong"}');
            yield* Deferred.await(done);
          }).pipe(Effect.scoped),
        ),
      );
      feed(envelope(ZAP_FRAME_METHOD, 5, "good", '{"_tag":"Ping"}'));
      yield* until(1);
      assert.deepStrictEqual(
        written.map((r) => [r.promiseID, decoder.decode(r.body)]),
        [[5, '{"_tag":"Pong"}']],
      );
      yield* Deferred.succeed(done, undefined);
    }),
  ),
);

it.live("refuses a frame on a stream that has no bearer", () =>
  run(
    Effect.gen(function* () {
      const { stream, feed, written, until } = fakeStream();
      yield* Effect.forkScoped(serveZapStream(stream, calls, () => Effect.never));
      feed(envelope(ZAP_FRAME_METHOD, 1, "", "{}"));
      yield* until(1);
      assert.equal(written[0]?.status, Status.Unauthorized);
    }),
  ),
);
