// @effect-diagnostics-next-line nodeBuiltinImport:off -- The local ZAP stream is a platform socket.
import * as NodeNet from "node:net";
import * as Config from "effect/Config";
import * as NetAddress from "effect/net/NetAddress";
import * as Deferred from "effect/Deferred";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as FetchHttpClient from "effect/http/FetchHttpClient";
import * as Option from "effect/Option";
import * as RpcServer from "effect/rpc/RpcServer";
import * as SocketServer from "effect/socket/SocketServer";
import type * as Socket from "effect/socket/Socket";

import * as AnalyticsService from "../telemetry/AnalyticsService.ts";
import { makeHanzoIamConfig } from "../cloud/publicConfig.ts";
import { RpcSessions } from "../ws.ts";
import { layerIam, ZapAuth } from "./ZapAuth.ts";
import { serveZapStream, type ZapStream } from "./ZapSession.ts";

// A connection of the local ZAP stream (a unix socket, or loopback TCP) read as a byte stream.
const streamOf = (socket: NodeNet.Socket): ZapStream => {
  const chunks = socket[Symbol.asyncIterator]();
  socket.on("error", () => socket.destroy());
  return {
    read: async () => {
      const next = await chunks.next();
      return next.done ? null : new Uint8Array(next.value as Uint8Array);
    },
    write: (bytes) =>
      new Promise<void>((resolve, reject) =>
        socket.write(bytes, (error) => (error ? reject(error) : resolve())),
      ),
    close: () => void socket.end(),
  };
};

/** Mounts one socket as the only connection of a socket-server protocol, for one session. */
const mountSocket = Effect.fn("ZapServer.mountSocket")(function* (socket: Socket.Socket) {
  const ended = yield* Deferred.make<void>();
  const server = SocketServer.SocketServer.of({
    address: NetAddress.unixPathAddress("zap"),
    run: Effect.fn("ZapServer.run")((handler) =>
      handler(socket).pipe(
        Effect.ensuring(Deferred.succeed(ended, undefined)),
        Effect.andThen(Effect.never),
        Effect.orDie,
      ),
    ),
  });
  const protocol = yield* RpcServer.makeProtocolSocketServer.pipe(
    Effect.provideService(SocketServer.SocketServer, server),
  );
  return { protocol, attach: Deferred.await(ended) };
});

/** Where the local ZAP stream listens: a unix socket path, or a loopback TCP port. */
export type ZapListen =
  | { readonly socket: string; readonly port?: never }
  | { readonly port: number; readonly socket?: never };

/**
 * Serves the environment protocol over ZAP on a local stream: zip's WebTransport edge carries
 * each session's frames here unchanged. The first envelope's cap is checked here as an IAM bearer.
 */
export const layer = (listen: ZapListen) =>
  Layer.effectDiscard(
    Effect.gen(function* () {
      const sessions = yield* RpcSessions;
      const auth = yield* ZapAuth;
      const context = yield* Effect.context<AnalyticsService.AnalyticsService>();
      const runSession = (stream: ZapStream) =>
        serveZapStream(stream, (session, socket) =>
          Effect.scoped(
            Effect.gen(function* () {
              const attach = yield* sessions.serve({
                session,
                clientAnalyticsProps: {},
                settingsClient: { secretReferences: false },
                mount: mountSocket(socket),
              });
              yield* attach;
            }),
          ),
        ).pipe(Effect.scoped, Effect.provideService(ZapAuth, auth), Effect.ignoreCause);
      const run = Effect.runForkWith(context);
      yield* Effect.acquireRelease(
        Effect.callback<NodeNet.Server>((resume) => {
          const server = NodeNet.createServer((connection) =>
            run(runSession(streamOf(connection))),
          );
          server.once("listening", () => resume(Effect.succeed(server)));
          if (listen.socket !== undefined) {
            server.listen({ path: listen.socket, readableAll: false, writableAll: false });
          } else {
            server.listen({ host: "127.0.0.1", port: listen.port });
          }
        }),
        (server) => Effect.callback<void>((resume) => void server.close(() => resume(Effect.void))),
      );
    }),
  );

/** ZAP listens only where the environment names a socket or port; the person it serves is named there too. */
export const ZapListenConfig = Config.all({
  socket: Config.String("HANZO_DEV_ZAP_SOCKET").pipe(Config.option),
  port: Config.Int("HANZO_DEV_ZAP_PORT").pipe(Config.option),
  principal: Config.String("HANZO_DEV_ZAP_PRINCIPAL"),
}).pipe(Config.option);

/** ZAP when the environment names a stream; nothing otherwise. Tokens are checked against hanzo.id's JWKS. */
export const layerFromEnvironment = Layer.unwrap(
  Effect.gen(function* () {
    const config = yield* ZapListenConfig;
    if (Option.isNone(config)) return Layer.empty;
    const { socket, port, principal } = config.value;
    const listen: ZapListen | undefined = Option.isSome(socket)
      ? { socket: socket.value }
      : Option.isSome(port)
        ? { port: port.value }
        : undefined;
    if (listen === undefined) return Layer.empty;
    const { iamIssuer, iamClientId } = yield* makeHanzoIamConfig();
    const auth = layerIam({ origin: iamIssuer, audience: iamClientId, principal }).pipe(
      Layer.provide(FetchHttpClient.layer),
    );
    return layer(listen).pipe(Layer.provide(auth));
  }),
);
