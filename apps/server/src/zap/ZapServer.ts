import * as Config from "effect/Config";
import * as NetAddress from "effect/net/NetAddress";
import * as SecureRandom from "effect/crypto/SecureRandom";
import * as Deferred from "effect/Deferred";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Layer from "effect/Layer";
import * as FetchHttpClient from "effect/http/FetchHttpClient";
import * as Jwks from "effect/jose/Jwks";
import * as Option from "effect/Option";
import * as RpcServer from "effect/rpc/RpcServer";
import * as SocketServer from "effect/socket/SocketServer";
import type * as Socket from "effect/socket/Socket";

import * as AnalyticsService from "../telemetry/AnalyticsService.ts";
import { makeHanzoIamConfig } from "../cloud/publicConfig.ts";
import { normalizeHanzoIssuer } from "@hanzodev/shared/hanzoIamAuth";
import { RpcSessions } from "../ws.ts";
import { layerIam, ZapAuth } from "./ZapAuth.ts";
import { serveZapStream, type ZapStream } from "./ZapSession.ts";

// A WebTransport stream is a pair of web streams; this is the byte stream the session reads.
const streamOf = (stream: {
  readonly readable: ReadableStream<Uint8Array>;
  readonly writable: WritableStream<Uint8Array>;
}): ZapStream => {
  const reader = stream.readable.getReader();
  const writer = stream.writable.getWriter();
  return {
    read: async () => {
      const { value, done } = await reader.read();
      return done ? null : value;
    },
    write: (bytes) => writer.write(bytes),
    close: () => void writer.close().catch(() => undefined),
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

export interface ZapListen {
  readonly host: string;
  readonly port: number;
  /** The path sessions are opened on: `https://<host>:<port><path>`. */
  readonly path: string;
  readonly certPath: string;
  readonly keyPath: string;
}

/**
 * Serves the environment protocol over ZAP on WebTransport (HTTP/3 on UDP): one bidirectional
 * stream per session, the first envelope's cap the IAM bearer. The QUIC binding is
 * `@fails-components/webtransport`, loaded only when ZAP is listening.
 */
export const layer = (listen: ZapListen) =>
  Layer.effectDiscard(
    Effect.gen(function* () {
      const fs = yield* FileSystem.FileSystem;
      const sessions = yield* RpcSessions;
      const auth = yield* ZapAuth;
      const context = yield* Effect.context<AnalyticsService.AnalyticsService>();
      const secret = yield* SecureRandom.SecureRandom.use((random) => random.uuidV4());
      const [cert, privKey] = yield* Effect.all([
        fs.readFileString(listen.certPath),
        fs.readFileString(listen.keyPath),
      ]);
      const { Http3Server } = yield* Effect.promise(
        () => import("@fails-components/webtransport"),
      );
      const server = yield* Effect.acquireRelease(
        Effect.sync(() => {
          const http3 = new Http3Server({
            host: listen.host,
            port: listen.port,
            secret,
            cert,
            privKey,
          });
          http3.startServer();
          return http3;
        }),
        (http3) => Effect.sync(() => http3.stopServer()),
      );
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
        ).pipe(
          Effect.scoped,
          Effect.provideService(ZapAuth, auth),
          Effect.ignoreCause,
        );
      const runtime = Effect.runForkWith(context);
      const sessionStream = yield* Effect.promise(async () => server.sessionStream(listen.path));
      const accept = Effect.promise(async () => {
        const reader = sessionStream.getReader();
        for (;;) {
          const { value: transport, done } = await reader.read();
          if (done) return;
          void (async () => {
            await transport.ready;
            const incoming = transport.incomingBidirectionalStreams.getReader();
            const { value: bidirectional } = await incoming.read();
            if (bidirectional === undefined) return;
            runtime(runSession(streamOf(bidirectional)));
          })().catch(() => undefined);
        }
      });
      yield* Effect.forkScoped(accept);
      yield* Effect.promise(() => server.ready);
    }),
  );

/** ZAP listens only where the environment names a port; the person it serves is named there too. */
export const ZapListenConfig = Config.all({
  port: Config.Int("HANZO_DEV_ZAP_PORT"),
  host: Config.String("HANZO_DEV_ZAP_HOST").pipe(Config.withDefault("0.0.0.0")),
  path: Config.String("HANZO_DEV_ZAP_PATH").pipe(Config.withDefault("/")),
  certPath: Config.String("HANZO_DEV_ZAP_CERT"),
  keyPath: Config.String("HANZO_DEV_ZAP_KEY"),
  principal: Config.String("HANZO_DEV_ZAP_PRINCIPAL"),
}).pipe(Config.option);


/** ZAP when the environment names a port; nothing otherwise. Tokens are checked against hanzo.id's JWKS. */
export const layerFromEnvironment = Layer.unwrap(
  Effect.gen(function* () {
    const listen = yield* ZapListenConfig;
    if (Option.isNone(listen)) return Layer.empty;
    const { iamIssuer, iamClientId } = yield* makeHanzoIamConfig();
    const issuer = normalizeHanzoIssuer(iamIssuer);
    // A key set that redirects is refused, so the client must not follow redirects.
    const http = FetchHttpClient.layer.pipe(
      Layer.provide(Layer.succeed(FetchHttpClient.RequestInit, { redirect: "manual" })),
    );
    const auth = layerIam({
      issuer,
      audience: iamClientId,
      principal: listen.value.principal,
    }).pipe(Layer.provide(Jwks.layerRemote({ url: `${issuer}/.well-known/jwks` }).pipe(Layer.provide(http))));
    return layer(listen.value).pipe(Layer.provide(auth));
  }),
);
