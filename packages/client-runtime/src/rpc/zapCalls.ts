import type { PreparedHttpAuthorization } from "../connection/model.ts";
import {
  ZAP_DESCRIPTOR_METHOD,
  ZAP_SESSION_METHOD,
  ZAP_SHELL_METHOD,
} from "@hanzodev/shared/zapFrames";
import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";

import { executeEnvironmentHttpRequest } from "./http.ts";
import { callZap } from "./zapSocket.ts";

export { ZAP_DESCRIPTOR_METHOD, ZAP_SESSION_METHOD, ZAP_SHELL_METHOD };

/** A runtime reached over ZAP has no HTTP: its address is the `zaps:` URL its calls go to. */
export const isZapUrl = (url: string): boolean => url.startsWith("zaps:");

/** The bearer a connection presents to a ZAP runtime. */
export const zapBearer = (authorization: PreparedHttpAuthorization | null): string | null =>
  authorization === null
    ? null
    : authorization._tag === "Bearer"
      ? authorization.token
      : authorization.accessToken;

export class ZapCallError extends Schema.TaggedError<ZapCallError>()("ZapCallError", {
  cause: Schema.Defect(),
}) {}

/** One call on the runtime's ZAP session: the JSON body of the route it stands for. */
export const zapCallJson = (
  url: string,
  method: number,
  options: { readonly bearer: string | null; readonly query?: Record<string, unknown> },
) =>
  Effect.tryPromise({
    try: () => callZap(url, method, options),
    catch: (cause) => new ZapCallError({ cause }),
  });

/**
 * The answer to a route of the environment's HTTP API, asked over the runtime's ZAP session
 * instead, and read by the schema the route declares. Failures are the HTTP path's own.
 */
export const zapRouteAnswer = <S extends Schema.Top>(
  url: string,
  method: number,
  schema: S,
  options: {
    readonly timeoutMs: number;
    readonly bearer: string | null;
    readonly query?: Record<string, unknown>;
  },
) =>
  executeEnvironmentHttpRequest(
    url,
    options.timeoutMs,
    zapCallJson(url, method, options).pipe(
      Effect.flatMap(Schema.decodeUnknownEffect(Schema.toCodecJson(schema))),
    ),
  );
