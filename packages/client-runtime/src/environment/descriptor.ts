import { ExecutionEnvironmentDescriptor } from "@hanzodev/contracts";
import * as Effect from "effect/Effect";

import { environmentEndpointUrl } from "./endpoint.ts";
import { executeEnvironmentHttpRequest, makeEnvironmentHttpApiGroupClient } from "../rpc/http.ts";
import { isZapUrl, ZAP_DESCRIPTOR_METHOD, zapRouteAnswer } from "../rpc/zapCalls.ts";

const DEFAULT_REMOTE_REQUEST_TIMEOUT_MS = 10_000;

export const fetchRemoteEnvironmentDescriptor = Effect.fn(
  "clientRuntime.environment.fetchRemoteEnvironmentDescriptor",
)(function* (input: {
  readonly httpBaseUrl: string;
  /** The runtime's `zaps:` address, when it is reached over ZAP: the descriptor is asked there. */
  readonly zapUrl?: string;
  readonly timeoutMs?: number;
}) {
  // The descriptor is public: a ZAP runtime answers it before the session holds a credential.
  if (input.zapUrl !== undefined && isZapUrl(input.zapUrl)) {
    return yield* zapRouteAnswer(
      input.zapUrl,
      ZAP_DESCRIPTOR_METHOD,
      ExecutionEnvironmentDescriptor,
      { timeoutMs: input.timeoutMs ?? DEFAULT_REMOTE_REQUEST_TIMEOUT_MS, bearer: null },
    );
  }
  const client = yield* makeEnvironmentHttpApiGroupClient(input.httpBaseUrl, "metadata");
  return yield* executeEnvironmentHttpRequest(
    environmentEndpointUrl(input.httpBaseUrl, "/.well-known/hanzo/environment"),
    input.timeoutMs ?? DEFAULT_REMOTE_REQUEST_TIMEOUT_MS,
    client.descriptor(),
  );
});
