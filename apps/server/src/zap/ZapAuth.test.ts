import * as NodeCrypto from "@effect/platform-node/NodeCrypto";
import { assert, it } from "@effect/vitest";
import * as Effect from "effect/Effect";
import * as FetchHttpClient from "effect/http/FetchHttpClient";
import * as Layer from "effect/Layer";

import * as ZapAuth from "./ZapAuth.ts";

// A host that serves no discovery document names no issuer and no key set: nothing is trusted.
it.effect("refuses every bearer when discovery names no issuer or key set", () =>
  Effect.gen(function* () {
    const auth = yield* ZapAuth.ZapAuth;
    const error = yield* Effect.flip(auth.authenticate("a.b.c"));
    assert.equal(error.reason, "invalid_token");
  }).pipe(
    Effect.provide(
      (() => {
        const fetch: typeof globalThis.fetch = async () => new Response("no", { status: 404 });
        return ZapAuth.layerIam({
          origin: "https://no-discovery.example",
          audience: "app",
          principal: "hanzo/z",
          fetch,
        }).pipe(Layer.provide(Layer.mergeAll(FetchHttpClient.layer, NodeCrypto.layer)));
      })(),
    ),
  ),
);
