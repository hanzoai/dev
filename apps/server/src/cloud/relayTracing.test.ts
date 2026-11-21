import * as NodeServices from "@effect/platform-node/NodeServices";
import { assert, it } from "@effect/vitest";
import type { ManagedPolicyDocument } from "@hanzodev/contracts";
import * as RelayTracing from "@hanzodev/shared/relayTracing";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";
import { FetchHttpClient } from "effect/http";

import * as ServerConfig from "../config.ts";
import * as ManagedPolicy from "../policy/ManagedPolicy.ts";
import { relayLayer } from "./relayTracing.ts";

const tracesTo = (policy?: ManagedPolicyDocument) =>
  Effect.service(RelayTracing.RelayClientTracer).pipe(
    Effect.provide(
      relayLayer(
        { serviceName: "hanzodev-server", runtime: "node", client: "environment-server" },
        { tracesUrl: "https://traces.example.com", tracesDataset: "relay", tracesToken: "token" },
      ).pipe(
        Layer.provide(ManagedPolicy.layerTest(policy)),
        Layer.provide(
          ServerConfig.layerTest(process.cwd(), { prefix: "hanzo-relay-tracing-" }).pipe(
            Layer.provide(NodeServices.layer),
          ),
        ),
        Layer.provide(FetchHttpClient.layer),
      ),
    ),
    Effect.map(Option.isSome),
  );

it.effect("a policy with telemetry off stops relay tracing to Hanzo Link", () =>
  Effect.gen(function* () {
    assert.isTrue(yield* tracesTo());
    assert.isFalse(
      yield* tracesTo({
        schemaVersion: 1,
        organization: "Example Corp",
        limits: { telemetry: false },
      }),
    );
  }),
);
