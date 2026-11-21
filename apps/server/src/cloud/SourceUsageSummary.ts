import { type UsageSummary } from "@hanzodev/usage";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Schema from "effect/Schema";

import * as ServerConfig from "../config.ts";
import { readHostReadings } from "../usage/HostReadings.ts";
import { usageRuntimeFor } from "../usage/UsageRuntime.ts";

export class SourceUsageSummaryError extends Schema.TaggedError<SourceUsageSummaryError>()(
  "SourceUsageSummaryError",
  { cause: Schema.Defect() },
) {}

/** The usage model's current summary for this desktop: what the panel and the tray read. */
export class SourceUsageSummary extends Context.Service<
  SourceUsageSummary,
  { readonly read: Effect.Effect<UsageSummary, SourceUsageSummaryError> }
>()("hanzo/cloud/SourceUsageSummary") {}

export const layer = Layer.effect(
  SourceUsageSummary,
  Effect.gen(function* () {
    const config = yield* ServerConfig.ServerConfig;
    const runtime = usageRuntimeFor(config.stateDir, {
      platform: yield* HostProcess.Platform,
      home: yield* HostProcess.HomeDirectory,
      env: yield* HostProcess.Environment,
    });
    const services = yield* Effect.context<Effect.Services<typeof readHostReadings>>();
    const host = () => Effect.runPromiseWith(services)(readHostReadings);
    return SourceUsageSummary.of({
      read: Effect.tryPromise({
        try: () => runtime.summary(host),
        catch: (cause) => new SourceUsageSummaryError({ cause }),
      }),
    });
  }),
);
