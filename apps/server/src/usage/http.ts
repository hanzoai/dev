// @effect-diagnostics globalDate:off
import { AuthOrchestrationOperateScope, AuthOrchestrationReadScope } from "@hanzodev/contracts";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import { UsageSettings, type HostReadings, type ProviderSnapshot } from "@hanzodev/usage";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Schema from "effect/Schema";
import {
  HttpRouter,
  HttpServerRequest,
  HttpServerRespondable,
  HttpServerResponse,
} from "effect/http";

import * as BrowserSources from "../browser/BrowserSources.ts";
import * as ServerConfig from "../config.ts";
import { authenticateRawRouteWithScope } from "../http.ts";
import { readHostReadings } from "./HostReadings.ts";
import { usageRuntimeFor, type UsageRuntime } from "./UsageRuntime.ts";

const decodeSettings = Schema.decodeUnknownSync(UsageSettings);

class UsageRouteError extends Schema.TaggedError<UsageRouteError>()("UsageRouteError", {
  message: Schema.String,
}) {}

type Scope = typeof AuthOrchestrationReadScope | typeof AuthOrchestrationOperateScope;

const route = (
  method: "GET" | "POST" | "PUT",
  path: `/${string}`,
  scope: Scope,
  handle: (
    runtime: UsageRuntime,
    request: { url: string; body: unknown },
    host: () => Promise<HostReadings>,
    browser: () => Promise<ReadonlyArray<ProviderSnapshot>>,
  ) => Promise<unknown>,
) =>
  HttpRouter.add(
    method,
    path,
    Effect.gen(function* () {
      yield* authenticateRawRouteWithScope(scope);
      const config = yield* ServerConfig.ServerConfig;
      const request = yield* HttpServerRequest.HttpServerRequest;
      const body =
        method === "PUT" ? yield* request.json.pipe(Effect.orElseSucceed(() => null)) : null;
      const runtime = usageRuntimeFor(config.stateDir, {
        platform: yield* HostProcess.Platform,
        home: yield* HostProcess.HomeDirectory,
        env: yield* HostProcess.Environment,
      });
      const services = yield* Effect.context<Effect.Services<typeof readHostReadings>>();
      const host = () => Effect.runPromiseWith(services)(readHostReadings);
      const browsers = yield* BrowserSources.BrowserSources;
      const browser = () => Effect.runPromiseWith(services)(browsers.usage);
      const out = yield* Effect.tryPromise({
        try: () => handle(runtime, { url: request.url, body }, host, browser),
        catch: (cause) => new UsageRouteError({ message: String(cause) }),
      });
      // @effect-diagnostics-next-line globalJsonInEffect:off
      return HttpServerResponse.text(JSON.stringify(out), {
        contentType: "application/json; charset=utf-8",
        headers: { "Cache-Control": "no-store" },
      });
    }).pipe(
      Effect.catchTags({
        EnvironmentAuthInvalidError: HttpServerRespondable.toResponse,
        EnvironmentInternalError: HttpServerRespondable.toResponse,
        EnvironmentScopeRequiredError: HttpServerRespondable.toResponse,
        UsageRouteError: (e) => Effect.succeed(HttpServerResponse.text(e.message, { status: 400 })),
      }),
      Effect.withTracerEnabled(false),
    ),
  );

/** Provider usage for the panel, the tray and linked-desktop reports. Metadata only; see @hanzodev/usage. */
export const layer = Layer.mergeAll(
  route("GET", "/v1/usage", AuthOrchestrationReadScope, async (r, _q, host, browser) =>
    BrowserSources.withBrowserSnapshots(await r.summary(host), await browser()),
  ),
  route("POST", "/v1/usage/refresh", AuthOrchestrationOperateScope, (r, _q, host) =>
    r.refresh(host),
  ),
  route("GET", "/v1/usage/tray", AuthOrchestrationReadScope, (r, _q, host) =>
    r.tray(host, new Date()),
  ),
  route("GET", "/v1/usage/settings", AuthOrchestrationReadScope, (r) => r.settings()),
  route("PUT", "/v1/usage/settings", AuthOrchestrationOperateScope, async (r, req) => {
    const next = decodeSettings(req.body);
    await r.saveSettings(next);
    return next;
  }),
  route("GET", "/v1/usage/report", AuthOrchestrationReadScope, (r, req, host) => {
    const q = new URL(req.url, "http://local").searchParams;
    const since = new Date(q.get("since") ?? Date.now() - 86_400_000);
    return r.report(host, q.get("deviceId") ?? "local", since);
  }),
).pipe(Layer.provide(BrowserSources.layer));
