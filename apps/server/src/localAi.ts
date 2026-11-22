// @effect-diagnostics globalFetch:off -- Local engines are plain OpenAI-compatible HTTP servers on this machine.
/**
 * The phone's local chat from this machine's engines (Hanzo Engine through hanzod, then Ollama and
 * LM Studio): `GET /v1/local-ai/models` lists them, `POST /v1/local-ai/chat` relays the engine's SSE
 * reply unchanged. Nothing here reaches past loopback: a model no engine here serves is an error.
 */
import { AuthOrchestrationOperateScope } from "@hanzodev/contracts";
import {
  BUILT_IN_LOCAL_ENGINES,
  isLocalEngineInstanceId,
  probeLocalEngines,
  type LocalEngineStatus,
} from "@hanzodev/shared/localAi";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Stream from "effect/Stream";
import {
  HttpRouter,
  HttpServerRequest,
  HttpServerRespondable,
  HttpServerResponse,
} from "effect/http";

import { authenticateRawRouteWithScope } from "./http.ts";
import { ServerSettingsService } from "./serverSettings.ts";

const LOCAL_AI_MODELS_PATH = "/v1/local-ai/models";
const LOCAL_AI_CHAT_PATH = "/v1/local-ai/chat";

/** Every engine that answers, in Local AI's order, with the models it serves. */
const onlineEngines = (): Promise<ReadonlyArray<LocalEngineStatus>> =>
  // Hanzo Engine starts on its first request and loads Metal shaders, so give it time.
  probeLocalEngines(BUILT_IN_LOCAL_ENGINES, { timeoutMs: 30_000 }).then((all) =>
    all.filter((s) => s.online),
  );

/** The `{data}` body of the models route: the first engine to serve an id owns it. */
export function modelsBody(engines: ReadonlyArray<LocalEngineStatus>) {
  const seen = new Set<string>();
  const data: Array<{ id: string; engine: string }> = [];
  for (const status of engines) {
    for (const model of status.models) {
      if (seen.has(model.id)) continue;
      seen.add(model.id);
      data.push({ id: model.id, engine: status.engine.name });
    }
  }
  return { data };
}

/** The base URL of the first engine that serves `model`, or null. */
export const engineFor = (
  engines: ReadonlyArray<LocalEngineStatus>,
  model: string,
): string | null =>
  engines.find((status) => status.models.some((m) => m.id === model))?.baseUrl ?? null;

/** `{model, messages}` of a chat body, or null when it is not one. */
export function parseChatBody(
  body: unknown,
): { model: string; messages: ReadonlyArray<unknown> } | null {
  const row = body as { model?: unknown; messages?: unknown } | null;
  if (typeof row?.model !== "string" || row.model === "") return null;
  if (!Array.isArray(row.messages) || row.messages.length === 0) return null;
  return { model: row.model, messages: row.messages };
}

const failures = {
  EnvironmentAuthInvalidError: HttpServerRespondable.toResponse,
  EnvironmentInternalError: HttpServerRespondable.toResponse,
  EnvironmentScopeRequiredError: HttpServerRespondable.toResponse,
} as const;

const jsonBody = (body: unknown): string => JSON.stringify(body);

const json = (status: number, body: unknown) =>
  HttpServerResponse.text(jsonBody(body), {
    status,
    contentType: "application/json; charset=utf-8",
    headers: { "Cache-Control": "no-store" },
  });

/** Opens the engine's streamed reply; null when the engine does not answer. */
const openChat = (
  base: string,
  chat: { model: string; messages: ReadonlyArray<unknown> },
  signal: AbortSignal,
): Promise<Response | null> =>
  fetch(`${base}/chat/completions`, {
    method: "POST",
    headers: { "Content-Type": "application/json", Accept: "text/event-stream" },
    body: jsonBody({ model: chat.model, messages: chat.messages, stream: true }),
    signal,
  }).catch(() => null);

const modelsRoute = HttpRouter.add(
  "GET",
  LOCAL_AI_MODELS_PATH,
  Effect.gen(function* () {
    yield* authenticateRawRouteWithScope(AuthOrchestrationOperateScope);
    const engines = yield* Effect.promise(onlineEngines);
    return json(200, modelsBody(engines));
  }).pipe(Effect.catchTags(failures), Effect.withTracerEnabled(false)),
);

const chatRoute = HttpRouter.add(
  "POST",
  LOCAL_AI_CHAT_PATH,
  Effect.gen(function* () {
    yield* authenticateRawRouteWithScope(AuthOrchestrationOperateScope);
    const request = yield* HttpServerRequest.HttpServerRequest;
    const parsed = parseChatBody(yield* request.json.pipe(Effect.orElseSucceed(() => null)));
    if (parsed === null) return json(400, { error: "body must be {model, messages, stream}" });
    const engines = yield* Effect.promise(onlineEngines);
    const base = engineFor(engines, parsed.model);
    if (base === null) {
      return json(404, { error: `no local engine on this machine serves ${parsed.model}` });
    }
    const upstream = yield* Effect.promise((signal) => openChat(base, parsed, signal));
    if (upstream === null || upstream.body === null) {
      return json(502, { error: "the local engine did not answer" });
    }
    if (!upstream.ok)
      return json(upstream.status, { error: "the local engine refused the request" });
    return HttpServerResponse.stream(
      Stream.fromAsyncIterable(
        upstream.body as unknown as AsyncIterable<Uint8Array>,
        (cause) => cause,
      ),
      {
        status: 200,
        contentType: "text/event-stream",
        headers: { "Cache-Control": "no-store, no-transform" },
      },
    );
  }).pipe(Effect.catchTags(failures), Effect.withTracerEnabled(false)),
);

/**
 * Hanzo Engine starts on its first request and then loads the model, so a user who added a local
 * engine would wait for that on their first message. Ask once, shortly after the server is up.
 */
const warmEngine = Layer.effectDiscard(
  Effect.gen(function* () {
    const settings = yield* ServerSettingsService;
    yield* Effect.sleep("5 seconds").pipe(
      Effect.andThen(settings.getSettings),
      Effect.flatMap((all) =>
        Object.keys(all.providerInstances ?? {}).some(isLocalEngineInstanceId)
          ? Effect.promise(onlineEngines)
          : Effect.void,
      ),
      Effect.ignore,
      Effect.forkScoped,
    );
  }),
);

export const layer = Layer.mergeAll(modelsRoute, chatRoute, warmEngine);
