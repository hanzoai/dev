/**
 * Cloud chat with no environment. Same backend as hanzo.app's Chat:
 *   - replies   POST /v1/chat/completions (SSE, OpenAI-compatible)
 *   - models    GET  /v1/models           (chat-capable only: `writesReplies`)
 *   - threads   POST /v1/agents/chat/conversations {conversationId?, messages[]} -> {conversationId}
 *               (records turns without running a completion; `/v1/agent/chat` is the same route)
 *   - sandbox   POST /v1/dev/sessions     (`createCloudSession`)
 * Thread sync is best effort: a failure degrades to a local thread and never blocks sending.
 */
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";

import { createCloudSession, type DiscoveryFetch } from "../discovery/environments.ts";
import { writesReplies } from "../replyModels.ts";
import { createSseParser } from "./sse.ts";

export const CLOUD_CHAT_BASE_URL = "https://api.hanzo.ai";
export const FALLBACK_MODELS: ReadonlyArray<CloudChatModel> = [
  { id: "enso-flash", label: "enso-flash" },
  { id: "enso", label: "enso" },
  { id: "zen6-flash", label: "zen6-flash" },
];
export const DEFAULT_MODEL_ID = "enso-flash";

export interface CloudChatModel {
  readonly id: string;
  readonly label: string;
}

export interface CloudChatMessage {
  readonly role: "user" | "assistant";
  readonly content: string;
}

export interface CloudChatAuth {
  readonly accessToken: string | null;
  readonly orgId?: string | null;
}

export interface CloudChatOptions {
  readonly fetch?: DiscoveryFetch;
  readonly baseUrl?: string;
}

export type CloudChatErrorKind = "signed-out" | "unauthorized" | "unavailable" | "failed";

export class CloudChatError extends Error {
  readonly kind: CloudChatErrorKind;
  constructor(kind: CloudChatErrorKind, message: string) {
    super(message);
    this.name = "CloudChatError";
    this.kind = kind;
  }
}

const resolve = (options: CloudChatOptions) => ({
  fetch: options.fetch ?? ((input: string, init?: RequestInit) => globalThis.fetch(input, init)),
  baseUrl: (options.baseUrl ?? CLOUD_CHAT_BASE_URL).replace(/\/+$/, ""),
});

function requireToken(auth: CloudChatAuth): string {
  const token = auth.accessToken?.trim();
  if (!token) throw new CloudChatError("signed-out", "Not signed in");
  return token;
}

function failure(status: number): CloudChatError {
  if (status === 401 || status === 403) return new CloudChatError("unauthorized", `HTTP ${status}`);
  if (status === 404 || status === 501) return new CloudChatError("unavailable", `HTTP ${status}`);
  return new CloudChatError("failed", `HTTP ${status}`);
}

/** Chat-capable catalog models. Falls back to the known live set only when the catalog is empty. */
export function parseChatModels(body: unknown): ReadonlyArray<CloudChatModel> {
  const list =
    typeof body === "object" && body !== null ? (body as { data?: unknown }).data : undefined;
  if (!Array.isArray(list)) return [];
  const models: CloudChatModel[] = [];
  for (const entry of list) {
    if (typeof entry !== "object" || entry === null) continue;
    const model = entry as {
      id?: unknown;
      name?: unknown;
      endpoint?: unknown;
      outputs?: unknown;
    };
    if (typeof model.id !== "string" || model.id === "") continue;
    const outputs = Array.isArray(model.outputs)
      ? model.outputs.filter((value): value is string => typeof value === "string")
      : undefined;
    const endpoint = typeof model.endpoint === "string" ? model.endpoint : undefined;
    if (!writesReplies({ outputs, endpoint })) continue;
    models.push({
      id: model.id,
      label: typeof model.name === "string" && model.name !== "" ? model.name : model.id,
    });
  }
  return models;
}

export function pickDefaultModel(models: ReadonlyArray<CloudChatModel>): string {
  return models.find((model) => model.id === DEFAULT_MODEL_ID)?.id ?? models[0]?.id ?? DEFAULT_MODEL_ID;
}

export async function listChatModels(
  auth: CloudChatAuth,
  options: CloudChatOptions = {},
): Promise<ReadonlyArray<CloudChatModel>> {
  const token = requireToken(auth);
  const { fetch, baseUrl } = resolve(options);
  let response: Response;
  try {
    response = await fetch(`${baseUrl}/v1/models`, {
      headers: buildHanzoApiHeaders({ accessToken: token, orgId: auth.orgId }),
    });
  } catch {
    throw new CloudChatError("unavailable", "Network unavailable");
  }
  if (!response.ok) throw failure(response.status);
  const models = parseChatModels(await response.json().catch(() => null));
  return models.length > 0 ? models : FALLBACK_MODELS;
}

/** Streams one reply. Calls `onDelta` per text chunk and resolves to the full reply. */
export async function streamChatReply(input: {
  readonly auth: CloudChatAuth;
  readonly model: string;
  readonly messages: ReadonlyArray<CloudChatMessage>;
  readonly onDelta: (text: string) => void;
  readonly signal?: AbortSignal;
  readonly options?: CloudChatOptions;
}): Promise<string> {
  const token = requireToken(input.auth);
  const { fetch, baseUrl } = resolve(input.options ?? {});
  let response: Response;
  try {
    response = await fetch(`${baseUrl}/v1/chat/completions`, {
      method: "POST",
      headers: buildHanzoApiHeaders({
        accessToken: token,
        orgId: input.auth.orgId,
        extra: { "Content-Type": "application/json", Accept: "text/event-stream" },
      }),
      body: JSON.stringify({ model: input.model, messages: input.messages, stream: true }),
      ...(input.signal ? { signal: input.signal } : {}),
    });
  } catch {
    throw new CloudChatError("unavailable", "Network unavailable");
  }
  if (!response.ok) throw failure(response.status);

  const parser = createSseParser();
  let reply = "";
  let done = false;
  const apply = (events: ReturnType<typeof parser.push>) => {
    for (const event of events) {
      if (event.kind === "done") done = true;
      else {
        reply += event.text;
        input.onDelta(event.text);
      }
    }
  };
  const reader = response.body?.getReader();
  if (reader === undefined) {
    // React Native's fetch does not expose a stream: decode the whole body at once.
    apply(parser.push(await response.text()));
  } else {
    const decoder = new TextDecoder();
    while (!done) {
      const { value, done: finished } = await reader.read();
      if (finished) break;
      apply(parser.push(decoder.decode(value, { stream: true })));
    }
    if (done) void reader.cancel().catch(() => undefined);
  }
  apply(parser.flush());
  return reply;
}

/** Records turns in the hanzo.app thread store. Returns the thread id, or null when sync is unavailable. */
export async function recordTurns(input: {
  readonly auth: CloudChatAuth;
  readonly threadId: string | null;
  readonly messages: ReadonlyArray<CloudChatMessage>;
  readonly options?: CloudChatOptions;
}): Promise<string | null> {
  const token = input.auth.accessToken?.trim();
  if (!token) return null;
  const { fetch, baseUrl } = resolve(input.options ?? {});
  try {
    const response = await fetch(`${baseUrl}/v1/agents/chat/conversations`, {
      method: "POST",
      headers: buildHanzoApiHeaders({
        accessToken: token,
        orgId: input.auth.orgId,
        extra: { "Content-Type": "application/json" },
      }),
      body: JSON.stringify({
        ...(input.threadId ? { conversationId: input.threadId } : {}),
        messages: input.messages,
      }),
    });
    if (!response.ok) return null;
    const body = (await response.json()) as { conversationId?: unknown };
    return typeof body.conversationId === "string" && body.conversationId !== ""
      ? body.conversationId
      : null;
  } catch {
    return null;
  }
}

export type RunInCloudResult =
  | { readonly kind: "started"; readonly sessionId: string }
  | { readonly kind: "unavailable" }
  | { readonly kind: "failed"; readonly message: string };

/** "Run in the cloud": a sandbox session. A missing endpoint says so and chat carries on. */
export async function runInCloud(
  auth: CloudChatAuth,
  options: CloudChatOptions = {},
): Promise<RunInCloudResult> {
  const { session, report } = await createCloudSession(auth, options);
  if (session !== null) return { kind: "started", sessionId: session.id };
  if (report.status === "unavailable") return { kind: "unavailable" };
  return { kind: "failed", message: report.message ?? "Failed" };
}
