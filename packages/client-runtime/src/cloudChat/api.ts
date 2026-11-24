// @effect-diagnostics extendsNativeError:off globalDate:off globalFetch:off -- Cloud chat is a plain promise client shared with React Native and the browser; it has no Effect runtime.
/**
 * Cloud chat with no environment, on hanzo.app Chat's backend: `/v1/chat/completions` (SSE),
 * `/v1/models`, and `/v1/agents/chat/conversations`, which records turns without running a
 * completion. Thread sync is best effort: a failure degrades to a local thread, never a blocked send.
 */
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";
import { mFallback, msg } from "gt-i18n";

import { createCloudSession, type DiscoveryFetch } from "../discovery/environments.ts";
import { writesReplies } from "../replyModels.ts";
import { createSseParser } from "./sse.ts";

export const CLOUD_CHAT_BASE_URL = "https://api.hanzo.ai";
/* oxlint-disable hanzo/no-untranslated-copy -- model names, spelled as their makers ship them */
export const FALLBACK_MODELS: ReadonlyArray<CloudChatModel> = [
  { id: "enso-flash", label: "enso-flash", provider: "hanzo" },
  { id: "enso", label: "enso", provider: "hanzo" },
  { id: "zen6", label: "Zen 6", provider: "hanzo" },
  { id: "zen6-flash", label: "Zen 6 Flash", provider: "hanzo" },
  { id: "zen6-coder", label: "Zen 6 Coder", provider: "hanzo" },
  { id: "claude-opus-4-6", label: "Claude Opus 4.6", provider: "anthropic" },
  { id: "claude-sonnet-4-6", label: "Claude Sonnet 4.6", provider: "anthropic" },
  { id: "claude-haiku-4-5", label: "Claude Haiku 4.5", provider: "anthropic" },
  { id: "gpt-5.4", label: "GPT-5.4", provider: "openai" },
  { id: "gpt-5.4-mini", label: "GPT-5.4 Mini", provider: "openai" },
  { id: "o3", label: "o3", provider: "openai" },
  { id: "gemini-2.5-pro", label: "Gemini 2.5 Pro", provider: "google" },
  { id: "gemini-2.5-flash", label: "Gemini 2.5 Flash", provider: "google" },
  { id: "grok-4", label: "Grok 4", provider: "xai" },
  { id: "deepseek-r1", label: "DeepSeek R1", provider: "deepseek" },
];
/* oxlint-enable hanzo/no-untranslated-copy */
export const DEFAULT_MODEL_ID = "enso-flash";
/** Auto: the model id that lets the gateway route each turn (hanzo.app Chat's `ENSO`). */
const AUTO_MODEL_ID = "enso-auto";

/** Auto first: it is always offered, whether or not the catalog lists the router. */
export function withAuto(models: ReadonlyArray<CloudChatModel>): ReadonlyArray<CloudChatModel> {
  return [
    {
      id: AUTO_MODEL_ID,
      label: mFallback(msg("Auto", { $context: "model choice: the gateway picks the model" })),
    },
    ...models.filter((model) => model.id !== AUTO_MODEL_ID),
  ];
}

export interface CloudChatModel {
  readonly id: string;
  readonly label: string;
  /** Who serves the model (`owned_by` / `provider`), when the catalog says. */
  readonly provider?: string | undefined;
  /** What the model reads, from the catalog (`inputs`): absent when the catalog does not say. */
  readonly inputs?: ReadonlyArray<string> | undefined;
}

/** What a user message carries besides its text, already read into what the wire needs. */
export type CloudChatAttachment =
  | { readonly kind: "image"; readonly dataUrl: string }
  /** A text file, sent inline as text. */
  | { readonly kind: "text"; readonly name: string; readonly text: string }
  /** A file the model reads itself, such as a PDF: a base64 data URL. */
  | { readonly kind: "file"; readonly name: string; readonly dataUrl: string };

export type WebSearchMode = "off" | "auto" | "on";

export interface WebSearchResult {
  readonly title: string;
  readonly url: string;
  readonly snippet: string;
}

export interface CloudChatMessage {
  readonly role: "user" | "assistant";
  readonly content: string;
  /** Only on the request being sent: threads store and sync the text alone. */
  readonly attachments?: ReadonlyArray<CloudChatAttachment> | undefined;
}

type WirePart =
  | { readonly type: "text"; readonly text: string }
  | { readonly type: "image_url"; readonly image_url: { readonly url: string } }
  | {
      readonly type: "file";
      readonly file: { readonly filename: string; readonly file_data: string };
    };

/** OpenAI-compatible chat message: a string, or content parts when the message has attachments. */
export function toWireMessage(message: CloudChatMessage): {
  readonly role: "user" | "assistant";
  readonly content: string | ReadonlyArray<WirePart>;
} {
  const attachments = message.attachments ?? [];
  if (attachments.length === 0) return { role: message.role, content: message.content };
  const parts: WirePart[] = [{ type: "text", text: message.content }];
  for (const attachment of attachments) {
    if (attachment.kind === "image")
      parts.push({ type: "image_url", image_url: { url: attachment.dataUrl } });
    else if (attachment.kind === "text")
      parts.push({ type: "text", text: `${attachment.name}:\n${attachment.text}` });
    else
      parts.push({
        type: "file",
        file: { filename: attachment.name, file_data: attachment.dataUrl },
      });
  }
  return { role: message.role, content: parts };
}

export interface CloudChatAuth {
  readonly accessToken: string | null;
  readonly orgId?: string | null;
  /** Scopes the device copy of threads; absent means one shared anonymous scope. */
  readonly userId?: string | null;
}

export interface CloudChatOptions {
  readonly fetch?: DiscoveryFetch;
  readonly baseUrl?: string;
}

export type CloudChatErrorKind =
  | "signed-out"
  | "unauthorized"
  | "unavailable"
  | "failed"
  /** A local model was chosen and the machine that serves it is not connected. */
  | "offline";

export class CloudChatError extends Error {
  readonly kind: CloudChatErrorKind;
  constructor(kind: CloudChatErrorKind, message: string) {
    super(message);
    this.name = "CloudChatError";
    this.kind = kind;
  }
}

export const resolveCloudChatOptions = (options: CloudChatOptions) => ({
  fetch: options.fetch ?? ((input: string, init?: RequestInit) => globalThis.fetch(input, init)),
  baseUrl: (options.baseUrl ?? CLOUD_CHAT_BASE_URL).replace(/\/+$/, ""),
});

function requireToken(auth: CloudChatAuth): string {
  const token = auth.accessToken?.trim();
  if (!token) throw new CloudChatError("signed-out", "Not signed in");
  return token;
}

export function httpFailure(status: number): CloudChatError {
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
      inputs?: unknown;
      input_modalities?: unknown;
      architecture?: { input_modalities?: unknown } | null;
      owned_by?: unknown;
      provider?: unknown;
    };
    if (typeof model.id !== "string" || model.id === "") continue;
    const outputs = Array.isArray(model.outputs)
      ? model.outputs.filter((value): value is string => typeof value === "string")
      : undefined;
    const endpoint = typeof model.endpoint === "string" ? model.endpoint : undefined;
    if (!writesReplies({ outputs, endpoint })) continue;
    const strings = (value: unknown) =>
      Array.isArray(value)
        ? value.filter((item): item is string => typeof item === "string")
        : undefined;
    const inputs =
      strings(model.inputs) ??
      strings(model.input_modalities) ??
      strings(model.architecture?.input_modalities);
    const owner = [model.owned_by, model.provider].find(
      (value): value is string => typeof value === "string" && value.trim() !== "",
    );
    models.push({
      id: model.id,
      label: typeof model.name === "string" && model.name !== "" ? model.name : model.id,
      ...(owner === undefined ? {} : { provider: owner.trim() }),
      ...(inputs === undefined ? {} : { inputs }),
    });
  }
  return models;
}

export function pickDefaultModel(models: ReadonlyArray<CloudChatModel>): string {
  return (
    models.find((model) => model.id === DEFAULT_MODEL_ID)?.id ?? models[0]?.id ?? DEFAULT_MODEL_ID
  );
}

export async function listChatModels(
  auth: CloudChatAuth,
  options: CloudChatOptions = {},
): Promise<ReadonlyArray<CloudChatModel>> {
  const token = auth.accessToken?.trim() ?? "";
  const { fetch, baseUrl } = resolveCloudChatOptions(options);
  const headers = token
    ? buildHanzoApiHeaders({ accessToken: token, orgId: auth.orgId })
    : undefined;
  let response: Response;
  try {
    response = await fetch(`${baseUrl}/v1/models`, headers ? { headers } : undefined);
  } catch {
    return withAuto(FALLBACK_MODELS);
  }
  if (!response.ok) return withAuto(FALLBACK_MODELS);
  const models = parseChatModels(await response.json().catch(() => null));
  return withAuto(models.length > 0 ? models : FALLBACK_MODELS);
}

/** Reads an OpenAI-compatible SSE reply, calling `onDelta` per chunk; resolves to the full reply. */
export async function readSseReply(
  response: Response,
  onDelta: (text: string) => void,
): Promise<string> {
  const parser = createSseParser();
  let reply = "";
  let done = false;
  const apply = (events: ReturnType<typeof parser.push>) => {
    for (const event of events) {
      if (event.kind === "done") done = true;
      else {
        reply += event.text;
        onDelta(event.text);
      }
    }
  };
  const reader = response.body?.getReader();
  if (reader === undefined) {
    // A fetch without a streaming body (React Native's built-in): decode it all at once.
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

const WEB_SEARCH_EXPLICIT_RE =
  /\b(?:search the web|web search|search online|look up online|look it up|google it|browse the web|cite source|cite sources)\b/i;
const WEB_SEARCH_FRESHNESS_RE =
  /\b(?:latest|newest|current|today|yesterday|this week|this month|this year|recent|recently|news|weather|price of|stock price|score|stable version|release notes|released)\b/i;
const WEB_SEARCH_GREETING_RE =
  /^(?:hi|hello|hey|thanks|thank you|ok|okay|yo|sup|good morning|good afternoon|good evening)[!.?\s]*$/i;

/** Decides whether a user prompt should run live web search before completion. */
export function needsWebSearch(prompt: string, mode: WebSearchMode = "off"): boolean {
  const trimmed = prompt.trim();
  if (trimmed === "" || mode === "off") return false;
  if (mode === "on") return true;
  if (WEB_SEARCH_GREETING_RE.test(trimmed)) return false;
  if (WEB_SEARCH_EXPLICIT_RE.test(trimmed)) return true;
  if (/https?:\/\/\S+/i.test(trimmed)) return true;
  // Skip pure code blocks unless the user explicitly asked for web search above.
  if (trimmed.includes("```")) return false;
  return WEB_SEARCH_FRESHNESS_RE.test(trimmed);
}

/** Extracts a concise search query from the user's message. */
export function webSearchQueryFor(prompt: string): string {
  const cleaned = prompt
    .replace(/^(?:please\s+)?(?:search the web|web search|search online|look up)\s*[:,-]?\s*/i, "")
    .replace(/\b(?:cite sources?|with sources?|with links?)\b[.!?]*/gi, "")
    .replace(/\s+/g, " ")
    .trim();
  return (cleaned || prompt.trim()).slice(0, 300);
}

/** Runs live web search against the gateway's `/v1/websearch` endpoint. */
async function searchWeb(
  auth: CloudChatAuth,
  query: string,
  options: CloudChatOptions & { readonly signal?: AbortSignal | undefined } = {},
): Promise<ReadonlyArray<WebSearchResult>> {
  const token = auth.accessToken?.trim();
  const q = query.trim();
  if (!token || q === "") return [];
  const { fetch, baseUrl } = resolveCloudChatOptions(options);
  try {
    const response = await fetch(`${baseUrl}/v1/websearch`, {
      method: "POST",
      headers: buildHanzoApiHeaders({
        accessToken: token,
        orgId: auth.orgId,
        extra: { "Content-Type": "application/json", Accept: "application/json" },
      }),
      body: JSON.stringify({ q, limit: 6 }),
      ...(options.signal ? { signal: options.signal } : {}),
    });
    if (!response.ok) return [];
    const body = (await response.json().catch(() => null)) as {
      results?: ReadonlyArray<Record<string, unknown>>;
      data?: ReadonlyArray<Record<string, unknown>>;
    } | null;
    const rows = Array.isArray(body?.results)
      ? body.results
      : Array.isArray(body?.data)
        ? body.data
        : [];
    const out: WebSearchResult[] = [];
    for (const row of rows) {
      const url = typeof row.url === "string" ? row.url.trim() : "";
      if (!url) continue;
      const title =
        (typeof row.title === "string" && row.title.trim()) ||
        (typeof row.name === "string" && row.name.trim()) ||
        url;
      const snippet =
        (typeof row.snippet === "string" && row.snippet.trim()) ||
        (typeof row.description === "string" && row.description.trim()) ||
        (typeof row.content === "string" && row.content.trim().slice(0, 400)) ||
        "";
      out.push({ title, url, snippet });
    }
    return out;
  } catch {
    return [];
  }
}

/** Streams one reply. Calls `onDelta` per text chunk and resolves to the full reply. */
export async function streamChatReply(input: {
  readonly auth: CloudChatAuth;
  readonly model: string;
  readonly messages: ReadonlyArray<CloudChatMessage>;
  /** Reasoning effort for models that take one; omitted when unset. */
  readonly effort?: string | null | undefined;
  /** A system message ahead of the turns (a persona's instructions); never stored in the thread. */
  readonly system?: string | undefined;
  /** Live web search mode ("off" | "auto" | "on"). Runs `/v1/websearch` before streaming when triggered. */
  readonly webSearch?: WebSearchMode | undefined;
  /** Current ISO date (YYYY-MM-DD) for grounding time-sensitive questions. */
  readonly today?: string | undefined;
  readonly onDelta: (text: string) => void;
  readonly signal?: AbortSignal | undefined;
  readonly options?: CloudChatOptions | undefined;
}): Promise<string> {
  const token = requireToken(input.auth);
  const { fetch, baseUrl } = resolveCloudChatOptions(input.options ?? {});
  let system = input.system?.trim() ?? "";
  const webMode = input.webSearch ?? "off";
  if (webMode !== "off") {
    const today = input.today ?? new Date().toISOString().slice(0, 10);
    const grounding: string[] = [
      `Today is ${today}. Answer directly in plain text/markdown and never output raw JSON tool-call objects.`,
    ];
    const lastUser = [...input.messages].reverse().find((m) => m.role === "user");
    if (lastUser && needsWebSearch(lastUser.content, webMode)) {
      const q = webSearchQueryFor(lastUser.content);
      const results = await searchWeb(input.auth, q, {
        ...input.options,
        signal: input.signal,
      });
      if (results.length > 0) {
        const formatted = results
          .map((r, idx) => `[${idx + 1}] ${r.title} (${r.url})${r.snippet ? `\n${r.snippet}` : ""}`)
          .join("\n\n");
        grounding.push(
          `Live web search results for "${q}":\n\n${formatted}\n\nUse these live results to answer accurately and cite sources with markdown links.`,
        );
      }
    }
    system = system ? `${system}\n\n${grounding.join("\n\n")}` : grounding.join("\n\n");
  }
  let response: Response;
  try {
    response = await fetch(`${baseUrl}/v1/chat/completions`, {
      method: "POST",
      headers: buildHanzoApiHeaders({
        accessToken: token,
        orgId: input.auth.orgId,
        extra: { "Content-Type": "application/json", Accept: "text/event-stream" },
      }),
      body: JSON.stringify({
        model: input.model,
        messages: [
          ...(system ? [{ role: "system" as const, content: system }] : []),
          ...input.messages.map(toWireMessage),
        ],
        stream: true,
        ...(input.effort ? { reasoning_effort: input.effort } : {}),
      }),
      ...(input.signal ? { signal: input.signal } : {}),
    });
  } catch (cause) {
    if (input.signal?.aborted || (cause instanceof Error && cause.name === "AbortError")) {
      throw cause;
    }
    throw new CloudChatError("unavailable", "Network unavailable");
  }
  if (!response.ok) throw httpFailure(response.status);

  return readSseReply(response, input.onDelta);
}

/** Records turns in the hanzo.app thread store. Returns the thread id, or null when sync is unavailable. */
export async function recordTurns(input: {
  readonly auth: CloudChatAuth;
  readonly threadId: string | null;
  readonly messages: ReadonlyArray<CloudChatMessage>;
  readonly options?: CloudChatOptions | undefined;
}): Promise<string | null> {
  const token = input.auth.accessToken?.trim();
  if (!token) return null;
  const { fetch, baseUrl } = resolveCloudChatOptions(input.options ?? {});
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
        messages: input.messages.map((m) => ({ role: m.role, content: m.content })),
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
  // oxlint-disable-next-line hanzo/no-untranslated-copy -- diagnostic detail; clients render their own copy from `kind`
  return { kind: "failed", message: report.message ?? "Failed" };
}
