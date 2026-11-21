/**
 * Saved cloud threads. Same store `recordTurns` writes to:
 *   - list  GET /v1/agents/chat/conversations       -> summaries (id, title, last append)
 *   - read  GET /v1/agents/chat/conversations/{id}  -> every message in order
 */
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";

import {
  CLOUD_CHAT_BASE_URL,
  CloudChatError,
  type CloudChatAuth,
  type CloudChatMessage,
  type CloudChatOptions,
} from "./api.ts";

export interface CloudChatThread {
  readonly id: string;
  readonly title: string;
  readonly updatedAt: string | null;
}

const record = (value: unknown): Record<string, unknown> | null =>
  typeof value === "object" && value !== null ? (value as Record<string, unknown>) : null;

const text = (value: unknown): string | null =>
  typeof value === "string" && value !== "" ? value : null;

/** Thread summaries from a list body: a bare array, or one under `conversations` / `data`. */
export function parseChatThreads(body: unknown): ReadonlyArray<CloudChatThread> {
  const wrapper = record(body);
  const list = Array.isArray(body) ? body : (wrapper?.conversations ?? wrapper?.data);
  if (!Array.isArray(list)) return [];
  const threads: CloudChatThread[] = [];
  for (const entry of list) {
    const thread = record(entry);
    const id = text(thread?.id) ?? text(thread?.conversationId);
    if (thread === null || id === null) continue;
    threads.push({
      id,
      title: text(thread.title) ?? "",
      updatedAt: text(thread.updatedAt) ?? text(thread.updated_at) ?? null,
    });
  }
  return threads;
}

/** User and assistant messages of one thread body, in order. Tool and system turns are skipped. */
export function parseChatThreadMessages(body: unknown): ReadonlyArray<CloudChatMessage> {
  const list = record(body)?.messages;
  if (!Array.isArray(list)) return [];
  const messages: CloudChatMessage[] = [];
  for (const entry of list) {
    const message = record(entry);
    if (message === null) continue;
    const role = message.role;
    if ((role !== "user" && role !== "assistant") || typeof message.content !== "string") continue;
    if (message.content === "") continue;
    messages.push({ role, content: message.content });
  }
  return messages;
}

async function getJson(
  path: string,
  auth: CloudChatAuth,
  options: CloudChatOptions,
): Promise<unknown> {
  const token = auth.accessToken?.trim();
  if (!token) throw new CloudChatError("signed-out", "Not signed in");
  const fetch =
    options.fetch ?? ((input: string, init?: RequestInit) => globalThis.fetch(input, init));
  const baseUrl = (options.baseUrl ?? CLOUD_CHAT_BASE_URL).replace(/\/+$/, "");
  let response: Response;
  try {
    response = await fetch(`${baseUrl}${path}`, {
      headers: buildHanzoApiHeaders({ accessToken: token, orgId: auth.orgId }),
    });
  } catch {
    throw new CloudChatError("unavailable", "Network unavailable");
  }
  if (response.status === 401 || response.status === 403)
    throw new CloudChatError("unauthorized", `HTTP ${response.status}`);
  if (response.status === 404 || response.status === 501)
    throw new CloudChatError("unavailable", `HTTP ${response.status}`);
  if (!response.ok) throw new CloudChatError("failed", `HTTP ${response.status}`);
  return response.json().catch(() => null);
}

export async function listChatThreads(
  auth: CloudChatAuth,
  options: CloudChatOptions = {},
): Promise<ReadonlyArray<CloudChatThread>> {
  return parseChatThreads(await getJson("/v1/agents/chat/conversations", auth, options));
}

export async function loadChatThread(
  auth: CloudChatAuth,
  threadId: string,
  options: CloudChatOptions = {},
): Promise<ReadonlyArray<CloudChatMessage>> {
  return parseChatThreadMessages(
    await getJson(`/v1/agents/chat/conversations/${encodeURIComponent(threadId)}`, auth, options),
  );
}
