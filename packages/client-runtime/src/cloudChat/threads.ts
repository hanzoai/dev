/**
 * Cloud chat threads. Remote store (shared with hanzo.app Chat):
 *   list  GET /v1/agents/chat/conversations       -> {conversations: [{id, title, updatedAt}]}
 *   read  GET /v1/agents/chat/conversations/<id>  -> {id, messages: [{id, role, content}]}
 * A local copy (injected `ThreadStore`) keeps a thread across restarts when sync fails.
 */
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";

import {
  type CloudChatAuth,
  type CloudChatMessage,
  type CloudChatOptions,
  resolveCloudChatOptions,
} from "./api.ts";

export interface StoredThread {
  /** Stable local key; equals the remote id for a thread first seen remotely. */
  readonly key: string;
  readonly remoteId: string | null;
  readonly title: string;
  readonly updatedAt: number;
  readonly messages: ReadonlyArray<CloudChatMessage>;
  /** How many leading messages the remote store already holds. */
  readonly synced: number;
  readonly pinned?: boolean;
  /** The user chose this title; a remote title never replaces it. */
  readonly renamed?: boolean;
  /** Hidden on this device (and from the remote list) after Delete. */
  readonly deleted?: boolean;
}

export interface ThreadSummary {
  readonly key: string;
  readonly remoteId: string | null;
  readonly title: string;
  readonly updatedAt: number;
  /** A device copy exists, so it reopens without the network. */
  readonly local: boolean;
  readonly pinned: boolean;
  readonly renamed: boolean;
}

export interface ThreadStore {
  load(owner: string): Promise<ReadonlyArray<StoredThread>>;
  save(owner: string, thread: StoredThread): Promise<void>;
}

export const MAX_STORED_THREADS = 50;
const TITLE_LENGTH = 48;

export function threadTitle(messages: ReadonlyArray<CloudChatMessage>): string {
  const first = messages.find((message) => message.role === "user")?.content.trim() ?? "";
  const line = first.split("\n")[0] ?? "";
  return line.length > TITLE_LENGTH ? `${line.slice(0, TITLE_LENGTH).trimEnd()}…` : line;
}

export function summaryOf(thread: StoredThread): ThreadSummary {
  return {
    key: thread.key,
    remoteId: thread.remoteId,
    title: thread.title,
    updatedAt: thread.updatedAt,
    local: true,
    pinned: thread.pinned === true,
    renamed: thread.renamed === true,
  };
}

/**
 * Local and remote threads as one list, newest first. A remote thread a local copy already
 * names collapses into it (the newer title and time win); the rest are remote only.
 */
export function mergeThreads(
  local: ReadonlyArray<ThreadSummary>,
  remote: ReadonlyArray<ThreadSummary>,
): ReadonlyArray<ThreadSummary> {
  const byRemote = new Map<string, ThreadSummary>();
  const out: ThreadSummary[] = [];
  for (const thread of local) {
    out.push(thread);
    if (thread.remoteId !== null) byRemote.set(thread.remoteId, thread);
  }
  for (const thread of remote) {
    const id = thread.remoteId;
    const mine = id === null ? undefined : byRemote.get(id);
    if (mine === undefined) {
      out.push(thread);
      continue;
    }
    if (thread.updatedAt > mine.updatedAt) {
      out[out.indexOf(mine)] = {
        ...mine,
        title: !mine.renamed && thread.title !== "" ? thread.title : mine.title,
        updatedAt: thread.updatedAt,
      };
    }
  }
  return out
    .toSorted((a, b) => Number(b.pinned) - Number(a.pinned) || b.updatedAt - a.updatedAt)
    .slice(0, MAX_STORED_THREADS);
}

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

function toMillis(value: unknown): number {
  if (typeof value === "number" && Number.isFinite(value)) return value;
  if (typeof value === "string") {
    const parsed = Date.parse(value);
    if (!Number.isNaN(parsed)) return parsed;
  }
  return 0;
}

export function parseThreadList(body: unknown): ReadonlyArray<ThreadSummary> {
  const list = isRecord(body) ? body.conversations : undefined;
  if (!Array.isArray(list)) return [];
  const out: ThreadSummary[] = [];
  for (const entry of list) {
    if (!isRecord(entry) || typeof entry.id !== "string" || entry.id === "") continue;
    out.push({
      key: entry.id,
      remoteId: entry.id,
      title: typeof entry.title === "string" ? entry.title : "",
      updatedAt: toMillis(entry.updatedAt),
      local: false,
      pinned: false,
      renamed: false,
    });
  }
  return out;
}

export function parseThreadMessages(body: unknown): ReadonlyArray<CloudChatMessage> {
  const list = isRecord(body) ? body.messages : undefined;
  if (!Array.isArray(list)) return [];
  const out: CloudChatMessage[] = [];
  for (const entry of list) {
    if (!isRecord(entry) || typeof entry.content !== "string") continue;
    if (entry.role !== "user" && entry.role !== "assistant") continue;
    out.push({ role: entry.role, content: entry.content });
  }
  return out;
}

async function get(
  path: string,
  auth: CloudChatAuth,
  options: CloudChatOptions,
): Promise<unknown | null> {
  const token = auth.accessToken?.trim();
  if (!token) return null;
  const { fetch, baseUrl } = resolveCloudChatOptions(options);
  try {
    const response = await fetch(`${baseUrl}${path}`, {
      headers: buildHanzoApiHeaders({ accessToken: token, orgId: auth.orgId }),
    });
    if (!response.ok) return null;
    return await response.json();
  } catch {
    return null;
  }
}

/** The account's remote threads. Any failure (404/501 included) is an empty list. */
export async function listRemoteThreads(
  auth: CloudChatAuth,
  options: CloudChatOptions = {},
): Promise<ReadonlyArray<ThreadSummary>> {
  return parseThreadList(await get("/v1/agents/chat/conversations", auth, options));
}

/** One remote thread's turns, or null when it cannot be read. */
export async function readRemoteThread(
  auth: CloudChatAuth,
  remoteId: string,
  options: CloudChatOptions = {},
): Promise<ReadonlyArray<CloudChatMessage> | null> {
  const body = await get(
    `/v1/agents/chat/conversations/${encodeURIComponent(remoteId)}`,
    auth,
    options,
  );
  const messages = parseThreadMessages(body);
  return messages.length > 0 ? messages : null;
}

/** Best-effort remote delete. False when the backend has no such route or refuses. */
export async function deleteRemoteThread(
  auth: CloudChatAuth,
  remoteId: string,
  options: CloudChatOptions = {},
): Promise<boolean> {
  const token = auth.accessToken?.trim();
  if (!token) return false;
  const { fetch, baseUrl } = resolveCloudChatOptions(options);
  try {
    const response = await fetch(
      `${baseUrl}/v1/agents/chat/conversations/${encodeURIComponent(remoteId)}`,
      {
        method: "DELETE",
        headers: buildHanzoApiHeaders({ accessToken: token, orgId: auth.orgId }),
      },
    );
    return response.ok;
  } catch {
    return false;
  }
}
