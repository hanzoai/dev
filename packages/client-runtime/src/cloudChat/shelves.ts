// @effect-diagnostics globalFetch:off -- Cloud shelves are a plain promise client shared with React Native and the browser; it has no Effect runtime.
/**
 * What the `+` menu's cloud shelves list, from the same API hanzo.app reads:
 *   connectors      GET /v1/tool/mcp/servers   {servers: [{id, name, url}]}
 *   design systems  GET /v1/studio/brands      {data: [{id, name, tokens}]}
 *   memories        GET /v1/ai/memory/list     [{id, content}]  what Hanzo remembers, newest first
 */
import { mFallback, msg } from "gt-i18n";
import type { MFunctionType } from "gt-i18n/types";
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";

import {
  CloudChatError,
  type CloudChatAuth,
  type CloudChatOptions,
  resolveCloudChatOptions,
} from "./api.ts";

export interface CloudShelfRow {
  readonly id: string;
  readonly name: string;
  /** A few words beside the name: a server's host, a brand's colors. */
  readonly note: string;
}

const rowsOf = (value: unknown): ReadonlyArray<Record<string, unknown>> =>
  Array.isArray(value)
    ? value.filter(
        (row): row is Record<string, unknown> =>
          typeof row === "object" && row !== null && !Array.isArray(row),
      )
    : [];

const text = (value: unknown): string => (typeof value === "string" ? value.trim() : "");

const hostOf = (url: string): string => {
  try {
    return new URL(url).host;
  } catch {
    return "";
  }
};

export function parseConnectors(body: unknown): ReadonlyArray<CloudShelfRow> {
  const servers =
    typeof body === "object" && body !== null ? (body as { servers?: unknown }).servers : undefined;
  return rowsOf(servers).flatMap((row) => {
    const id = text(row.id);
    return id ? [{ id, name: text(row.name) || id, note: hostOf(text(row.url)) }] : [];
  });
}

export function parseDesignSystems(body: unknown): ReadonlyArray<CloudShelfRow> {
  const data =
    typeof body === "object" && body !== null ? (body as { data?: unknown }).data : undefined;
  return rowsOf(data).flatMap((row) => {
    const id = text(row.id);
    if (!id) return [];
    const tokens =
      typeof row.tokens === "object" && row.tokens !== null
        ? (row.tokens as Record<string, unknown>)
        : {};
    const colors = [tokens["--primary"], tokens["--accent"]].map(text).filter(Boolean).join(" ");
    return [{ id, name: text(row.name) || id, note: colors }];
  });
}

/** The most memories listed. */
const MEMORY_LIMIT = 20;

export function parseMemories(body: unknown): ReadonlyArray<CloudShelfRow> {
  const list = Array.isArray(body)
    ? body
    : typeof body === "object" && body !== null
      ? ((body as { data?: unknown }).data ?? (body as { memories?: unknown }).memories)
      : undefined;
  return rowsOf(list).flatMap((row, index) => {
    const content = text(row.content);
    return content ? [{ id: text(row.id) || String(index), name: content, note: "" }] : [];
  });
}

async function read(
  path: string,
  auth: CloudChatAuth,
  options: CloudChatOptions,
): Promise<unknown> {
  const token = auth.accessToken?.trim();
  if (!token) throw new CloudChatError("signed-out", "Not signed in");
  const { fetch, baseUrl } = resolveCloudChatOptions(options);
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

export async function listConnectors(auth: CloudChatAuth, options: CloudChatOptions = {}) {
  return parseConnectors(await read("/v1/tool/mcp/servers", auth, options));
}

export async function listDesignSystems(auth: CloudChatAuth, options: CloudChatOptions = {}) {
  return parseDesignSystems(await read("/v1/studio/brands", auth, options));
}

async function listMemories(auth: CloudChatAuth, options: CloudChatOptions = {}) {
  return parseMemories(await read(`/v1/ai/memory/list?limit=${MEMORY_LIMIT}`, auth, options));
}

const MEMORY_CONTEXT_MAX_CHARS = 30_000;

/** Formats memories from `GET /v1/ai/memory/list` into a system context block, or `undefined` if none. */
export function formatMemoriesSystemContext(
  memories: ReadonlyArray<CloudShelfRow>,
): string | undefined {
  const lines = memories.map((row) => row.name.trim()).filter(Boolean);
  if (lines.length === 0) return undefined;
  const header = "User memories:\n";
  let result = header;
  for (const line of lines) {
    const bullet = `- ${line}\n`;
    if (result.length + bullet.length > MEMORY_CONTEXT_MAX_CHARS) break;
    result += bullet;
  }
  const trimmed = result.trimEnd();
  return trimmed.length > header.trimEnd().length ? trimmed : undefined;
}

/**
 * Fetches the user's Hanzo memories and formats them as system context.
 * Returns `undefined` when signed out, empty, or unreachable so sending is never blocked.
 */
export async function loadMemoriesSystemContext(
  auth: CloudChatAuth,
  options: CloudChatOptions = {},
): Promise<string | undefined> {
  if (!auth.accessToken?.trim()) return undefined;
  try {
    return formatMemoriesSystemContext(await listMemories(auth, options));
  } catch {
    return undefined;
  }
}

/** What choosing `row` on `shelf` writes at the end of the draft, in `resolve`'s language. */
export function shelfDraftText(
  shelf: "connectors" | "design",
  row: CloudShelfRow,
  resolve: MFunctionType = mFallback,
): string {
  const name = row.name;
  return shelf === "connectors"
    ? resolve(msg("Use the {name} connector to ", { name }))
    : resolve(msg("Use the {name} design system: ", { name }));
}
