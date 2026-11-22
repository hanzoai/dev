// @effect-diagnostics globalFetch:off -- Local chat is a plain promise client shared with React Native and the browser; it has no Effect runtime.
/**
 * Chat served by the user's linked Mac. The phone never talks to its engines; it asks the Mac's Dev
 * server over the environment connection it holds: `GET /v1/local-ai/models` and
 * `POST /v1/local-ai/chat` (SSE). A local model never reaches api.hanzo.ai, and a missing Mac is an
 * error, never a cloud reply.
 */
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";

import type { DiscoveryFetch } from "../discovery/environments.ts";
import {
  CloudChatError,
  httpFailure,
  readSseReply,
  toWireMessage,
  type CloudChatMessage,
} from "./api.ts";

export const LOCAL_CHAT_DRIVER = "local";
const SLUG_PREFIX = "local:";

export interface LocalChatModel {
  /** The id the engine serves the model under. */
  readonly id: string;
  /** The engine that serves it (`Hanzo Engine`, `Ollama`, ...), when the Mac says. */
  readonly engine?: string | undefined;
}

/** The linked Mac, as chat sees it. */
export interface LocalChatHost {
  readonly models: (signal?: AbortSignal) => Promise<ReadonlyArray<LocalChatModel>>;
  readonly stream: (input: {
    readonly model: string;
    readonly messages: ReadonlyArray<CloudChatMessage>;
    readonly onDelta: (text: string) => void;
    readonly signal?: AbortSignal | undefined;
  }) => Promise<string>;
}

/** The picker slug of a local model: kept apart from every cloud id. */
export const localChatSlug = (id: string): string => `${SLUG_PREFIX}${id}`;

export const isLocalChatSlug = (slug: string): boolean => slug.startsWith(SLUG_PREFIX);

export const localChatModelId = (slug: string): string => slug.slice(SLUG_PREFIX.length);

/** The models of a `/v1/local-ai/models` body; rows without an id are dropped. */
export function parseLocalChatModels(body: unknown): ReadonlyArray<LocalChatModel> {
  const list =
    typeof body === "object" && body !== null ? (body as { data?: unknown }).data : undefined;
  if (!Array.isArray(list)) return [];
  const seen = new Set<string>();
  const models: LocalChatModel[] = [];
  for (const entry of list) {
    const row = entry as { id?: unknown; engine?: unknown } | null;
    if (typeof row?.id !== "string" || row.id === "" || seen.has(row.id)) continue;
    seen.add(row.id);
    models.push({
      id: row.id,
      ...(typeof row.engine === "string" && row.engine !== "" ? { engine: row.engine } : {}),
    });
  }
  return models;
}

/**
 * The host over HTTP at the Mac's environment address. `fetch` carries the connection's own
 * authorization; `bearer` is for a connection that presents one as a plain header.
 */
export function createLocalChatHost(input: {
  readonly baseUrl: string;
  readonly bearer: string | null;
  readonly fetch: DiscoveryFetch;
}): LocalChatHost {
  const base = input.baseUrl.replace(/\/+$/, "");
  const headers = (extra?: Record<string, string>) =>
    buildHanzoApiHeaders({ accessToken: input.bearer, extra });
  const request = async (path: string, init: RequestInit): Promise<Response> => {
    let response: Response;
    try {
      response = await input.fetch(`${base}${path}`, init);
    } catch {
      throw new CloudChatError("offline", "The Mac is not reachable");
    }
    if (!response.ok) throw httpFailure(response.status);
    return response;
  };
  return {
    models: async (signal) =>
      parseLocalChatModels(
        await (
          await request("/v1/local-ai/models", {
            headers: headers(),
            ...(signal ? { signal } : {}),
          })
        )
          .json()
          .catch(() => null),
      ),
    stream: async ({ model, messages, onDelta, signal }) =>
      readSseReply(
        await request("/v1/local-ai/chat", {
          method: "POST",
          headers: headers({ "Content-Type": "application/json", Accept: "text/event-stream" }),
          body: JSON.stringify({ model, messages: messages.map(toWireMessage), stream: true }),
          ...(signal ? { signal } : {}),
        }),
        onDelta,
      ),
  };
}
