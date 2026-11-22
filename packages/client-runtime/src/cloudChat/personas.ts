// @effect-diagnostics globalFetch:off -- Cloud chat is a plain promise client shared with React Native and the browser.
/**
 * Personas: the org's agents, as hanzo.app's Chat picks them. A persona's instructions ride as the
 * turn's system message and the model it is pinned to answers. The roster is `GET /v1/agents`;
 * `GET /v1/agents/{name}` adds `instructions`.
 */
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";

import {
  CloudChatError,
  resolveCloudChatOptions,
  type CloudChatAuth,
  type CloudChatOptions,
} from "./api.ts";

export interface Persona {
  readonly name: string;
  readonly model: string | null;
  readonly description: string | null;
  readonly emoji: string | null;
}

export interface PersonaRecord extends Persona {
  readonly instructions: string | null;
}

const NAME = /^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$/;
const text = (value: unknown): string | null =>
  typeof value === "string" && value.trim() !== "" ? value : null;

function toPersona(raw: unknown): PersonaRecord | null {
  if (typeof raw !== "object" || raw === null) return null;
  const row = raw as Record<string, unknown>;
  const name = text(row.name);
  if (name === null) return null;
  const look =
    typeof row.appearance === "object" && row.appearance !== null
      ? (row.appearance as Record<string, unknown>)
      : {};
  return {
    name,
    model: text(row.model),
    description: text(row.description),
    emoji: text(row.emoji) ?? text(look.emoji),
    instructions: text(row.instructions),
  };
}

async function read(
  auth: CloudChatAuth,
  path: string,
  options?: CloudChatOptions,
): Promise<unknown> {
  const token = auth.accessToken?.trim();
  if (!token) throw new CloudChatError("signed-out", "Sign in to use personas");
  const { fetch, baseUrl } = resolveCloudChatOptions(options ?? {});
  const response = await fetch(`${baseUrl}${path}`, {
    headers: buildHanzoApiHeaders({ accessToken: token, orgId: auth.orgId }),
  });
  if (!response.ok) throw new CloudChatError("unavailable", `Personas answered ${response.status}`);
  return response.json();
}

/** The org's personas, by name. A refusal or an unreadable answer is an empty roster, never a failed chat. */
export async function listPersonas(
  auth: CloudChatAuth,
  options?: CloudChatOptions,
): Promise<ReadonlyArray<Persona>> {
  try {
    const body = (await read(auth, "/v1/agents", options)) as { agents?: unknown };
    const rows = Array.isArray(body?.agents) ? body.agents : [];
    return rows.flatMap((row) => {
      const persona = toPersona(row);
      return persona === null ? [] : [persona];
    });
  } catch {
    return [];
  }
}

/** One persona's record, with its instructions; null when it cannot be read. */
export async function readPersona(
  auth: CloudChatAuth,
  name: string,
  options?: CloudChatOptions,
): Promise<PersonaRecord | null> {
  if (!NAME.test(name)) return null;
  try {
    return toPersona(await read(auth, `/v1/agents/${name}`, options));
  } catch {
    return null;
  }
}
