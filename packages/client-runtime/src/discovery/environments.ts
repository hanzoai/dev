// @effect-diagnostics globalFetch:off -- Discovery is a plain promise client shared with React Native and the browser.
/**
 * Environment discovery after Hanzo IAM sign-in: linked machines (`/v1/sources`),
 * an offered cloud session (`/v1/dev/sessions`), and saved/relay environments.
 * Cloud sessions are only created by an explicit `createCloudSession` call.
 */

const DEFAULT_DISCOVERY_API_BASE_URL = "https://api.hanzo.ai";

export type DiscoveryFetch = (input: string, init?: RequestInit) => Promise<Response>;

export interface DiscoveryAuth {
  readonly accessToken: string | null;
  readonly orgId?: string | null;
}

export interface DiscoveryOptions {
  readonly fetch?: DiscoveryFetch;
  readonly baseUrl?: string;
}

export type DiscoverySourceStatus = "ok" | "unavailable" | "error";

export interface LinkedMachine {
  readonly id: string;
  readonly kind: "desktop" | "browser";
  readonly name: string;
  readonly os: string;
  readonly arch: string;
  readonly version: string;
  readonly state: "online" | "offline" | "revoked";
  readonly lastSeen: string | null;
  readonly linkedAt: string;
}

export interface CloudSession {
  readonly id: string;
  readonly label: string;
  readonly zap: string;
  readonly sandbox?: string;
}

/** An environment the app already knows: saved by the user or registered through the relay. */
export interface KnownEnvironment {
  readonly id: string;
  readonly label: string;
  readonly origin: "saved" | "relay";
}

export type DiscoveredEnvironment =
  | {
      readonly type: "linked";
      readonly id: string;
      readonly label: string;
      readonly machine: LinkedMachine;
    }
  | {
      readonly type: "cloud";
      readonly id: string;
      readonly label: string;
      readonly session: CloudSession | null;
    }
  | { readonly type: "saved" | "relay"; readonly id: string; readonly label: string };

export interface DiscoveryReport {
  readonly status: DiscoverySourceStatus;
  readonly message?: string;
}

export interface EnvironmentDiscovery {
  readonly environments: ReadonlyArray<DiscoveredEnvironment>;
  readonly sources: DiscoveryReport;
  /** Cloud sessions are offered, never spawned: `ok` means the offer is available. */
  readonly cloud: DiscoveryReport;
  /** True when no linked machine exists: show "Link a Mac from Hanzo Dev on desktop". */
  readonly showLinkPrompt: boolean;
}

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

const str = (value: unknown): string | null => (typeof value === "string" ? value : null);

function parseMachine(value: unknown): LinkedMachine | null {
  if (!isRecord(value)) return null;
  const id = str(value.id);
  const kind = value.kind;
  const state = value.state;
  if (id === null || id === "") return null;
  if (kind !== "desktop" && kind !== "browser") return null;
  if (state !== "online" && state !== "offline" && state !== "revoked") return null;
  return {
    id,
    kind,
    name: str(value.name) ?? id,
    os: str(value.os) ?? "",
    arch: str(value.arch) ?? "",
    version: str(value.version) ?? "",
    state,
    lastSeen: str(value.last_seen),
    linkedAt: str(value.linked_at) ?? "",
  };
}

function parseSession(value: unknown): CloudSession | null {
  if (!isRecord(value)) return null;
  const id = str(value.id);
  const zap = str(value.zap);
  if (id === null || id === "" || zap === null) return null;
  const sandbox = str(value.sandbox);
  return {
    id,
    label: str(value.label) ?? "Cloud",
    zap,
    ...(sandbox === null ? {} : { sandbox }),
  };
}

const headers = (
  auth: DiscoveryAuth & { readonly accessToken: string },
): Record<string, string> => ({
  authorization: `Bearer ${auth.accessToken}`,
  "content-type": "application/json",
  ...(auth.orgId ? { "x-hanzo-org": auth.orgId } : {}),
});

const resolve = (options: DiscoveryOptions) => ({
  fetch: options.fetch ?? ((input: string, init?: RequestInit) => globalThis.fetch(input, init)),
  baseUrl: (options.baseUrl ?? DEFAULT_DISCOVERY_API_BASE_URL).replace(/\/+$/, ""),
});

/** 401/403 and server errors are `error`; a missing or unimplemented route or no network is `unavailable`. */
function classify(status: number): DiscoverySourceStatus {
  return status === 404 || status === 501 ? "unavailable" : "error";
}

async function listLinkedMachines(
  auth: DiscoveryAuth,
  options: DiscoveryOptions = {},
): Promise<{ readonly machines: ReadonlyArray<LinkedMachine>; readonly report: DiscoveryReport }> {
  if (!auth.accessToken)
    return { machines: [], report: { status: "error", message: "Not signed in" } };
  const { fetch, baseUrl } = resolve(options);
  let response: Response;
  try {
    response = await fetch(`${baseUrl}/v1/sources`, {
      method: "GET",
      headers: headers({ ...auth, accessToken: auth.accessToken }),
    });
  } catch {
    return { machines: [], report: { status: "unavailable" } };
  }
  if (!response.ok) {
    return {
      machines: [],
      report: { status: classify(response.status), message: `HTTP ${response.status}` },
    };
  }
  let body: unknown;
  try {
    body = await response.json();
  } catch {
    return { machines: [], report: { status: "error", message: "Invalid response" } };
  }
  const raw =
    isRecord(body) && Array.isArray(body.sources) ? (body.sources as ReadonlyArray<unknown>) : null;
  if (raw === null)
    return { machines: [], report: { status: "error", message: "Invalid response" } };
  const machines = raw
    .map(parseMachine)
    .filter((machine): machine is LinkedMachine => machine !== null && machine.state !== "revoked");
  return { machines, report: { status: "ok" } };
}

/** Explicit user action: creates (or reuses) a cloud session. Never called at sign-in. */
export async function createCloudSession(
  auth: DiscoveryAuth,
  options: DiscoveryOptions = {},
): Promise<{ readonly session: CloudSession | null; readonly report: DiscoveryReport }> {
  if (!auth.accessToken)
    return { session: null, report: { status: "error", message: "Not signed in" } };
  const { fetch, baseUrl } = resolve(options);
  let response: Response;
  try {
    response = await fetch(`${baseUrl}/v1/dev/sessions`, {
      method: "POST",
      headers: headers({ ...auth, accessToken: auth.accessToken }),
      body: JSON.stringify({ runtime: "auto" }),
    });
  } catch {
    return { session: null, report: { status: "unavailable" } };
  }
  if (!response.ok) {
    return {
      session: null,
      report: { status: classify(response.status), message: `HTTP ${response.status}` },
    };
  }
  let body: unknown;
  try {
    body = await response.json();
  } catch {
    return { session: null, report: { status: "error", message: "Invalid response" } };
  }
  const session = parseSession(body);
  return session === null
    ? { session: null, report: { status: "error", message: "Invalid response" } }
    : { session, report: { status: "ok" } };
}

/**
 * Pure merge. Order: linked, then cloud, then saved/relay. The first entry for an id wins,
 * so a saved/relay record of a linked machine is dropped.
 */
export function mergeEnvironments(input: {
  readonly machines: ReadonlyArray<LinkedMachine>;
  readonly cloud: CloudSession | "offer" | null;
  readonly known: ReadonlyArray<KnownEnvironment>;
}): ReadonlyArray<DiscoveredEnvironment> {
  const out: Array<DiscoveredEnvironment> = [];
  const seen = new Set<string>();
  const push = (environment: DiscoveredEnvironment) => {
    if (seen.has(environment.id)) return;
    seen.add(environment.id);
    out.push(environment);
  };
  for (const machine of input.machines) {
    push({ type: "linked", id: machine.id, label: machine.name, machine });
  }
  if (input.cloud === "offer") {
    push({ type: "cloud", id: "cloud", label: "Run in the cloud", session: null });
  } else if (input.cloud !== null) {
    push({ type: "cloud", id: input.cloud.id, label: input.cloud.label, session: input.cloud });
  }
  for (const known of input.known) {
    push({ type: known.origin, id: known.id, label: known.label });
  }
  return out;
}

/**
 * Lists linked machines, offers a cloud environment (only when no machine is linked), and merges
 * saved/relay environments. Makes no POST; a missing token yields `error` for sources and cloud.
 */
export async function discoverEnvironments(
  auth: DiscoveryAuth,
  known: ReadonlyArray<KnownEnvironment>,
  options: DiscoveryOptions = {},
): Promise<EnvironmentDiscovery> {
  const { machines, report } = await listLinkedMachines(auth, options);
  const signedIn = Boolean(auth.accessToken);
  const cloudReport: DiscoveryReport = !signedIn
    ? { status: "error", message: "Not signed in" }
    : { status: report.status === "unavailable" ? "unavailable" : "ok" };
  const offer = cloudReport.status === "ok" && machines.length === 0;
  return {
    environments: mergeEnvironments({ machines, cloud: offer ? "offer" : null, known }),
    sources: report,
    cloud: cloudReport,
    showLinkPrompt: machines.length === 0,
  };
}
