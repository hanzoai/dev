import type { DevCloud } from "./runtime";

/** What `POST /v1/platform/apps` answers (the platform's own words, never ours). */
export interface Declared {
  /** The build this write started; its record says when it is green. */
  readonly build: { readonly id: string; readonly status: string } | null;
  /** A repository in the org's own code workspace goes live when its build does. */
  readonly live: boolean;
  /** On a branch the change is a review: nothing deploys until it merges. */
  readonly review: string | null;
  readonly notice: string | null;
}

export interface PublishSource {
  /** The https clone URL. */
  readonly repo: string;
  readonly ref: string;
  /** The app name to declare. */
  readonly name: string;
  readonly project?: string;
}

const https = (url: unknown): string | null =>
  typeof url === "string" && /^https:\/\/[^\s]+$/.test(url) ? url : null;

/** A DNS-1123 label from anything: what an app may be named. */
export const label = (s: string): string =>
  s
    .toLowerCase()
    .replace(/[^a-z0-9-]+/g, "-")
    .replace(/-+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 63)
    .replace(/-+$/g, "") || "app";

const record = (value: unknown): Record<string, unknown> =>
  value !== null && typeof value === "object" ? (value as Record<string, unknown>) : {};

async function call(cloud: DevCloud, method: "GET" | "POST", path: string, body?: unknown) {
  const response = await fetch(`${cloud.api}${path}`, {
    method,
    headers: {
      Authorization: `Bearer ${await cloud.token()}`,
      ...(cloud.org ? { "X-Org-Id": cloud.org } : {}),
      ...(body === undefined ? {} : { "Content-Type": "application/json" }),
    },
    ...(body === undefined ? {} : { body: JSON.stringify(body) }),
  });
  const text = await response.text();
  const json: unknown = text ? JSON.parse(text) : {};
  if (!response.ok) {
    const said = record(json).error ?? record(json).message;
    throw new Error(typeof said === "string" && said ? said : `The platform answered ${response.status}.`);
  }
  return record(json);
}

/** Builds the repository on a branch and declares it in a project: a review, not a deploy. */
export async function publish(cloud: DevCloud, source: PublishSource): Promise<Declared> {
  const raw = await call(cloud, "POST", "/v1/platform/apps", {
    repo: source.repo,
    ref: source.ref || "main",
    name: label(source.name),
    partOf: label(source.project ?? source.name),
    mode: "branch",
  });
  const build = record(raw.build);
  const declaration = record(raw.declaration);
  return {
    build: typeof build.id === "string" ? { id: build.id, status: String(build.status ?? "") } : null,
    live: declaration.live === true,
    review: https(declaration.review),
    notice: typeof raw.notice === "string" ? raw.notice : null,
  };
}

/** The status the platform last reported for one build. */
export async function buildStatus(cloud: DevCloud, id: string): Promise<string | null> {
  const raw = await call(cloud, "GET", "/v1/platform/builds");
  const rows = Array.isArray(raw.builds) ? (raw.builds as unknown[]) : [];
  const row = rows.map(record).find((b) => b.id === id);
  return row ? String(row.status ?? "") : null;
}

export const isGreen = (status: string): boolean =>
  /^(succeeded|success|done|complete|completed|ok)$/i.test(status);
