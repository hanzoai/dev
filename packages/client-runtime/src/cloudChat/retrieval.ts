// @effect-diagnostics globalFetch:off globalDate:off globalTimers:off -- Cloud chat is a plain promise client shared with React Native and the browser.
/**
 * Files that go with a chat message, by reference: the file is uploaded to the org's workspace and
 * indexed (/v1/knowledge), and the turn carries the passages read from it for the question, each
 * citing file > section > paragraph. The block format is the one hanzo.app's Chat wrote, so a
 * conversation started there reads the same here (`said` takes the block back off for drawing).
 */
import { buildHanzoApiHeaders } from "@hanzodev/shared/hanzoIamAuth";

import { CloudChatError, resolveCloudChatOptions, type CloudChatAuth, type CloudChatOptions } from "./api.ts";

const OPEN = "<workspace-files>";
const CLOSE = "</workspace-files>";
/** One PUT carries the file; larger ones are asked to be attached by Drive. */
export const SINGLE_UPLOAD_LIMIT = 8 * 1024 * 1024;

export interface FileRef {
  readonly id: string;
  readonly name: string;
  readonly type: string;
  readonly size: number;
}

export interface WorkFile extends FileRef {
  readonly status: "queued" | "indexing" | "ready" | "stored" | "failed";
  readonly stage?: string;
  readonly passages?: number;
}

export interface Passage {
  readonly cite: string;
  readonly text: string;
}

export interface Grounds {
  readonly passages: ReadonlyArray<Passage>;
}

const slug = (name: string): string =>
  name.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 40).replace(/-+$/, "") || "workspace";

const settled = (file: WorkFile) => file.status === "ready" || file.status === "stored" || file.status === "failed";
const askable = (file: WorkFile) =>
  file.status === "ready" ||
  (file.status === "indexing" && (file.stage === "embed" || file.stage === "graph") && (file.passages ?? 0) > 0);

interface Api {
  json: <T>(path: string, init?: { method?: string; body?: unknown }) => Promise<T>;
}

function api(auth: CloudChatAuth, options?: CloudChatOptions): Api {
  const token = auth.accessToken?.trim();
  if (!token) throw new CloudChatError("signed-out", "Sign in to attach files");
  const { fetch, baseUrl } = resolveCloudChatOptions(options ?? {});
  return {
    json: async <T>(path: string, init: { method?: string; body?: unknown } = {}) => {
      const response = await fetch(`${baseUrl}${path}`, {
        method: init.method ?? "GET",
        headers: buildHanzoApiHeaders({
          accessToken: token,
          orgId: auth.orgId,
          extra: { "Content-Type": "application/json" },
        }),
        ...(init.body === undefined ? {} : { body: JSON.stringify(init.body) }),
      });
      if (!response.ok) throw new CloudChatError(response.status === 401 || response.status === 403 ? "unauthorized" : "failed", `HTTP ${response.status}`);
      return (response.status === 204 ? undefined : await response.json()) as T;
    },
  };
}

/** The workspace's bucket, named for the org and made on first use. */
export async function workspaceBucket(auth: CloudChatAuth, options?: CloudChatOptions): Promise<string> {
  const http = api(auth, options);
  const name = slug(auth.orgId ?? "workspace");
  const out = await http.json<{ buckets?: Array<{ name: string }> }>("/v1/s3/buckets");
  if (!(out?.buckets ?? []).some((bucket) => bucket.name === name)) {
    await http.json("/v1/s3/buckets", { method: "POST", body: { name } }).catch((cause: unknown) => {
      // Two sends racing the first upload both make it; the loser's conflict is the bucket existing.
      if (!/409|exist/i.test(String((cause as Error)?.message ?? cause))) throw cause;
    });
  }
  return name;
}

/** Uploads one file to the workspace and queues its ingest. Answers the record. */
export async function attachFile(
  auth: CloudChatAuth,
  file: { readonly name: string; readonly type: string; readonly bytes: Blob },
  options?: CloudChatOptions,
): Promise<WorkFile> {
  if (file.bytes.size >= SINGLE_UPLOAD_LIMIT)
    throw new CloudChatError("failed", `${file.name} is over 8 MB: add it in Drive, then ask about it`);
  const http = api(auth, options);
  const bucket = await workspaceBucket(auth, options);
  const key = `chat/${Date.now().toString(36)}-${file.name.replace(/[^A-Za-z0-9._-]+/g, "_")}`;
  const grant = await http.json<{ url?: string }>(`/v1/s3/buckets/${encodeURIComponent(bucket)}/objects`, {
    method: "POST",
    body: { bucket, key },
  });
  if (!grant?.url) throw new CloudChatError("failed", "The store issued no upload address");
  const { fetch } = resolveCloudChatOptions(options ?? {});
  const put = await fetch(grant.url, {
    method: "PUT",
    ...(file.type ? { headers: { "Content-Type": file.type } } : {}),
    body: file.bytes,
  });
  if (!put.ok) throw new CloudChatError("failed", `The store answered ${put.status}`);
  return http.json<WorkFile>("/v1/knowledge/files", { method: "POST", body: { bucket, key } });
}

/** Waits, at most `ms`, for each file to have passages to read or to settle; answers what the index last said. */
export async function readableFiles(
  auth: CloudChatAuth,
  files: ReadonlyArray<WorkFile>,
  ms: number,
  options?: CloudChatOptions,
  sleep: (ms: number) => Promise<void> = (wait) => new Promise((done) => setTimeout(done, wait)),
  now: () => number = Date.now,
): Promise<ReadonlyArray<WorkFile>> {
  const http = api(auth, options);
  const until = now() + ms;
  let held = files.slice();
  while (now() < until && held.some((file) => !askable(file) && !settled(file))) {
    await sleep(1200);
    held = await Promise.all(
      held.map((file) =>
        askable(file) || settled(file)
          ? file
          : http.json<WorkFile>(`/v1/knowledge/files/${encodeURIComponent(file.id)}`).catch(() => file),
      ),
    );
  }
  return held;
}

/** The passages that answer `query`, read from `files`. */
export function retrieve(
  auth: CloudChatAuth,
  query: string,
  files: ReadonlyArray<string>,
  limit = 8,
  options?: CloudChatOptions,
): Promise<Grounds> {
  return api(auth, options).json<Grounds>("/v1/knowledge/files/retrieve", {
    method: "POST",
    body: { query, files, limit },
  });
}

const inert = (text: string) => text.split(CLOSE).join("</workspace files>");

function weigh(bytes: number): string {
  if (!Number.isFinite(bytes) || bytes < 0) return "";
  if (bytes < 1024) return `${bytes} B`;
  const units = ["KB", "MB", "GB", "TB"];
  let n = bytes / 1024;
  let u = 0;
  while (n >= 1024 && u < units.length - 1) {
    n /= 1024;
    u++;
  }
  return `${n >= 10 ? Math.round(n) : n.toFixed(1)} ${units[u]}`;
}

/** The turn as sent: the words, then one block naming the files and holding the passages read for this question. */
export function compose(
  text: string,
  carried: { readonly attached: ReadonlyArray<FileRef>; readonly reused: ReadonlyArray<FileRef> },
  grounds: Grounds | null,
  why?: string,
): string {
  const words = text.trim();
  const all = [...carried.attached, ...carried.reused];
  if (all.length === 0) return words;
  const passages = grounds?.passages ?? [];
  const lines = [
    OPEN,
    JSON.stringify({ attached: carried.attached, reused: carried.reused }),
    `The person's workspace holds these files: ${all.map((f) => `${f.name} (${f.type || "file"}, ${weigh(f.size)})`).join("; ")}.`,
  ];
  if (passages.length > 0) {
    lines.push(
      words
        ? "Answer from the passages below, read from those files for this question. Cite each claim with its number, like [1], and end with a Sources list giving each passage you used."
        : "The person sent these files without a question: say what each one is and what it covers, from the passages below, citing them like [1].",
    );
    passages.forEach((p, i) => lines.push("", `[${i + 1}] ${inert(p.cite)}`, inert(p.text.trim())));
  } else {
    lines.push(
      `No passages could be read from them${why ? ` (${why})` : ""}: say so, and answer only what you can without them.`,
    );
  }
  lines.push(CLOSE);
  const block = lines.join("\n");
  return words ? `${words}\n\n${block}` : block;
}

export interface Said {
  readonly text: string;
  readonly carried: { readonly attached: ReadonlyArray<FileRef>; readonly reused: ReadonlyArray<FileRef> } | null;
  /** The citations the turn handed the model, numbered as it numbered them. */
  readonly cites: ReadonlyArray<string>;
}

const isRef = (value: unknown): value is FileRef =>
  typeof value === "object" && value !== null && typeof (value as FileRef).id === "string" && typeof (value as FileRef).name === "string";

/** Takes the files block back off a turn, for drawing. */
export function said(content: string): Said {
  const at = content.startsWith(`${OPEN}\n`) ? 0 : content.lastIndexOf(`\n${OPEN}\n`);
  if (at < 0) return { text: content, carried: null, cites: [] };
  const start = content.indexOf(OPEN, at) + OPEN.length + 1;
  const end = content.indexOf(CLOSE, start);
  const body = content.slice(start, end < 0 ? undefined : end);
  const nl = body.indexOf("\n");
  try {
    const head = JSON.parse(nl < 0 ? body : body.slice(0, nl)) as { attached?: unknown; reused?: unknown };
    const carried = {
      attached: Array.isArray(head.attached) ? head.attached.filter(isRef) : [],
      reused: Array.isArray(head.reused) ? head.reused.filter(isRef) : [],
    };
    const cites = [...body.matchAll(/^\[(\d+)\] (.+)$/gm)].map((match) => match[2]!);
    return { text: content.slice(0, at).trimEnd(), carried, cites };
  } catch {
    return { text: content, carried: null, cites: [] };
  }
}

/** Every file the conversation's turns have carried, once each, oldest first. */
export function carriedIn(turns: ReadonlyArray<{ readonly role: string; readonly content: string }>): ReadonlyArray<FileRef> {
  const out = new Map<string, FileRef>();
  for (const turn of turns) {
    if (turn.role !== "user") continue;
    const carried = said(turn.content).carried;
    for (const file of [...(carried?.attached ?? []), ...(carried?.reused ?? [])]) if (!out.has(file.id)) out.set(file.id, file);
  }
  return [...out.values()];
}
