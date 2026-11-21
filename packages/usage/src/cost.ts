/**
 * Local cost scan: reads the session logs Claude Code and Codex already write, counts tokens per model per
 * day, prices them from the table below. Reads files only; sends nothing anywhere. Message text is never read
 * into the result.
 *
 * Log layouts and the dedupe rule are ported from CodexBar (MIT, Copyright (c) 2026 Peter Steinberger); see NOTICE.
 */
import type { UsageEnv } from "./env.ts";
import type { CostDay, UsageEvent } from "./schema.ts";

/** USD per million tokens. An estimate: models absent here are reported unpriced, never as zero. */
export interface Price {
  readonly match: RegExp;
  readonly input: number;
  readonly output: number;
  readonly cacheRead: number;
  readonly cacheWrite: number;
}

export const PRICES: ReadonlyArray<Price> = [
  { match: /opus-4-[5-9]|opus-5/, input: 5, output: 25, cacheRead: 0.5, cacheWrite: 6.25 },
  { match: /opus/, input: 15, output: 75, cacheRead: 1.5, cacheWrite: 18.75 },
  { match: /sonnet/, input: 3, output: 15, cacheRead: 0.3, cacheWrite: 3.75 },
  { match: /haiku-4/, input: 1, output: 5, cacheRead: 0.1, cacheWrite: 1.25 },
  { match: /haiku/, input: 0.8, output: 4, cacheRead: 0.08, cacheWrite: 1 },
  { match: /gpt-5.*mini/, input: 0.25, output: 2, cacheRead: 0.025, cacheWrite: 0.25 },
  { match: /gpt-5/, input: 1.25, output: 10, cacheRead: 0.125, cacheWrite: 1.25 },
];

export const priceOf = (
  e: Pick<
    UsageEvent,
    "model" | "inputTokens" | "outputTokens" | "cacheReadTokens" | "cacheWriteTokens"
  >,
): number | undefined => {
  const p = PRICES.find((x) => x.match.test(e.model));
  if (!p) return undefined;
  return (
    (e.inputTokens * p.input +
      e.outputTokens * p.output +
      e.cacheReadTokens * p.cacheRead +
      e.cacheWriteTokens * p.cacheWrite) /
    1_000_000
  );
};

const rec = (v: unknown): Record<string, unknown> =>
  v && typeof v === "object" ? (v as Record<string, unknown>) : {};
const n = (v: unknown): number => (typeof v === "number" && Number.isFinite(v) ? v : 0);
const lines = (text: string) =>
  text.split("\n").flatMap((l) => {
    if (!l.trim()) return [];
    try {
      return [rec(JSON.parse(l))];
    } catch {
      return [];
    }
  });

export const parseClaudeLog = (text: string, file: string): UsageEvent[] => {
  const seen = new Set<string>();
  const out: UsageEvent[] = [];
  for (const row of lines(text)) {
    if (row.type !== "assistant") continue;
    const msg = rec(row.message);
    const usage = rec(msg.usage);
    if (
      typeof msg.model !== "string" ||
      typeof row.timestamp !== "string" ||
      !Object.keys(usage).length
    )
      continue;
    const id = `${String(msg.id ?? "")}:${String(row.requestId ?? "")}`;
    if (id !== ":") {
      if (seen.has(id)) continue; // streamed turns repeat the same message
      seen.add(id);
    }
    const e = {
      id: id !== ":" ? id : `${file}:${out.length}`,
      provider: "claude",
      model: msg.model,
      at: row.timestamp,
      inputTokens: n(usage.input_tokens),
      outputTokens: n(usage.output_tokens),
      cacheReadTokens: n(usage.cache_read_input_tokens),
      cacheWriteTokens: n(usage.cache_creation_input_tokens),
      ...(typeof row.sessionId === "string" ? { sessionId: row.sessionId } : {}),
    };
    const costUsd = priceOf(e);
    out.push({ ...e, ...(costUsd !== undefined ? { costUsd } : {}) });
  }
  return out;
};

export const parseCodexLog = (text: string, file: string): UsageEvent[] => {
  let model = "gpt-5";
  const out: UsageEvent[] = [];
  for (const row of lines(text)) {
    const p = rec(row.payload);
    if (row.type === "turn_context" && typeof p.model === "string") model = p.model;
    if (row.type !== "event_msg" || p.type !== "token_count" || typeof row.timestamp !== "string")
      continue;
    const last = rec(rec(p.info).last_token_usage);
    if (!Object.keys(last).length) continue;
    const cached = n(last.cached_input_tokens);
    const e = {
      id: `${file}:${out.length}`,
      provider: "codex",
      model,
      at: row.timestamp,
      inputTokens: Math.max(0, n(last.input_tokens) - cached),
      outputTokens: n(last.output_tokens),
      cacheReadTokens: cached,
      cacheWriteTokens: 0,
      sessionId: file,
    };
    const costUsd = priceOf(e);
    out.push({ ...e, ...(costUsd !== undefined ? { costUsd } : {}) });
  }
  return out;
};

export const scanEvents = async (env: UsageEnv, since: Date): Promise<UsageEvent[]> => {
  const out: UsageEvent[] = [];
  const claudeDir = env.env.CLAUDE_CONFIG_DIR?.trim() || `${env.home}/.claude`;
  const codexDir = env.env.CODEX_HOME?.trim() || `${env.home}/.codex`;
  for (const f of await env.listFiles(`${claudeDir}/projects`, ".jsonl")) {
    out.push(...parseClaudeLog((await env.readText(f)) ?? "", f));
  }
  for (const f of await env.listFiles(`${codexDir}/sessions`, ".jsonl")) {
    out.push(...parseCodexLog((await env.readText(f)) ?? "", f));
  }
  return out.filter((e) => Date.parse(e.at) >= since.getTime());
};

export const rollupDays = (events: ReadonlyArray<UsageEvent>): CostDay[] => {
  const by = new Map<string, CostDay>();
  for (const e of events) {
    const day = e.at.slice(0, 10);
    const key = `${day}|${e.provider}|${e.model}`;
    const prev = by.get(key);
    const tokens = e.cacheReadTokens + e.cacheWriteTokens;
    const cost = e.costUsd === undefined ? prev?.costUsd : (prev?.costUsd ?? 0) + e.costUsd;
    by.set(key, {
      day,
      provider: e.provider,
      model: e.model,
      inputTokens: (prev?.inputTokens ?? 0) + e.inputTokens,
      outputTokens: (prev?.outputTokens ?? 0) + e.outputTokens,
      cacheTokens: (prev?.cacheTokens ?? 0) + tokens,
      ...(cost !== undefined ? { costUsd: cost } : {}),
    });
  }
  return [...by.values()].sort((a, b) =>
    a.day < b.day ? 1 : a.day > b.day ? -1 : a.model.localeCompare(b.model),
  );
};
