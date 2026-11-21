// Records the replay corpus for the pure deciders: inputs, and the outputs the TS functions give.
// Usage: node scripts/record-decide-corpus.ts <statev2.sqlite copy> <out.jsonl>
// The database is opened read-only and must be a copy. Nothing from it that identifies a user is
// written: thread ids become t0, t1, ... and only event types and runtime modes are kept.
import * as NodeSqlite from "node:sqlite";
import * as NodeFS from "node:fs";
import type { RuntimeMode } from "@hanzodev/contracts";
import { dispatchModeLimitFns } from "../src/orchestration-v2/DispatchModeLimit.replay.ts";

const [dbPath, outPath] = process.argv.slice(2);
if (!dbPath || !outPath) throw new Error("usage: record-decide-corpus.ts <db copy> <out.jsonl>");

const RUNTIME: RuntimeMode[] = ["approval-required", "auto-accept-edits", "auto", "full-access"];
const INTERACTION = ["plan", "default"] as const;
const modes = RUNTIME.flatMap((runtimeMode) =>
  INTERACTION.map((interactionMode) => ({ runtimeMode, interactionMode })),
);

const lines: string[] = [];
const record = (source: string, fn: string, input: unknown) =>
  lines.push(
    JSON.stringify({ source, fn, input, output: dispatchModeLimitFns[fn]?.(input) ?? null }),
  );

for (const limit of modes)
  for (const m of modes) record("exhaustive", "exceeded", { limit, modes: m });

for (const limit of [undefined, ...modes])
  for (const ceiling of [
    undefined,
    ...RUNTIME.map((r) => ({ organization: "o", maxRuntimeMode: r })),
  ])
    record("exhaustive", "narrow", { limit: limit ?? null, ceiling: ceiling ?? null });

// Real streams: the runtime-mode and run events of each thread, in sequence order.
const db = new NodeSqlite.DatabaseSync(dbPath, { readOnly: true });
const rows = db
  .prepare(
    `SELECT stream_id, event_type, payload_json FROM orchestration_events
     WHERE aggregate_kind = 'thread'
       AND event_type IN ('thread.created','thread.runtime-mode-updated','run.created','run-attempt.created')
     ORDER BY sequence`,
  )
  .all() as Array<{ stream_id: string; event_type: string; payload_json: string }>;
const alias = new Map<string, string>();
const streams = new Map<string, Array<Record<string, unknown>>>();
for (const row of rows) {
  if (!alias.has(row.stream_id)) alias.set(row.stream_id, `t${alias.size}`);
  const threadId = alias.get(row.stream_id) as string;
  const mode = (JSON.parse(row.payload_json) as { runtimeMode?: string }).runtimeMode;
  const event: Record<string, unknown> = { type: row.event_type, threadId };
  if (mode !== undefined) event.payload = { runtimeMode: mode };
  (streams.get(threadId) ?? streams.set(threadId, []).get(threadId))?.push(event);
}
db.close();

// Synthetic: every short sequence over the four event kinds, so every branch is reached.
const kinds = [
  (t: string, m: RuntimeMode) => ({
    type: "thread.created",
    threadId: t,
    payload: { runtimeMode: m },
  }),
  (t: string, m: RuntimeMode) => ({
    type: "thread.runtime-mode-updated",
    threadId: t,
    payload: { runtimeMode: m },
  }),
  (t: string) => ({ type: "run.created", threadId: t }),
  (t: string) => ({ type: "run-attempt.created", threadId: t }),
];
const synthetic: Array<Array<Record<string, unknown>>> = [];
for (const a of kinds.keys())
  for (const b of kinds.keys())
    for (const m1 of RUNTIME)
      for (const m2 of RUNTIME)
        synthetic.push([kinds[a]!("s", m1), kinds[b]!(b < 2 ? "s" : "u", m2)] as never);

const cases: Array<[string, Array<Record<string, unknown>>]> = [
  ...[...streams.values()].map((s) => ["real", s] as [string, Array<Record<string, unknown>>]),
  ...synthetic.map((s) => ["synthetic", s] as [string, Array<Record<string, unknown>>]),
];
for (const [source, events] of cases) {
  const known = [...new Set(events.map((e) => e.threadId as string))];
  record(source, "threadsStartingWork", { events });
  for (const max of RUNTIME)
    for (const committed of [
      [],
      ...(source === "real" ? RUNTIME : (["approval-required", "full-access"] as const)).map((r) =>
        known.map((t) => [t, r]),
      ),
    ]) {
      record(source, "ceilingRefusal", { max, events, runtimeModes: committed });
    }
}
NodeFS.writeFileSync(outPath, `${lines.join("\n")}\n`);
console.log(`${lines.length} cases`);
