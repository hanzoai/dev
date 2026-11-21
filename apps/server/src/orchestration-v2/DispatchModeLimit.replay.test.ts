import * as NodeFS from "node:fs";
import { assert, it } from "@effect/vitest";
import { type DecideCase, dispatchModeLimitFns } from "./DispatchModeLimit.replay.ts";

const corpus = new URL(
  "../../../../crates/orchestrator/corpus/dispatch_mode_limit.jsonl",
  import.meta.url,
);

it("replays the recorded DispatchModeLimit corpus byte for byte", () => {
  const cases = NodeFS.readFileSync(corpus, "utf8").trimEnd().split("\n");
  assert.isAbove(cases.length, 3000);
  for (const line of cases) {
    const c = JSON.parse(line) as DecideCase;
    const out = JSON.stringify(dispatchModeLimitFns[c.fn]?.(c.input) ?? null);
    if (out !== JSON.stringify(c.output)) assert.fail(`${c.fn} ${JSON.stringify(c.input)}: ${out}`);
  }
});
