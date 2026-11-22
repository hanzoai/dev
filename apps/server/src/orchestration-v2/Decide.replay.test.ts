// @effect-diagnostics nodeBuiltinImport:off - the test reads the corpus and dev.wasm files synchronously.
import * as NodeFS from "node:fs";
import { assert, it } from "@effect/vitest";
import { type DecideCase, decideFns } from "./Decide.replay.ts";

// Every pure decide function replays its recorded corpus byte for byte. The corpora are the
// reference: crates/orchestrator replays the same files against the Rust functions.
it.each(["dispatch_mode_limit", "model_switch"])("replays the %s corpus", (name) => {
  const corpus = new URL(`../../../../crates/orchestrator/corpus/${name}.jsonl`, import.meta.url);
  const cases = NodeFS.readFileSync(corpus, "utf8").trimEnd().split("\n");
  assert.isAbove(cases.length, 1000);
  for (const line of cases) {
    const c = JSON.parse(line) as DecideCase;
    const out = JSON.stringify(decideFns[c.fn]?.(c.input) ?? null);
    if (out !== JSON.stringify(c.output)) assert.fail(`${c.fn} ${JSON.stringify(c.input)}: ${out}`);
  }
});

// Node's global `WebAssembly`. The server compiles without DOM types, which carry its declarations.
const { Module } = (
  globalThis as unknown as {
    readonly WebAssembly: {
      readonly Module: (new (bytes: Uint8Array) => object) & {
        readonly imports: (module: object) => ReadonlyArray<{ readonly name: string }>;
      };
    };
  }
).WebAssembly;

it("dev.wasm imports exactly environ_get, environ_sizes_get, fd_write and proc_exit", () => {
  const bytes = NodeFS.readFileSync(new URL("../../../../go/dev.wasm", import.meta.url));
  const imports = Module.imports(new Module(bytes)).map((i) => i.name);
  assert.deepStrictEqual(imports.toSorted(), [
    "environ_get",
    "environ_sizes_get",
    "fd_write",
    "proc_exit",
  ]);
});
