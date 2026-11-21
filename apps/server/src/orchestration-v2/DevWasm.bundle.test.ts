import * as NodeFS from "node:fs";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";
import { assert, it } from "@effect/vitest";
import { build } from "vite-plus/pack";
import { devWasmPlugin } from "../../../../scripts/lib/dev-wasm-plugin.ts";

// The bundle must carry dev.wasm: it is built from DevWasm.ts alone, moved where no repo is
// beside it, and asked to decide.
it("a built bundle decides without any file of the repo beside it", async () => {
  const built = NodeFS.mkdtempSync(NodePath.join(NodeOS.tmpdir(), "dev-wasm-built-"));
  const moved = NodeFS.mkdtempSync(NodePath.join(NodeOS.tmpdir(), "dev-wasm-moved-"));
  try {
    await build({
      config: false,
      entry: [NodePath.join(import.meta.dirname, "DevWasm.ts")],
      outDir: built,
      format: "esm",
      platform: "node",
      plugins: [devWasmPlugin],
      logLevel: "silent",
    });
    const bundle = NodeFS.readdirSync(built).find((f) => f.endsWith(".mjs") || f.endsWith(".js"));
    assert.isDefined(bundle);
    NodeFS.copyFileSync(NodePath.join(built, bundle!), NodePath.join(moved, "DevWasm.mjs"));
    const mod = (await import(NodePath.join(moved, "DevWasm.mjs"))) as {
      decide: (name: string, input: unknown) => unknown;
    };
    const limit = { runtimeMode: "auto", interactionMode: "default" };
    const asked = { runtimeMode: "full-access", interactionMode: "default" };
    assert.strictEqual(mod.decide("exceeded", { limit, modes: asked }), "runtime");
  } finally {
    NodeFS.rmSync(built, { recursive: true, force: true });
    NodeFS.rmSync(moved, { recursive: true, force: true });
  }
});
