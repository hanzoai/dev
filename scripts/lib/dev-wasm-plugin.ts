// @effect-diagnostics nodeBuiltinImport:off - A build-time plugin; synchronous Node APIs keep it small.
import * as NodeFS from "node:fs";
import * as NodePath from "node:path";

const repoRoot = NodePath.resolve(import.meta.dirname, "../..");
const sourceModule = NodePath.join(repoRoot, "apps/server/src/orchestration-v2/devWasmBytes.ts");
const wasmPath = NodePath.join(repoRoot, "go/dev.wasm");

/** Pack plugin: the bundle carries `dev.wasm` inline in place of `devWasmBytes.ts`. */
export const devWasmPlugin = {
  name: "hanzodev:dev-wasm",
  load(id: string) {
    if (NodePath.resolve(id) !== sourceModule) return;
    const base64 = NodeFS.readFileSync(wasmPath).toString("base64");
    return {
      code: `export const devWasmBytes = () => new Uint8Array(Buffer.from("${base64}", "base64"));`,
      moduleType: "js",
    };
  },
};
