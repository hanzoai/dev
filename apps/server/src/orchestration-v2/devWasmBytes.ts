import * as NodeFS from "node:fs";

/**
 * The bytes of `dev.wasm`. From source they are read from the module `make wasm` builds; the pack
 * build replaces this module with the bytes inline (scripts/lib/dev-wasm-plugin.ts), so a bundle,
 * the single executable and the desktop app carry the module and look for no file.
 */
export const devWasmBytes = (): Uint8Array =>
  NodeFS.readFileSync(new URL("../../../../go/dev.wasm", import.meta.url));
