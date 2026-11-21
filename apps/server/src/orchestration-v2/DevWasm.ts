import * as NodeWasi from "node:wasi";
import { devWasmBytes } from "./devWasmBytes.ts";

/**
 * The Dev engine's pure decide functions, loaded from `dev.wasm` (crates/ffi) with Node's built-in
 * WebAssembly: no native addon, no per-platform build. The module imports only what the loop needs
 * (`environ_get`, `environ_sizes_get`, `fd_write`, `proc_exit`), so it touches no file, socket or clock.
 */
interface Exports {
  readonly memory: WebAssembly.Memory;
  readonly dev_alloc: (len: number) => number;
  readonly dev_release: (ptr: number, len: number) => void;
  readonly dev_free: (buf: number) => void;
  readonly dev_decide: (
    name: number,
    nameLen: number,
    input: number,
    inputLen: number,
    out: number,
  ) => number;
}

let loaded: Exports | undefined;
const engine = (): Exports => {
  if (loaded) return loaded;
  const wasi = new NodeWasi.WASI({ version: "preview1" });
  const instance = new WebAssembly.Instance(
    new WebAssembly.Module(devWasmBytes() as BufferSource),
    wasi.getImportObject() as WebAssembly.Imports,
  );
  wasi.initialize(instance as never);
  loaded = instance.exports as unknown as Exports;
  return loaded;
};

const encoder = new TextEncoder();
const decoder = new TextDecoder();

/** Applies the pure decide function `name` to `input`, and returns what its JSON output parses to. */
export function decide(name: string, input: unknown): unknown {
  const wasm = engine();
  const bytes = [encoder.encode(name), encoder.encode(JSON.stringify(input))] as const;
  const out = wasm.dev_alloc(12);
  const lent = bytes.map((b) => wasm.dev_alloc(b.length));
  try {
    bytes.forEach((b, i) => new Uint8Array(wasm.memory.buffer).set(b, lent[i]));
    const status = wasm.dev_decide(lent[0]!, bytes[0].length, lent[1]!, bytes[1].length, out);
    if (status !== 0) throw new Error(`dev_decide ${name} answered ${status}`);
    const view = new DataView(wasm.memory.buffer);
    const ptr = view.getUint32(out, true);
    const len = view.getUint32(out + 4, true);
    const json = decoder.decode(new Uint8Array(wasm.memory.buffer, ptr, len));
    wasm.dev_free(out);
    return JSON.parse(json);
  } finally {
    lent.forEach((p, i) => wasm.dev_release(p, bytes[i]!.length));
    wasm.dev_release(out, 12);
  }
}
