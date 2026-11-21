import * as NodeChildProcess from "node:child_process";
import * as NodeURL from "node:url";
import { describe, expect, it } from "vitest";

const record = (session: number, kind: number, payload: Uint8Array) => {
  const out = new Uint8Array(9 + payload.byteLength);
  const view = new DataView(out.buffer);
  view.setUint32(0, session, true);
  view.setUint8(4, kind);
  view.setUint32(5, payload.byteLength, true);
  out.set(payload, 9);
  return out;
};

// The worker as hanzod starts it: the mux on descriptor 3, a socketpair end it did not open.
const worker = `
  const { serveMux } = await import(${JSON.stringify(NodeURL.pathToFileURL(`${import.meta.dirname}/ZapMux.ts`).href)});
  serveMux(3, (stream) => {
    void (async () => {
      const first = await stream.read();
      await stream.write(new TextEncoder().encode("pong:" + new TextDecoder().decode(first ?? new Uint8Array())));
    })();
  });
`;

describe("ZapMux", () => {
  it("opens a session per OPEN record and carries bytes both ways over an inherited descriptor", async () => {
    const child = NodeChildProcess.spawn(process.execPath, ["--input-type=module", "-e", worker], {
      stdio: ["ignore", "inherit", "inherit", "pipe"],
    });
    const pair = child.stdio[3];
    if (pair === null || pair === undefined || !("write" in pair))
      throw new Error("no descriptor 3");
    const reply = new Promise<Uint8Array>((resolve) =>
      pair.once("data", (d) => resolve(new Uint8Array(d))),
    );
    pair.write(record(1, 1, new Uint8Array(0)));
    pair.write(record(1, 0, new TextEncoder().encode("ping")));
    const got = await reply;
    expect(new TextDecoder().decode(got.slice(9))).toBe("pong:ping");
    expect(new DataView(got.buffer, got.byteOffset).getUint32(0, true)).toBe(1);
    child.kill();
  });
});
