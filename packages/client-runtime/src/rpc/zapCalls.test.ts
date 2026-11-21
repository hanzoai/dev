import {
  lengthPrefixed,
  splitMessages,
  ZAP_CALL_ID_BASE,
  ZAP_DESCRIPTOR_METHOD,
  ZAP_FRAME_METHOD,
  ZAP_SESSION_METHOD,
} from "@hanzodev/shared/zapFrames";
import { buildResponse, parseRequest, Status } from "@zap-proto/zap";
import { afterEach, describe, expect, it, vi } from "@effect/vitest";

import { callZap } from "./zapSocket.ts";

const encoder = new TextEncoder();
const decoder = new TextDecoder();

/** A runtime on the far end of a fake WebTransport: it records what it was sent. */
const runtime = () => {
  const seen: Array<{ method: number; promiseID: number; cap: string; payload: string }> = [];
  const toClient = new TransformStream<Uint8Array, Uint8Array>();
  const fromClient = new TransformStream<Uint8Array, Uint8Array>();
  const out = toClient.writable.getWriter();
  void (async () => {
    const reader = fromClient.readable.getReader();
    let rest: Uint8Array = new Uint8Array(0);
    for (;;) {
      const { value, done } = await reader.read();
      if (done) return;
      const joined = new Uint8Array(rest.byteLength + value.byteLength);
      joined.set(rest);
      joined.set(value, rest.byteLength);
      const split = splitMessages(joined);
      rest = split.rest;
      for (const message of split.messages) {
        const call = parseRequest(message.slice());
        seen.push({
          method: call.method,
          promiseID: call.promiseID,
          cap: decoder.decode(call.cap),
          payload: decoder.decode(call.payload),
        });
        const body =
          call.method === ZAP_DESCRIPTOR_METHOD
            ? '{"environmentId":"e"}'
            : call.method === ZAP_SESSION_METHOD
              ? '{"authenticated":true}'
              : '{"_tag":"Pong"}';
        await out.write(
          lengthPrefixed(buildResponse(Status.OK, call.promiseID, encoder.encode(body))),
        );
      }
    }
  })();
  class FakeTransport {
    ready = Promise.resolve();
    createBidirectionalStream = async () => ({
      readable: toClient.readable,
      writable: fromClient.writable,
    });
    close = () => {};
  }
  vi.stubGlobal("WebTransport", FakeTransport);
  return seen;
};

afterEach(() => vi.unstubAllGlobals());

describe("calls on a ZAP session", () => {
  it("answer with the route's JSON, and carry the bearer once", async () => {
    const seen = runtime();
    const url = "zaps://zap.example/v1/dev/zap/a";
    expect(await callZap(url, ZAP_DESCRIPTOR_METHOD, { bearer: null })).toEqual({
      environmentId: "e",
    });
    expect(await callZap(url, ZAP_SESSION_METHOD, { bearer: "token" })).toEqual({
      authenticated: true,
    });
    await callZap(url, ZAP_SESSION_METHOD, { bearer: "token" });
    expect(seen.map((s) => [s.method, s.cap])).toEqual([
      [ZAP_DESCRIPTOR_METHOD, ""],
      [ZAP_SESSION_METHOD, "token"],
      [ZAP_SESSION_METHOD, ""],
    ]);
    // A call's id has the call bit, which no frame's id has.
    expect(seen.every((s) => s.promiseID >= ZAP_CALL_ID_BASE)).toBe(true);
    expect(ZAP_FRAME_METHOD).toBe(0);
  });
});
