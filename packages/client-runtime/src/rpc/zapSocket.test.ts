import { buildRequest, parseRequest } from "@zap-proto/zap";
import { describe, expect, it } from "@effect/vitest";

import { lengthPrefixed, splitMessages, ZAP_FRAME_METHOD } from "./zapSocket.ts";

const call = (text: string) =>
  buildRequest({
    method: ZAP_FRAME_METHOD,
    promiseID: 1,
    target: 0,
    cap: new Uint8Array(0),
    payload: new TextEncoder().encode(text),
  });

describe("ZAP stream framing", () => {
  it("splits whole messages and keeps a partial one for the next read", () => {
    const a = lengthPrefixed(call("one"));
    const b = lengthPrefixed(call("two"));
    const joined = new Uint8Array(a.byteLength + b.byteLength - 3);
    joined.set(a);
    joined.set(b.subarray(0, b.byteLength - 3), a.byteLength);
    const first = splitMessages(joined);
    expect(first.messages).toHaveLength(1);
    expect(new TextDecoder().decode(parseRequest(first.messages[0]!).payload)).toBe("one");
    const rest = new Uint8Array(first.rest.byteLength + 3);
    rest.set(first.rest);
    rest.set(b.subarray(b.byteLength - 3), first.rest.byteLength);
    const second = splitMessages(rest);
    expect(new TextDecoder().decode(parseRequest(second.messages[0]!).payload)).toBe("two");
    expect(second.rest.byteLength).toBe(0);
  });
});
