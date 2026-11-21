// ZAP session framing shared by the Dev client (browser) and the Dev server.
// A session is one bidirectional stream; a message on it is a u32 little-endian
// length followed by one ZAP router envelope (apps/web/src/embed/runtime.ts).

/** The Dev session service's one method: a frame of the environment protocol. */
export const ZAP_FRAME_METHOD = 0;

/**
 * The session's other methods are the questions a client asks before it opens the stream. Each is
 * what the Dev server answers over HTTP, as a call on the session: the request payload is the
 * route's query as a JSON object, the response body the route's JSON (frame.Method* in go/frame).
 */
export const ZAP_DESCRIPTOR_METHOD = 1; // GET /.well-known/hanzo/environment
export const ZAP_SESSION_METHOD = 2; // GET /v1/auth/session
export const ZAP_SHELL_METHOD = 3; // GET /v1/event/shell

/**
 * A call's promise id has this bit set, and a frame's never does, so a response says which it
 * answers. Responses to frames carry the id of the last frame; responses to calls carry their own.
 */
export const ZAP_CALL_ID_BASE = 0x4000_0000;

/** A ZAP message on a stream: u32 little-endian length, then the router envelope. */
export function lengthPrefixed(envelope: Uint8Array): Uint8Array {
  const out = new Uint8Array(4 + envelope.byteLength);
  new DataView(out.buffer).setUint32(0, envelope.byteLength, true);
  out.set(envelope, 4);
  return out;
}

/** Splits `buffered` into whole messages; returns them with what is left over. */
export function splitMessages(buffered: Uint8Array): {
  readonly messages: ReadonlyArray<Uint8Array>;
  readonly rest: Uint8Array;
} {
  const messages: Array<Uint8Array> = [];
  let at = 0;
  while (buffered.byteLength - at >= 4) {
    const size = new DataView(buffered.buffer, buffered.byteOffset + at, 4).getUint32(0, true);
    if (buffered.byteLength - at - 4 < size) break;
    messages.push(buffered.subarray(at + 4, at + 4 + size));
    at += 4 + size;
  }
  return { messages, rest: buffered.subarray(at) };
}
