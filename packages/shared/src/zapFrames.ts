// ZAP session framing shared by the Dev client (browser) and the Dev server.
// A session is one bidirectional stream; a message on it is a u32 little-endian
// length followed by one ZAP router envelope (apps/web/src/embed/runtime.ts).

/** The Dev session service's one method: a frame of the environment protocol. */
export const ZAP_FRAME_METHOD = 0;

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
