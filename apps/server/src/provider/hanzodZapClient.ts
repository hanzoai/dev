// @effect-diagnostics nodeBuiltinImport:off globalTimers:off
/**
 * ZAP-HTTP v0.3 frame client for `hanzod.zap.sock` (`hanzoai/node/zap/src/lib.rs`).
 *
 * Wire format on the Unix socket: `[u32 BE length][frame]`.
 * Frame layout: 16-byte header (`ZAP\0`, u16 LE version=1, u16 LE flags=(frameType<<8),
 * u32 LE root=16, u32 LE size) followed by 48 bytes of root slots and variable payload bytes.
 */
import * as NodeFSP from "node:fs/promises";
import * as NodeNet from "node:net";
import * as NodePath from "node:path";

import { sharedIdentityDirectory } from "@hanzodev/shared/hanzoSharedIdentity";

import type { SharedIdentityHost } from "./hanzoSharedIdentityNode.ts";

const MAGIC = Buffer.from([0x5a, 0x41, 0x50, 0x00]); // "ZAP\0"
const HEADER = 16;
const SLOTS = 48;
const MAX_FRAME = 64 << 20;

const ZAP_FRAME_REQUEST = 0x01;
const ZAP_FRAME_RESPONSE = 0x02;

export interface ZapHttpRequest {
  readonly method: string;
  readonly target: string;
  readonly headers?: ReadonlyArray<readonly [string, string]>;
  readonly body?: Uint8Array;
}

export interface ZapHttpResponse {
  readonly status: number;
  readonly headers: ReadonlyArray<readonly [string, string]>;
  readonly body: Buffer;
}

function beginFrame(frameType: number): Buffer {
  const buf = Buffer.alloc(HEADER + SLOTS);
  MAGIC.copy(buf, 0);
  buf.writeUInt16LE(1, 4);
  buf.writeUInt16LE(frameType << 8, 6);
  buf.writeUInt32LE(HEADER, 8);
  buf.writeUInt32LE(0, 12);
  return buf;
}

function appendVar(
  chunks: Buffer[],
  cursor: { len: number },
  head: Buffer,
  slot: number,
  data: Uint8Array,
): void {
  if (data.length === 0) return;
  const start = cursor.len;
  const buf = Buffer.from(data.buffer, data.byteOffset, data.byteLength);
  chunks.push(buf);
  cursor.len += buf.length;
  head.writeUInt32LE(start - slot, slot);
  head.writeUInt32LE(buf.length, slot + 4);
}

function encodeHeaders(headers: ReadonlyArray<readonly [string, string]>): Buffer {
  if (headers.length === 0) return Buffer.alloc(0);
  const parts: Buffer[] = [];
  const countBuf = Buffer.alloc(4);
  countBuf.writeUInt32LE(headers.length, 0);
  parts.push(countBuf);
  for (const [k, v] of headers) {
    const kb = Buffer.from(k, "utf8");
    const vb = Buffer.from(v, "utf8");
    const kl = Buffer.alloc(4);
    kl.writeUInt32LE(kb.length, 0);
    const vl = Buffer.alloc(4);
    vl.writeUInt32LE(vb.length, 0);
    parts.push(kl, kb, vl, vb);
  }
  return Buffer.concat(parts);
}

function decodeHeaders(raw: Buffer): ReadonlyArray<readonly [string, string]> {
  if (raw.length === 0) return [];
  if (raw.length < 4) throw new Error("malformed ZAP headers");
  const count = Math.min(raw.readUInt32LE(0), 1024);
  let at = 4;
  const out: Array<readonly [string, string]> = [];
  for (let i = 0; i < count; i++) {
    if (at + 4 > raw.length) throw new Error("malformed ZAP headers");
    const kl = raw.readUInt32LE(at);
    at += 4;
    if (at + kl + 4 > raw.length) throw new Error("malformed ZAP headers");
    const k = raw.subarray(at, at + kl).toString("utf8");
    at += kl;
    const vl = raw.readUInt32LE(at);
    at += 4;
    if (at + vl > raw.length) throw new Error("malformed ZAP headers");
    const v = raw.subarray(at, at + vl).toString("utf8");
    at += vl;
    out.push([k, v]);
  }
  return out;
}

function validateFrame(
  frame: Buffer,
  expectedType: number,
): { readonly root: number; readonly size: number } {
  if (frame.length < HEADER || !frame.subarray(0, 4).equals(MAGIC)) {
    throw new Error("not a ZAP frame");
  }
  const version = frame.readUInt16LE(4);
  if (version !== 1 && version !== 2) {
    throw new Error("unknown ZAP version");
  }
  const frameType = frame.readUInt16LE(6) >> 8;
  if (frameType !== expectedType) {
    throw new Error(`unexpected ZAP frame type ${frameType}`);
  }
  const root = frame.readUInt32LE(8);
  const size = frame.readUInt32LE(12);
  if (size < HEADER || size > frame.length || root < HEADER || root + SLOTS > size) {
    throw new Error("invalid ZAP frame geometry");
  }
  return { root, size };
}

function readVar(frame: Buffer, size: number, slot: number): Buffer {
  if (slot + 8 > size) throw new Error("ZAP slot past frame");
  const rel = frame.readUInt32LE(slot);
  if (rel === 0) return Buffer.alloc(0);
  const len = frame.readUInt32LE(slot + 4);
  const start = slot + rel;
  const end = start + len;
  if (start < HEADER || end > size) throw new Error("ZAP field past frame");
  return frame.subarray(start, end);
}

export function encodeZapHttpRequest(req: ZapHttpRequest): Buffer {
  const head = beginFrame(ZAP_FRAME_REQUEST);
  const chunks: Buffer[] = [head];
  const cursor = { len: head.length };
  appendVar(chunks, cursor, head, HEADER, Buffer.from(req.method, "utf8"));
  appendVar(chunks, cursor, head, HEADER + 8, Buffer.from(req.target, "utf8"));
  appendVar(chunks, cursor, head, HEADER + 16, Buffer.from("HTTP/1.1", "utf8"));
  appendVar(chunks, cursor, head, HEADER + 24, encodeHeaders(req.headers ?? []));
  appendVar(chunks, cursor, head, HEADER + 32, req.body ?? Buffer.alloc(0));
  head.writeUInt32LE(cursor.len, 12);
  return Buffer.concat(chunks, cursor.len);
}

export function decodeZapHttpRequest(frame: Buffer): ZapHttpRequest {
  const { root, size } = validateFrame(frame, ZAP_FRAME_REQUEST);
  return {
    method: readVar(frame, size, root).toString("utf8"),
    target: readVar(frame, size, root + 8).toString("utf8"),
    headers: decodeHeaders(readVar(frame, size, root + 24)),
    body: Buffer.from(readVar(frame, size, root + 32)),
  };
}

export function encodeZapHttpResponse(res: ZapHttpResponse): Buffer {
  const head = beginFrame(ZAP_FRAME_RESPONSE);
  head.writeUInt16LE(res.status, HEADER);
  const chunks: Buffer[] = [head];
  const cursor = { len: head.length };
  appendVar(chunks, cursor, head, HEADER + 16, Buffer.from("ZAP-HTTP/1.0", "utf8"));
  appendVar(chunks, cursor, head, HEADER + 24, encodeHeaders(res.headers));
  appendVar(chunks, cursor, head, HEADER + 32, res.body);
  head.writeUInt32LE(cursor.len, 12);
  return Buffer.concat(chunks, cursor.len);
}

export function decodeZapHttpResponse(frame: Buffer): ZapHttpResponse {
  const { root, size } = validateFrame(frame, ZAP_FRAME_RESPONSE);
  return {
    status: frame.readUInt16LE(root),
    headers: decodeHeaders(readVar(frame, size, root + 24)),
    body: Buffer.from(readVar(frame, size, root + 32)),
  };
}

export function hanzodSocketPath(host: SharedIdentityHost): string {
  return NodePath.join(sharedIdentityDirectory(host), "hanzod.zap.sock");
}

export function hanzodConfigPath(host: SharedIdentityHost): string {
  return NodePath.join(sharedIdentityDirectory(host), "hanzod.json");
}

const isSafeSigninOrigin = (url: string): boolean =>
  url.startsWith("https://") ||
  url.startsWith("http://127.0.0.1") ||
  url.startsWith("http://localhost");

/** Reads `signin_origin` from `<data_local_dir>/hanzo/hanzod.json` when present and valid. */
export async function readHanzodSigninOrigin(
  host: SharedIdentityHost,
): Promise<string | undefined> {
  const raw = await NodeFSP.readFile(hanzodConfigPath(host), "utf8").catch(() => undefined);
  if (!raw?.trim()) return undefined;
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (typeof parsed !== "object" || parsed === null || Array.isArray(parsed)) return undefined;
    const origin = (parsed as { readonly signin_origin?: unknown }).signin_origin;
    if (typeof origin !== "string") return undefined;
    const trimmed = origin.trim().replace(/\/+$/u, "");
    return trimmed && isSafeSigninOrigin(trimmed) ? trimmed : undefined;
  } catch {
    return undefined;
  }
}

const DEFAULT_ZAP_TIMEOUT_MS = 2_000;

/** Sends one ZAP-HTTP request over a Unix socket; returns `null` if unreachable or malformed. */
async function callHanzodZap(
  socketPath: string,
  request: ZapHttpRequest,
  timeoutMs = DEFAULT_ZAP_TIMEOUT_MS,
): Promise<ZapHttpResponse | null> {
  return new Promise((resolve) => {
    let settled = false;
    const finish = (value: ZapHttpResponse | null) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      socket.destroy();
      resolve(value);
    };

    const socket = NodeNet.createConnection({ path: socketPath });
    const timer = setTimeout(() => finish(null), timeoutMs);
    let incoming: Buffer = Buffer.alloc(0);

    socket.on("error", () => finish(null));
    socket.on("connect", () => {
      const frame = encodeZapHttpRequest(request);
      const prefix = Buffer.alloc(4);
      prefix.writeUInt32BE(frame.length, 0);
      socket.write(Buffer.concat([prefix, frame]));
    });
    socket.on("data", (chunk: Buffer) => {
      incoming = incoming.length === 0 ? chunk : Buffer.concat([incoming, chunk]);
      if (incoming.length < 4) return;
      const len = incoming.readUInt32BE(0);
      if (len === 0 || len > MAX_FRAME) {
        finish(null);
        return;
      }
      if (incoming.length < 4 + len) return;
      try {
        finish(decodeZapHttpResponse(incoming.subarray(4, 4 + len)));
      } catch {
        finish(null);
      }
    });
    socket.on("end", () => finish(null));
    socket.on("close", () => finish(null));
  });
}

export type HanzodIdentityResult =
  | {
      readonly kind: "ok";
      readonly owner: string;
      readonly name: string;
      readonly id: string;
      readonly email?: string;
      readonly hasToken: boolean;
    }
  | { readonly kind: "signed_out" }
  | { readonly kind: "unavailable" };

export async function readHanzodIdentity(
  host: SharedIdentityHost,
  timeoutMs = DEFAULT_ZAP_TIMEOUT_MS,
): Promise<HanzodIdentityResult> {
  if (host.platform === "win32") return { kind: "unavailable" };
  const response = await callHanzodZap(
    hanzodSocketPath(host),
    { method: "GET", target: "/v1/identity", headers: [["Content-Type", "application/json"]] },
    timeoutMs,
  );
  if (!response) return { kind: "unavailable" };
  if (response.status === 404) return { kind: "signed_out" };
  if (response.status !== 200) return { kind: "unavailable" };
  try {
    const parsed = JSON.parse(response.body.toString("utf8")) as Record<string, unknown>;
    const owner = typeof parsed.owner === "string" ? parsed.owner.trim() : "";
    const name = typeof parsed.name === "string" ? parsed.name.trim() : "";
    if (!owner || !name) return { kind: "unavailable" };
    const email =
      typeof parsed.email === "string" && parsed.email.trim() ? parsed.email.trim() : undefined;
    return {
      kind: "ok",
      owner,
      name,
      id: `${owner}/${name}`,
      ...(email ? { email } : {}),
      hasToken: parsed.token !== false,
    };
  } catch {
    return { kind: "unavailable" };
  }
}

export type HanzodIdentityTokenResult =
  | { readonly kind: "ok"; readonly accessToken: string; readonly expiresAtMs?: number }
  | { readonly kind: "spent" }
  | { readonly kind: "signed_out" }
  | { readonly kind: "unavailable" };

export async function readHanzodIdentityToken(
  host: SharedIdentityHost,
  timeoutMs = DEFAULT_ZAP_TIMEOUT_MS,
): Promise<HanzodIdentityTokenResult> {
  if (host.platform === "win32") return { kind: "unavailable" };
  const response = await callHanzodZap(
    hanzodSocketPath(host),
    {
      method: "GET",
      target: "/v1/identity/token",
      headers: [["Content-Type", "application/json"]],
    },
    timeoutMs,
  );
  if (!response) return { kind: "unavailable" };
  if (response.status === 401) return { kind: "spent" };
  if (response.status === 404) return { kind: "signed_out" };
  if (response.status !== 200) return { kind: "unavailable" };
  try {
    const parsed = JSON.parse(response.body.toString("utf8")) as Record<string, unknown>;
    const accessToken = typeof parsed.access_token === "string" ? parsed.access_token.trim() : "";
    if (!accessToken) return { kind: "unavailable" };
    const expiresAt =
      typeof parsed.expires_at === "number" && Number.isFinite(parsed.expires_at)
        ? parsed.expires_at * 1_000
        : undefined;
    return {
      kind: "ok",
      accessToken,
      ...(expiresAt === undefined ? {} : { expiresAtMs: expiresAt }),
    };
  } catch {
    return { kind: "unavailable" };
  }
}
