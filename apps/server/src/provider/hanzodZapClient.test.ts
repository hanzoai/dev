// @effect-diagnostics nodeBuiltinImport:off
import * as NodeFSP from "node:fs/promises";
import * as NodeNet from "node:net";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";

import { afterEach, describe, expect, it } from "vite-plus/test";

import {
  decodeZapHttpRequest,
  decodeZapHttpResponse,
  encodeZapHttpRequest,
  encodeZapHttpResponse,
  hanzodConfigPath,
  hanzodSocketPath,
  readHanzodIdentity,
  readHanzodIdentityToken,
  readHanzodSigninOrigin,
  type ZapHttpRequest,
  type ZapHttpResponse,
} from "./hanzodZapClient.ts";
import {
  hanzoAccessToken,
  readSharedIdentity,
  type SharedIdentityHost,
} from "./hanzoSharedIdentityNode.ts";

const tempDirs: string[] = [];
const openServers: NodeNet.Server[] = [];

async function makeTempHost(): Promise<{
  readonly host: SharedIdentityHost;
  readonly hanzoDir: string;
}> {
  // Keep socket path short for macOS 104-byte sun_path limit.
  const root = await NodeFSP.mkdtemp(NodePath.join(NodeOS.tmpdir(), "hz-zap-"));
  tempDirs.push(root);
  const host: SharedIdentityHost = {
    platform: "linux",
    home: root,
    env: { XDG_DATA_HOME: root },
  };
  const hanzoDir = NodePath.join(root, "hanzo");
  await NodeFSP.mkdir(hanzoDir, { recursive: true });
  return { host, hanzoDir };
}

async function startZapServer(
  socketPath: string,
  handler: (req: ZapHttpRequest) => ZapHttpResponse,
): Promise<NodeNet.Server> {
  const server = NodeNet.createServer((socket) => {
    let incoming: Buffer = Buffer.alloc(0);
    socket.on("data", (chunk: Buffer) => {
      incoming = incoming.length === 0 ? chunk : Buffer.concat([incoming, chunk]);
      if (incoming.length < 4) return;
      const len = incoming.readUInt32BE(0);
      if (incoming.length < 4 + len) return;
      const req = decodeZapHttpRequest(incoming.subarray(4, 4 + len));
      const resFrame = encodeZapHttpResponse(handler(req));
      const prefix = Buffer.alloc(4);
      prefix.writeUInt32BE(resFrame.length, 0);
      socket.end(Buffer.concat([prefix, resFrame]));
    });
  });
  openServers.push(server);
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(socketPath, () => resolve());
  });
  return server;
}

const jsonResponse = (status: number, body: unknown): ZapHttpResponse => ({
  status,
  headers: [["Content-Type", "application/json"]],
  body: Buffer.from(JSON.stringify(body), "utf8"),
});

afterEach(async () => {
  await Promise.all(
    openServers.splice(0).map(
      (server) =>
        new Promise<void>((resolve) => {
          server.close(() => resolve());
        }),
    ),
  );
  await Promise.all(
    tempDirs.splice(0).map((dir) => NodeFSP.rm(dir, { recursive: true, force: true })),
  );
});

describe("ZAP-HTTP framing", () => {
  it("round-trips requests and responses and rejects invalid frames", () => {
    const req: ZapHttpRequest = {
      method: "GET",
      target: "/v1/identity/token",
      headers: [["Content-Type", "application/json"]],
      body: Buffer.from("{}", "utf8"),
    };
    const decodedReq = decodeZapHttpRequest(encodeZapHttpRequest(req));
    expect(decodedReq.method).toBe("GET");
    expect(decodedReq.target).toBe("/v1/identity/token");
    expect(decodedReq.headers).toEqual([["Content-Type", "application/json"]]);
    expect(decodedReq.body?.toString("utf8")).toBe("{}");

    const res = jsonResponse(200, { access_token: "tok-123" });
    const decodedRes = decodeZapHttpResponse(encodeZapHttpResponse(res));
    expect(decodedRes.status).toBe(200);
    expect(JSON.parse(decodedRes.body.toString("utf8"))).toEqual({ access_token: "tok-123" });

    expect(() => decodeZapHttpResponse(Buffer.from("bad"))).toThrow();
  });
});

describe("hanzod ZAP identity socket", () => {
  it("reads signin_origin from hanzod.json and ignores unsafe origins", async () => {
    const { host } = await makeTempHost();
    expect(await readHanzodSigninOrigin(host)).toBeUndefined();

    await NodeFSP.writeFile(
      hanzodConfigPath(host),
      JSON.stringify({ signin_origin: "https://hanzo.id/" }),
    );
    expect(await readHanzodSigninOrigin(host)).toBe("https://hanzo.id");

    await NodeFSP.writeFile(
      hanzodConfigPath(host),
      JSON.stringify({ signin_origin: "http://evil.example" }),
    );
    expect(await readHanzodSigninOrigin(host)).toBeUndefined();
  });

  it("prefers hanzod.zap.sock for identity and token when present", async () => {
    const { host } = await makeTempHost();
    const futureExp = 1_900_000_000;
    await NodeFSP.writeFile(
      hanzodConfigPath(host),
      JSON.stringify({ signin_origin: "https://hanzo.id" }),
    );
    await startZapServer(hanzodSocketPath(host), (req) => {
      if (req.target === "/v1/identity") {
        return jsonResponse(200, {
          owner: "hanzo",
          name: "alice",
          email: "alice@hanzo.ai",
          active: true,
        });
      }
      if (req.target === "/v1/identity/token") {
        return jsonResponse(200, { access_token: "zap-tok-fresh", expires_at: futureExp });
      }
      return jsonResponse(404, { error: "not found" });
    });

    expect(await readHanzodIdentity(host)).toEqual({
      kind: "ok",
      owner: "hanzo",
      name: "alice",
      id: "hanzo/alice",
      email: "alice@hanzo.ai",
      hasToken: true,
    });
    expect(await readHanzodIdentityToken(host)).toEqual({
      kind: "ok",
      accessToken: "zap-tok-fresh",
      expiresAtMs: futureExp * 1_000,
    });
    expect(await hanzoAccessToken(host)).toBe("zap-tok-fresh");

    const snapshot = await readSharedIdentity(host);
    expect(snapshot.activeAccountId).toBe("hanzo/alice");
    expect(snapshot.accounts).toHaveLength(1);
    expect(snapshot.accounts[0]?.accessToken).toBe("zap-tok-fresh");
    expect(snapshot.accounts[0]?.issuer).toBe("https://hanzo.id");
    expect(snapshot.accounts[0]?.email).toBe("alice@hanzo.ai");
  });

  it("reports spent (401) and signed_out (404) from hanzod.zap.sock and falls back when absent", async () => {
    const { host, hanzoDir } = await makeTempHost();
    // Without socket, falls back to vault file.
    expect(await readHanzodIdentityToken(host)).toEqual({ kind: "unavailable" });

    // 401 spent
    const server = await startZapServer(hanzodSocketPath(host), (req) => {
      if (req.target === "/v1/identity/token") {
        return jsonResponse(401, { error: "the stored token is spent" });
      }
      return jsonResponse(404, { error: "nobody is signed in" });
    });
    expect(await readHanzodIdentityToken(host)).toEqual({ kind: "spent" });
    expect(await readHanzodIdentity(host)).toEqual({ kind: "signed_out" });

    // Close server and remove socket, verify vault fallback works.
    await new Promise<void>((resolve) => server.close(() => resolve()));
    await NodeFSP.rm(hanzodSocketPath(host), { force: true });
    await NodeFSP.writeFile(
      NodePath.join(hanzoDir, "config.toml"),
      '[auth.active]\nhanzo = "hanzo/bob"\n',
    );
    await NodeFSP.writeFile(
      NodePath.join(hanzoDir, "credentials"),
      JSON.stringify({
        "hanzo/hanzo/bob": JSON.stringify({ access_token: "vault-tok" }),
      }),
    );
    expect(await hanzoAccessToken(host)).toBe("vault-tok");
  });
});
