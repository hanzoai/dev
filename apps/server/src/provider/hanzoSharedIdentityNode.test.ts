// @effect-diagnostics nodeBuiltinImport:off globalDate:off
import * as NodeFSP from "node:fs/promises";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";
import { describe, expect, it } from "vite-plus/test";

import {
  hanzoAccessToken,
  readFreshSharedIdentity,
  readSharedIdentity,
} from "./hanzoSharedIdentityNode.ts";

const b64 = (value: unknown) => Buffer.from(JSON.stringify(value)).toString("base64url");
const jwt = (claims: Record<string, unknown>) => `${b64({ alg: "none" })}.${b64(claims)}.sig`;
const tokenSet = (exp: number) =>
  JSON.stringify({
    access_token: jwt({ owner: "hanzo", name: "z", email: "z@hanzo.ai", exp }),
    refresh_token: "r",
  });

async function machine(exp: number) {
  const home = await NodeFSP.mkdtemp(NodePath.join(NodeOS.tmpdir(), "hanzo-identity-"));
  const dir = NodePath.join(home, ".local", "share", "hanzo");
  const bin = NodePath.join(home, ".local", "bin");
  await NodeFSP.mkdir(dir, { recursive: true });
  await NodeFSP.mkdir(bin, { recursive: true });
  await NodeFSP.writeFile(
    NodePath.join(dir, "credentials"),
    JSON.stringify({ "hanzo/hanzo/z": tokenSet(exp) }),
  );
  await NodeFSP.writeFile(NodePath.join(dir, "config.toml"), '[auth.active]\nhanzo = "hanzo/z"\n');
  return { home, dir, bin, host: { platform: "linux", home, env: {} } };
}

const FRESH_EXP = Math.floor(Date.now() / 1000) + 3600;

describe("machine Hanzo identity", () => {
  it("serves an unexpired token from the vault without running the CLI", async () => {
    const m = await machine(FRESH_EXP);
    const snapshot = await readSharedIdentity(m.host);
    expect(snapshot.accounts[0]?.email).toBe("z@hanzo.ai");
    expect(await hanzoAccessToken(m.host)).toBe(snapshot.accounts[0]?.accessToken);
  });

  it("asks the CLI to refresh an expired token and never touches the vault itself", async () => {
    const m = await machine(Math.floor(Date.now() / 1000) - 600);
    const before = await NodeFSP.readFile(NodePath.join(m.dir, "credentials"), "utf8");
    const fresh = tokenSet(FRESH_EXP);
    // The stand-in CLI behaves as the real one: it rewrites the vault under its own lock.
    await NodeFSP.writeFile(
      NodePath.join(m.bin, "hanzo"),
      `#!/bin/sh\nprintf '%s' '${JSON.stringify({ "hanzo/hanzo/z": fresh }).replace(/'/gu, "")}' > '${NodePath.join(m.dir, "credentials")}'\nprintf 'fresh-token'\n`,
      { mode: 0o755 },
    );
    expect(await NodeFSP.readFile(NodePath.join(m.dir, "credentials"), "utf8")).toBe(before);
    expect(await hanzoAccessToken(m.host)).toBe("fresh-token");
    const snapshot = await readFreshSharedIdentity(m.host);
    expect(snapshot.accounts[0]?.expiresAt).toBeGreaterThan(Date.now());
    expect(snapshot.accounts[0]?.refreshToken).toBeNull();
  });

  it("returns no token when refresh fails, which is when sign-in is asked for", async () => {
    const m = await machine(Math.floor(Date.now() / 1000) - 600);
    await NodeFSP.writeFile(NodePath.join(m.bin, "hanzo"), "#!/bin/sh\necho denied >&2\nexit 1\n", {
      mode: 0o755,
    });
    expect(await hanzoAccessToken(m.host)).toBeUndefined();
  });
});
