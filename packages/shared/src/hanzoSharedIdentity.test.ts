import { describe, expect, it } from "vite-plus/test";

import {
  isSharedSuperAdmin,
  isSharedTokenUsable,
  parseActiveIdentity,
  parseSharedIdentity,
  sharedIdentityDirectory,
} from "./hanzoSharedIdentity.ts";

const b64 = (value: unknown) => Buffer.from(JSON.stringify(value)).toString("base64url");
const jwt = (claims: Record<string, unknown>) => `${b64({ alg: "none" })}.${b64(claims)}.sig`;
const entry = (claims: Record<string, unknown>, refresh = "refresh") =>
  JSON.stringify({ access_token: jwt(claims), token_type: "Bearer", refresh_token: refresh });

const CONFIG = `[auth]
provider = "hanzo"

[auth.active]
hanzo = "admin/z"

[auth.active_in.hanzo]
admin = "z"
`;

describe("shared Hanzo identity vault", () => {
  const NOW = Date.UTC(2026, 10, 21);
  const credentials = JSON.stringify({
    "hanzo/admin/z": entry({
      owner: "admin",
      name: "z",
      email: "z@hanzo.ai",
      exp: NOW / 1000 + 600,
    }),
    "hanzo/hanzo/z": entry({
      owner: "hanzo",
      name: "z",
      email: "z@hanzo.ai",
      exp: NOW / 1000 - 60,
    }),
    "lux/lux/z": entry({ owner: "lux", name: "z" }),
  });

  it("reads every hanzo identity and the CLI's active one", () => {
    const snapshot = parseSharedIdentity({ credentials, config: CONFIG });
    expect(snapshot.activeAccountId).toBe("admin/z");
    expect(snapshot.accounts.map((a) => a.id)).toEqual(["admin/z", "hanzo/z"]);
    expect(snapshot.accounts[0]?.email).toBe("z@hanzo.ai");
    expect(isSharedSuperAdmin(snapshot.accounts[0]!)).toBe(true);
    expect(isSharedSuperAdmin(snapshot.accounts[1]!)).toBe(false);
  });

  it("falls back to the first identity when the index names none", () => {
    expect(parseSharedIdentity({ credentials, config: undefined }).activeAccountId).toBe("admin/z");
    expect(parseActiveIdentity('[auth.active]\nhanzo = "hanzo/z"\n')).toBe("hanzo/z");
  });

  it("never offers an expired token, and offers a live one", () => {
    const [live, stale] = parseSharedIdentity({ credentials, config: CONFIG }).accounts;
    expect(isSharedTokenUsable(live, NOW)).toBe(true);
    expect(isSharedTokenUsable(stale, NOW)).toBe(false);
    expect(isSharedTokenUsable(live, NOW + 590_000)).toBe(false);
  });

  it("is empty for a missing or corrupt vault", () => {
    expect(parseSharedIdentity({ credentials: undefined, config: CONFIG }).accounts).toEqual([]);
    expect(parseSharedIdentity({ credentials: "{nope", config: CONFIG }).accounts).toEqual([]);
  });

  it("places the vault in the platform's local data directory", () => {
    const env = {};
    expect(sharedIdentityDirectory({ platform: "darwin", home: "/h", env })).toBe(
      "/h/Library/Application Support/hanzo",
    );
    expect(sharedIdentityDirectory({ platform: "linux", home: "/h", env })).toBe(
      "/h/.local/share/hanzo",
    );
  });
});
