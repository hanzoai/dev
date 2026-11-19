import { describe, expect, it, vi } from "vite-plus/test";
import {
  buildHanzoApiHeaders,
  buildHanzoIamAccountFromTokens,
  buildHanzoIamAuthorizeUrl,
  buildHanzoSurfaceLaunchUrl,
  createPkceChallengeS256,
  createPkceVerifier,
  decodeHanzoJwtClaims,
  fetchHanzoBillingSummary,
  getHanzoIamBrand,
  HANZO_SUBSCRIPTION_PLANS,
  parseHanzoIamCallbackUrl,
  removeHanzoIamVaultAccount,
  resolveActiveHanzoOrgId,
  switchHanzoIamVaultOrg,
  upsertHanzoIamVaultAccount,
} from "./hanzoIamAuth.ts";

function makeJwt(payload: Record<string, unknown>): string {
  const encode = (obj: unknown) => Buffer.from(JSON.stringify(obj), "utf8").toString("base64url");
  return `${encode({ alg: "RS256", typ: "JWT" })}.${encode(payload)}.sig`;
}

describe("hanzoIamAuth", () => {
  it("resolves all four Hanzo ecosystem IAM brands and defaults to hanzo.id", () => {
    expect(getHanzoIamBrand("hanzo")).toEqual({
      id: "hanzo",
      label: "Hanzo",
      issuer: "https://hanzo.id",
      clientId: "hanzo-app",
      defaultClientId: "hanzo-app",
    });
    expect(getHanzoIamBrand("lux").issuer).toBe("https://lux.id");
    expect(getHanzoIamBrand("zoo").issuer).toBe("https://zoolabs.id");
    expect(getHanzoIamBrand("pars").issuer).toBe("https://pars.id");
    expect(getHanzoIamBrand("unknown").issuer).toBe("https://hanzo.id");
  });

  it("builds standard OIDC PKCE S256 authorize URLs", () => {
    const verifier = createPkceVerifier(new Uint8Array(32).fill(7));
    const challenge = createPkceChallengeS256(verifier);
    const url = new URL(
      buildHanzoIamAuthorizeUrl({
        issuer: "https://hanzo.id",
        clientId: "hanzo-app",
        redirectUri: "hanzodev://oauth/callback",
        state: "state-123",
        codeChallenge: challenge,
      }),
    );
    expect(url.origin).toBe("https://hanzo.id");
    expect(url.pathname).toBe("/oauth/authorize");
    expect(url.searchParams.get("client_id")).toBe("hanzo-app");
    expect(url.searchParams.get("redirect_uri")).toBe("hanzodev://oauth/callback");
    expect(url.searchParams.get("code_challenge")).toBe(challenge);
    expect(url.searchParams.get("code_challenge_method")).toBe("S256");
    expect(url.searchParams.get("scope")).toBe("openid profile email offline_access");
  });

  it("parses hanzo:// and hanzodev:// OAuth deep-link callbacks and ignores unrelated links", () => {
    expect(
      parseHanzoIamCallbackUrl("hanzo://oauth/callback?code=abc123&state=xyz789&brand=hanzo"),
    ).toEqual({
      code: "abc123",
      state: "xyz789",
      error: null,
      errorDescription: null,
      brand: "hanzo",
    });
    expect(parseHanzoIamCallbackUrl("hanzodev://oauth/lux?code=lux_code&state=s2")).toEqual({
      code: "lux_code",
      state: "s2",
      error: null,
      errorDescription: null,
      brand: "lux",
    });
    expect(
      parseHanzoIamCallbackUrl(
        "hanzo-dev://--/oauth/callback?error=access_denied&error_description=User+cancelled",
      ),
    ).toEqual({
      code: null,
      state: null,
      error: "access_denied",
      errorDescription: "User cancelled",
      brand: null,
    });
    expect(parseHanzoIamCallbackUrl("hanzodev://pair?url=https%3A%2F%2Fexample.com")).toBeNull();
    expect(parseHanzoIamCallbackUrl("hanzodev://new?prompt=hello")).toBeNull();
  });

  it("decodes JWT claims, persists X-Org-Id across accounts, and builds X-Org-Id API headers", () => {
    const tokenA = makeJwt({
      sub: "z",
      owner: "hanzo",
      name: "z",
      email: "z@hanzo.ai",
      organizations: ["hanzo", "lux", { id: "zoo-labs", displayName: "Zoo Labs" }],
      exp: 1_900_000_000,
      iss: "https://hanzo.id",
    });
    const tokenB = makeJwt({
      sub: "z",
      owner: "admin",
      name: "z",
      email: "admin@hanzo.ai",
      exp: 1_900_000_000,
      iss: "https://hanzo.id",
    });

    expect(decodeHanzoJwtClaims(tokenA)).toMatchObject({
      subject: "z",
      owner: "hanzo",
      email: "z@hanzo.ai",
      issuer: "https://hanzo.id",
    });

    const acctA = buildHanzoIamAccountFromTokens({
      issuer: "https://hanzo.id",
      clientId: "hanzo-app",
      accessToken: tokenA,
    });
    const acctB = buildHanzoIamAccountFromTokens({
      issuer: "https://hanzo.id",
      clientId: "hanzo-app",
      accessToken: tokenB,
    });
    expect(acctA.id).toBe("hanzo/z");
    expect(acctA.organizations?.map((o) => o.id)).toEqual(["hanzo", "lux", "zoo-labs"]);
    expect(acctB.id).toBe("admin/z");

    let vault = upsertHanzoIamVaultAccount({ activeAccountId: null, accounts: [] }, acctA);
    expect(resolveActiveHanzoOrgId(vault)).toBe("hanzo");

    vault = switchHanzoIamVaultOrg(vault, "zoo-labs");
    expect(resolveActiveHanzoOrgId(vault)).toBe("zoo-labs");
    expect(
      buildHanzoApiHeaders({
        accessToken: acctA.accessToken,
        orgId: resolveActiveHanzoOrgId(vault),
      }),
    ).toEqual({
      Accept: "application/json",
      Authorization: `Bearer ${acctA.accessToken}`,
      "X-Org-Id": "zoo-labs",
    });

    vault = upsertHanzoIamVaultAccount(vault, acctB);
    expect(vault.activeAccountId).toBe("admin/z");
    expect(resolveActiveHanzoOrgId(vault)).toBe("admin");

    vault = removeHanzoIamVaultAccount(vault, "admin/z");
    expect(vault.activeAccountId).toBe("hanzo/z");
    expect(resolveActiveHanzoOrgId(vault)).toBe("zoo-labs");
  });

  it("queries billing summary with X-Org-Id and exposes canonical Go/Pro/Team/Ultra plans", async () => {
    expect(HANZO_SUBSCRIPTION_PLANS.map((p) => `${p.name}:${p.priceLabel}`)).toEqual([
      "Free:$0/mo",
      "Go:$8/mo",
      "Pro:$15/mo",
      "Team:Team",
      "Ultra:$195/mo",
    ]);

    const fetchMock = vi.fn(async (_input: RequestInfo | URL, init?: RequestInit) => {
      expect((init?.headers as Record<string, string> | undefined)?.["X-Org-Id"]).toBe("hanzo");
      return new Response(
        JSON.stringify({
          plan: "ultra",
          credit_balance_cents: 14_250,
          monthly_used_cents: 4_800,
          monthly_limit_cents: 25_000,
        }),
        { status: 200, headers: { "Content-Type": "application/json" } },
      );
    });

    const summary = await fetchHanzoBillingSummary({
      accessToken: "tok_live",
      orgId: "hanzo",
      fetchImpl: fetchMock as unknown as typeof globalThis.fetch,
    });
    expect(summary).toEqual({
      planId: "ultra",
      planName: "Ultra",
      priceLabel: "$195/mo",
      creditBalanceCents: 14_250,
      monthlyUsedCents: 4_800,
      monthlyLimitCents: 25_000,
      billingPortalUrl: "https://billing.hanzo.ai/?org=hanzo",
    });

    expect(
      buildHanzoSurfaceLaunchUrl("https://platform.hanzo.ai", {
        orgId: "hanzo",
        accountId: "hanzo/z",
      }),
    ).toBe("https://platform.hanzo.ai/?org=hanzo&account=hanzo%2Fz");
  });
});
