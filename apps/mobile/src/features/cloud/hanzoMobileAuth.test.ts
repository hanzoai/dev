import { afterEach, describe, expect, it, vi } from "vite-plus/test";

const secureStoreState = new Map<string, string>();

vi.mock("expo-secure-store", () => ({
  getItemAsync: vi.fn(async (key: string) => secureStoreState.get(key) ?? null),
  setItemAsync: vi.fn(async (key: string, value: string) => {
    secureStoreState.set(key, value);
  }),
  deleteItemAsync: vi.fn(async (key: string) => {
    secureStoreState.delete(key);
  }),
}));

vi.mock("expo-linking", () => ({
  createURL: (path: string) => `hanzodev://${path.replace(/^\/+/u, "")}`,
  addEventListener: vi.fn(() => ({ remove: vi.fn() })),
}));

vi.mock("expo-web-browser", () => ({
  maybeCompleteAuthSession: vi.fn(),
  openAuthSessionAsync: vi.fn(),
}));

import { shouldHandleAppLink } from "../../lib/appLinking";
import { hanzoMobileAuthStore, type HanzoIamAccount } from "./hanzoMobileAuth";

function makeAccount(overrides: Partial<HanzoIamAccount> = {}): HanzoIamAccount {
  return {
    id: "hanzo/z",
    brand: "hanzo",
    issuer: "https://hanzo.id",
    clientId: "hanzo-app-client-id",
    accessToken: "mob_tok_123",
    refreshToken: "mob_ref_123",
    expiresAt: null,
    subject: "z",
    owner: "hanzo",
    email: "z@hanzo.ai",
    displayName: "Z Mobile",
    avatarUrl: null,
    organizations: [
      { id: "hanzo", name: "hanzo", displayName: "Hanzo AI" },
      { id: "lux", name: "lux", displayName: "Lux Network" },
    ],
    ...overrides,
  };
}

describe("hanzoMobileAuthStore & deep-link routing", () => {
  const originalFetch = globalThis.fetch;

  afterEach(() => {
    secureStoreState.clear();
    hanzoMobileAuthStore.resetForTesting();
    globalThis.fetch = originalFetch;
    vi.clearAllMocks();
  });

  it("persists X-Org-Id across SecureStore hydration and account switches", async () => {
    globalThis.fetch = vi.fn(async () => new Response("[]", { status: 200 })) as typeof fetch;

    hanzoMobileAuthStore.resetForTesting({
      activeAccount: makeAccount(),
      accounts: [
        makeAccount(),
        makeAccount({ id: "zoo/labs", brand: "zoo", owner: "zoo", accessToken: "zoo_tok" }),
      ],
    });

    expect(hanzoMobileAuthStore.getSnapshot().activeOrgId).toBe("hanzo");
    expect(hanzoMobileAuthStore.getApiHeaders()["X-Org-Id"]).toBe("hanzo");

    await hanzoMobileAuthStore.switchOrganization("lux");
    expect(hanzoMobileAuthStore.getSnapshot().activeOrgId).toBe("lux");
    expect(hanzoMobileAuthStore.getApiHeaders()["X-Org-Id"]).toBe("lux");
    expect(secureStoreState.get("hanzo.dev.iam.vault.v1")).toContain('"activeOrgId":"lux"');

    await hanzoMobileAuthStore.switchAccount("zoo/labs");
    expect(hanzoMobileAuthStore.getSnapshot().activeOrgId).toBe("zoo");

    await hanzoMobileAuthStore.switchAccount("hanzo/z");
    expect(hanzoMobileAuthStore.getSnapshot().activeOrgId).toBe("lux");
  });

  it("exchanges PKCE authorization code from hanzo://oauth/callback deep link", async () => {
    globalThis.fetch = vi.fn(async (input) => {
      const url = String(input);
      if (url.includes("/oauth/token")) {
        return new Response(
          JSON.stringify({
            access_token: "mob_exchanged_tok",
            refresh_token: "mob_exchanged_ref",
            expires_in: 3600,
          }),
          { status: 200, headers: { "Content-Type": "application/json" } },
        );
      }
      if (url.includes("/userinfo")) {
        return new Response(
          JSON.stringify({ sub: "z", owner: "hanzo", email: "z@hanzo.ai" }),
          { status: 200, headers: { "Content-Type": "application/json" } },
        );
      }
      return new Response("[]", { status: 200, headers: { "Content-Type": "application/json" } });
    }) as typeof fetch;

    hanzoMobileAuthStore.resetForTesting({
      pendingPkce: {
        state: "state_xyz",
        verifier: "verifier_1234567890",
        redirectUri: "hanzodev://oauth/callback",
        brandId: "hanzo",
        issuer: "https://hanzo.id",
        clientId: "hanzo-app-client-id",
      },
    });

    const consumed = await hanzoMobileAuthStore.handleDeepLinkCallback(
      "hanzo://oauth/callback?code=code_abc&state=state_xyz",
    );
    expect(consumed).toBe(true);
    expect(hanzoMobileAuthStore.getSnapshot().activeAccount?.id).toBe("hanzo/z");
    expect(hanzoMobileAuthStore.getSnapshot().activeAccount?.accessToken).toBe("mob_exchanged_tok");
  });

  it("filters OAuth callbacks out of React Navigation wildcard routing while preserving app links", () => {
    expect(shouldHandleAppLink("hanzo://oauth/callback?code=123&state=abc", true)).toBe(false);
    expect(shouldHandleAppLink("hanzodev://oauth/callback?code=123&state=abc", true)).toBe(false);
    expect(shouldHandleAppLink("hanzo://", true)).toBe(false);
    expect(shouldHandleAppLink("hanzo://settings/auth", true)).toBe(true);
    expect(shouldHandleAppLink("hanzodev://settings/auth", true)).toBe(true);
  });
});
