import {
  buildHanzoIamAccountFromTokens,
  EMPTY_HANZO_IAM_VAULT,
  type HanzoIamAccount,
  upsertHanzoIamVaultAccount,
} from "@hanzodev/shared/hanzoIamAuth";
import { afterEach, beforeEach, describe, expect, it, vi } from "vite-plus/test";

import {
  readSelectedLocalAiModel,
  writeSelectedLocalAiModel,
} from "../localAi/localAiState";
import { HanzoWebAuthController } from "./hanzoWebAuth";

function sampleAccount(overrides: Partial<HanzoIamAccount> = {}): HanzoIamAccount {
  return {
    id: "hanzo/z",
    brand: "hanzo",
    issuer: "https://hanzo.id",
    clientId: "hanzo-dev-client-id",
    accessToken: "tok_hanzo_123",
    refreshToken: "ref_hanzo_123",
    expiresAt: null,
    subject: "z",
    owner: "hanzo",
    email: "z@hanzo.ai",
    displayName: "Z",
    avatarUrl: null,
    organizations: [
      { id: "hanzo", name: "hanzo", displayName: "Hanzo AI" },
      { id: "lux", name: "lux", displayName: "Lux Network" },
    ],
    ...overrides,
  };
}

describe("HanzoWebAuthController snapshot stability", () => {
  it("returns referentially stable organizations and accounts snapshots when signed out", () => {
    const controller = new HanzoWebAuthController();
    controller.resetForTesting();

    expect(Object.is(controller.accounts, controller.accounts)).toBe(true);
    expect(Object.is(controller.organizations, controller.organizations)).toBe(true);
    expect(controller.organizations).toHaveLength(0);
  });

  it("caches fallback organizations per active account for useSyncExternalStore", () => {
    const controller = new HanzoWebAuthController();
    const account = buildHanzoIamAccountFromTokens({
      issuer: "https://hanzo.id",
      clientId: "hanzo-app",
      accessToken: "tok_test_123",
      refreshToken: null,
      userinfo: {
        sub: "alice",
        owner: "hanzo",
        name: "alice",
        displayName: "Alice",
        email: "alice@hanzo.ai",
      },
    });
    controller.resetForTesting(upsertHanzoIamVaultAccount(EMPTY_HANZO_IAM_VAULT, account, true));

    const first = controller.organizations;
    const second = controller.organizations;
    expect(Object.is(first, second)).toBe(true);
    expect(first.length).toBeGreaterThan(0);
  });
});

describe("HanzoWebAuthController IAM, Org Switcher & Local AI guard", () => {
  const originalFetch = globalThis.fetch;
  const originalWindow = globalThis.window;

  beforeEach(() => {
    const storage = new Map<string, string>();
    const session = new Map<string, string>();
    const mockStorage: Storage = {
      get length() {
        return storage.size;
      },
      clear: () => storage.clear(),
      getItem: (key) => storage.get(key) ?? null,
      key: (index) => [...storage.keys()][index] ?? null,
      removeItem: (key) => {
        storage.delete(key);
      },
      setItem: (key, value) => {
        storage.set(key, value);
      },
    };
    const mockSessionStorage: Storage = {
      get length() {
        return session.size;
      },
      clear: () => session.clear(),
      getItem: (key) => session.get(key) ?? null,
      key: (index) => [...session.keys()][index] ?? null,
      removeItem: (key) => {
        session.delete(key);
      },
      setItem: (key, value) => {
        session.set(key, value);
      },
    };
    Object.defineProperty(globalThis, "window", {
      configurable: true,
      writable: true,
      value: {
        location: {
          href: "http://127.0.0.1:5733/",
          origin: "http://127.0.0.1:5733",
          assign: vi.fn(),
        },
        history: { replaceState: vi.fn() },
        localStorage: mockStorage,
        sessionStorage: mockSessionStorage,
        open: vi.fn(),
      },
    });
  });

  afterEach(() => {
    globalThis.fetch = originalFetch;
    Object.defineProperty(globalThis, "window", {
      configurable: true,
      writable: true,
      value: originalWindow,
    });
    vi.restoreAllMocks();
  });

  it("persists active X-Org-Id per account and syncs to OS Keychain via desktopBridge", () => {
    let keychainPayload: string | null = null;
    (
      globalThis.window as unknown as {
        desktopBridge?: {
          getIamVault: () => Promise<string | null>;
          setIamVault: (payload: string | null) => Promise<boolean>;
        };
      }
    ).desktopBridge = {
      getIamVault: vi.fn(async () => keychainPayload),
      setIamVault: vi.fn(async (payload: string | null) => {
        keychainPayload = payload;
        return true;
      }),
    };

    globalThis.fetch = vi.fn(async () => new Response("{}", { status: 404 })) as typeof fetch;

    const controller = new HanzoWebAuthController();
    controller.resetForTesting({
      activeAccountId: "hanzo/z",
      accounts: [
        sampleAccount(),
        sampleAccount({
          id: "zoo/research",
          brand: "zoo",
          owner: "zoo",
          accessToken: "tok_zoo_456",
        }),
      ],
      activeOrgId: "hanzo",
      orgByAccountId: { "hanzo/z": "hanzo", "zoo/research": "zoo" },
    });

    expect(controller.activeOrgId).toBe("hanzo");
    expect(controller.getApiHeaders()).toEqual({
      Accept: "application/json",
      Authorization: "Bearer tok_hanzo_123",
      "X-Org-Id": "hanzo",
    });

    controller.switchOrganization("lux");
    expect(controller.activeOrgId).toBe("lux");
    expect(controller.getApiHeaders()["X-Org-Id"]).toBe("lux");
    expect(keychainPayload).toContain('"activeOrgId":"lux"');

    controller.switchAccount("zoo/research");
    expect(controller.activeOrgId).toBe("zoo");
    controller.switchAccount("hanzo/z");
    expect(controller.activeOrgId).toBe("lux");
  });

  it("completes desktop PKCE sign-in from hanzo://oauth/callback and hanzodev://oauth/callback deep links", async () => {
    const launchedUrls: string[] = [];
    const openExternal = vi.fn(async (url: string) => {
      launchedUrls.push(url);
      return true;
    });
    (
      globalThis.window as unknown as {
        desktopBridge?: { openExternal: (url: string) => Promise<boolean> };
      }
    ).desktopBridge = { openExternal };

    globalThis.fetch = vi.fn(async (input) => {
      const url = String(input);
      if (url.includes("/oauth/token")) {
        return new Response(
          JSON.stringify({
            access_token: "tok_pkce_desktop",
            refresh_token: "ref_pkce_desktop",
            expires_in: 3600,
          }),
          { status: 200, headers: { "Content-Type": "application/json" } },
        );
      }
      if (url.includes("/userinfo")) {
        return new Response(
          JSON.stringify({
            sub: "z",
            owner: "hanzo",
            displayName: "Z Desktop",
            email: "z@hanzo.ai",
          }),
          { status: 200, headers: { "Content-Type": "application/json" } },
        );
      }
      return new Response("[]", { status: 200, headers: { "Content-Type": "application/json" } });
    }) as typeof fetch;

    const controller = new HanzoWebAuthController();
    controller.resetForTesting();
    await controller.startOAuthSignIn({ brandId: "hanzo" });

    expect(launchedUrls).toHaveLength(1);
    const launchedUrl = new URL(launchedUrls[0]!);
    expect(launchedUrl.searchParams.get("redirect_uri")).toBe("hanzodev://oauth/callback");
    const state = launchedUrl.searchParams.get("state")!;

    const handled = await controller.handleDeepLinkCallback(
      `hanzo://oauth/callback?code=auth_code_999&state=${encodeURIComponent(state)}`,
    );
    expect(handled).toBe(true);
    expect(controller.activeAccount?.id).toBe("hanzo/z");
    expect(controller.activeAccount?.accessToken).toBe("tok_pkce_desktop");
  });

  it("enforces Qwen3+ local model selection and rejects legacy qwen2 weights", () => {
    writeSelectedLocalAiModel("zen4-coder:30b");
    expect(readSelectedLocalAiModel()).toBe("zen4-coder:30b");

    writeSelectedLocalAiModel("qwen2.5-coder:7b");
    expect(readSelectedLocalAiModel()).toBe("zen4-coder:30b");
  });
});
