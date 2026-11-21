import { afterEach, describe, expect, it, vi } from "vite-plus/test";

vi.mock("expo-crypto", () => ({
  getRandomValues: (array: Uint8Array) => array.fill(7),
}));

describe("crypto polyfill", () => {
  const original = globalThis.crypto;
  afterEach(() => {
    Object.defineProperty(globalThis, "crypto", { configurable: true, writable: true, value: original });
    vi.resetModules();
  });

  it("lets the sign-in start path run when Hermes has no globalThis.crypto", async () => {
    Object.defineProperty(globalThis, "crypto", { configurable: true, writable: true, value: undefined });
    vi.resetModules();
    await import("./crypto-polyfill");
    const { createOAuthState, createPkceChallengeS256, createPkceVerifier } = await import(
      "@hanzodev/shared/hanzoIamAuth"
    );
    const verifier = createPkceVerifier();
    expect(verifier.length).toBeGreaterThan(20);
    expect(createPkceChallengeS256(verifier).length).toBeGreaterThan(20);
    expect(createOAuthState().length).toBeGreaterThan(10);
  });
});
