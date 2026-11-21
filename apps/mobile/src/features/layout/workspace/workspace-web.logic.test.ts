import { describe, expect, it } from "vite-plus/test";

import { SESSION_KEYS, isAppUrl, sessionScript, webSectionUrl } from "./workspace-web.logic";

describe("web section urls", () => {
  it("are hanzo.ai's library, design and plugins routes", () => {
    expect(webSectionUrl("library")).toBe("https://hanzo.ai/library");
    expect(webSectionUrl("design")).toBe("https://hanzo.ai/design");
    expect(webSectionUrl("plugins")).toBe("https://hanzo.ai/dev/-/customize/plugins");
  });

  it("keeps the view on the app's origin over https", () => {
    expect(isAppUrl("https://hanzo.ai/design/x?y=1")).toBe(true);
    expect(isAppUrl("http://hanzo.ai/design")).toBe(false);
    expect(isAppUrl("https://hanzo.ai.evil.com/")).toBe(false);
    expect(isAppUrl("https://evil.com/?https://hanzo.ai")).toBe(false);
    expect(isAppUrl("not a url")).toBe(false);
  });
});

describe("session script", () => {
  const run = (script: string) => {
    const store = new Map<string, string>();
    new Function("localStorage", script)({
      setItem: (key: string, value: string) => store.set(key, value),
    });
    return store;
  };

  it("writes the keys hanzo.app reads", () => {
    const store = run(
      sessionScript({ accessToken: "a.b.c", refreshToken: "r", expiresAt: 1_700_000_000_000 }),
    );
    expect(store.get(SESSION_KEYS.accessToken)).toBe("a.b.c");
    expect(store.get(SESSION_KEYS.refreshToken)).toBe("r");
    expect(store.get(SESSION_KEYS.expiresAt)).toBe("1700000000000");
  });

  it("omits what the account does not have", () => {
    const store = run(sessionScript({ accessToken: "k", refreshToken: null, expiresAt: null }));
    expect([...store.keys()]).toEqual([SESSION_KEYS.accessToken]);
  });

  it("cannot be broken out of by a hostile token", () => {
    const token = `x"); window.pwned=1; ("`;
    const store = run(sessionScript({ accessToken: token, refreshToken: null, expiresAt: null }));
    expect(store.get(SESSION_KEYS.accessToken)).toBe(token);
  });
});
