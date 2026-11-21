import { ZapTransport } from "@hanzo/browser";
import { describe, expect, it } from "@effect/vitest";
import * as Effect from "effect/Effect";

import { fakeZapRouter, type FakeCall } from "./FakeZapRouter.testkit.ts";
import { refusal } from "./BrowserPolicy.ts";
import * as BrowserSources from "./BrowserSources.ts";

const FIREFOX = "browser/mac/firefox-4dec";
const BASE_CAPS = ["browser.tabs", "browser.navigate", "browser.dom", "browser.input"];

const tabs = [
  { id: 1, url: "https://example.com/docs", title: "Docs", active: true },
  { id: 2, url: "https://chatgpt.com/c/abc", title: "Private chat", active: false },
  { id: 3, url: "https://claude.ai/chat/9", title: "Another", active: false },
  { id: 4, url: "https://news.example.org/", title: "News", active: false },
];

const page = (call: FakeCall): unknown => {
  switch (call.method) {
    case "hanzo.listTabs":
      return { tabs };
    case "hanzo.snapshot":
      return { tree: `- button "Go" [ref=e1]`, url: tabs[Number(call.params.tabId) - 1]?.url };
    case "hanzo.read":
      return { markdown: "# Docs" };
    case "hanzo.act":
      // A click that lands the tab on a consumer AI page.
      return Number(call.params.tabId) === 4
        ? { ok: true, url: "https://chatgpt.com/" }
        : { ok: true };
    case "provider.accounts":
      return {
        accounts: [
          { provider: "chatgpt", plan: "plus", state: "ready" },
          { provider: "claude", state: "needs_login" },
        ],
      };
    case "usage.report":
      return {
        snapshots: [
          {
            provider: "codex",
            fetchedAt: "2026-11-21T10:00:00.000Z",
            source: "local",
            account: "someone@example.com",
            windows: [{ kind: "session", label: "5h", usedPercent: 40 }],
          },
        ],
      };
    default:
      throw new Error(`unknown method ${call.method}`);
  }
};

const withRouter = <A, E>(
  caps: ReadonlyArray<string>,
  use: (
    sources: BrowserSources.BrowserSources["Service"],
    calls: FakeCall[],
  ) => Effect.Effect<A, E>,
) =>
  Effect.scoped(
    Effect.gen(function* () {
      const router = yield* fakeZapRouter([{ id: FIREFOX, caps }], page);
      const sources = BrowserSources.make(
        BrowserSources.routerOf(new ZapTransport(router.socketPath)),
      );
      return yield* use(sources, router.calls);
    }),
  );

describe("BrowserSources discovery", () => {
  it.live(
    "lists the connected extension as a linked source, with no providers it does not advertise",
    () =>
      withRouter(BASE_CAPS, (sources, calls) =>
        Effect.gen(function* () {
          const list = yield* sources.list;
          expect(list).toEqual([
            {
              id: FIREFOX,
              browser: "firefox",
              name: "Firefox · Hanzo extension",
              capabilities: BASE_CAPS,
              providers: [],
            },
          ]);
          expect(calls).toEqual([]);
        }),
      ),
  );

  it.live("reads provider state when the extension advertises it", () =>
    withRouter([...BASE_CAPS, "provider.accounts"], (sources) =>
      Effect.gen(function* () {
        const [source] = yield* sources.list;
        expect(source?.providers).toEqual([
          { provider: "chatgpt", plan: "plus", state: "ready" },
          { provider: "claude", state: "needs_login" },
        ]);
      }),
    ),
  );

  it.live("lists nothing when there is no router", () =>
    Effect.gen(function* () {
      const sources = BrowserSources.make(
        BrowserSources.routerOf(new ZapTransport("/nonexistent/zapd.sock")),
      );
      expect(yield* sources.list).toEqual([]);
      expect(yield* sources.usage).toEqual([]);
    }),
  );

  it.live("reads usage snapshots and strips the account", () =>
    withRouter([...BASE_CAPS, "usage.report"], (sources) =>
      Effect.gen(function* () {
        const snapshots = yield* sources.usage;
        expect(snapshots).toHaveLength(1);
        expect(snapshots[0]).not.toHaveProperty("account");
        expect(snapshots[0]?.windows[0]?.usedPercent).toBe(40);
      }),
    ),
  );

  it.live("asks for no usage from an extension that holds none", () =>
    withRouter(BASE_CAPS, (sources, calls) =>
      Effect.gen(function* () {
        expect(yield* sources.usage).toEqual([]);
        expect(calls).toEqual([]);
      }),
    ),
  );
});

describe("browser tools refuse consumer AI pages", () => {
  it.live("withholds the URL and title of a blocked tab", () =>
    withRouter(BASE_CAPS, (sources) =>
      Effect.gen(function* () {
        const result = yield* sources.tabs({});
        const blocked = result.tabs.filter((tab) => tab.blocked !== undefined);
        expect(blocked.map((tab) => tab.id)).toEqual([2, 3]);
        for (const tab of blocked) {
          expect(tab).not.toHaveProperty("url");
          expect(tab).not.toHaveProperty("title");
        }
        expect(result.tabs[0]).toMatchObject({ id: 1, url: "https://example.com/docs" });
      }),
    ),
  );

  it.live(
    "snapshot, read and click on a consumer AI tab are refused before anything reaches the extension",
    () =>
      withRouter(BASE_CAPS, (sources, calls) =>
        Effect.gen(function* () {
          const errors = [
            yield* Effect.flip(Effect.asVoid(sources.snapshot({ tabId: 2 }))),
            yield* Effect.flip(Effect.asVoid(sources.read({ tabId: 3 }))),
            yield* Effect.flip(Effect.asVoid(sources.click({ tabId: 2, selector: "@e1" }))),
          ];
          for (const error of errors) {
            expect(error.reason).toBe("refused_origin");
            expect(error.message).toContain("terms bar automated access");
          }
          expect(new Set(calls.map((call) => call.method))).toEqual(new Set(["hanzo.listTabs"]));
        }),
      ),
  );

  it.live("withholds the answer of a click that left the tab on a consumer AI page", () =>
    withRouter(BASE_CAPS, (sources) =>
      Effect.gen(function* () {
        const error = yield* Effect.flip(
          Effect.asVoid(sources.click({ tabId: 4, selector: "@e1" })),
        );
        expect(error.reason).toBe("refused_origin");
        expect(error.message).toContain("ChatGPT");
      }),
    ),
  );

  it.live("drives an ordinary page, naming its tab", () =>
    withRouter(BASE_CAPS, (sources, calls) =>
      Effect.gen(function* () {
        expect(yield* sources.read({ tabId: 1, outline: true })).toEqual({ markdown: "# Docs" });
        expect(yield* sources.click({ tabId: 1, selector: "@e1" })).toEqual({ ok: true });
        expect(calls.at(-1)).toMatchObject({
          method: "hanzo.act",
          params: { op: "click", selector: "@e1", tabId: "1" },
        });
      }),
    ),
  );

  it.live("rejects a tab the browser does not have", () =>
    withRouter(BASE_CAPS, (sources) =>
      Effect.gen(function* () {
        const error = yield* Effect.flip(Effect.asVoid(sources.read({ tabId: 99 })));
        expect(error.reason).toBe("not_found");
      }),
    ),
  );

  it.live("calls only the four allowlisted methods", () =>
    withRouter(BASE_CAPS, (sources, calls) =>
      Effect.gen(function* () {
        yield* sources.tabs({});
        yield* sources.snapshot({ tabId: 1 });
        yield* sources.read({ tabId: 1 });
        yield* sources.click({ tabId: 1, selector: "@e1" });
        expect(new Set(calls.map((call) => call.method))).toEqual(
          new Set(["hanzo.listTabs", "hanzo.snapshot", "hanzo.read", "hanzo.act"]),
        );
      }),
    ),
  );
});

describe("withBrowserSnapshots", () => {
  const snapshot = (provider: string, windows: number) => ({
    provider,
    fetchedAt: "2026-11-21T10:00:00.000Z",
    source: "local" as const,
    windows: Array.from({ length: windows }, () => ({
      kind: "session" as const,
      label: "5h",
      usedPercent: 10,
    })),
  });
  const summary = (...snapshots: ReturnType<typeof snapshot>[]) => ({
    generatedAt: "2026-11-21T10:00:00.000Z",
    snapshots,
    cost: [],
  });

  it("adds what the desktop has no reading of, and never replaces one it has", () => {
    const merged = BrowserSources.withBrowserSnapshots(
      summary(snapshot("claude", 1), snapshot("codex", 0)),
      [snapshot("claude", 2), snapshot("codex", 1), snapshot("gemini", 1)],
    );
    expect(merged.snapshots.map((s) => [s.provider, s.windows.length])).toEqual([
      ["claude", 1],
      ["codex", 1],
      ["gemini", 1],
    ]);
  });

  it("returns the summary itself when the browser adds nothing", () => {
    const own = summary(snapshot("claude", 1));
    expect(BrowserSources.withBrowserSnapshots(own, [])).toBe(own);
  });
});

describe("refusal", () => {
  it.each([
    ["https://chatgpt.com/", "ChatGPT"],
    ["https://chat.openai.com/c/1", "ChatGPT"],
    ["https://claude.ai/new", "Claude.ai"],
    ["https://gemini.google.com/app", "Gemini"],
    ["https://grok.com/", "Grok"],
    ["https://x.com/i/grok", "Grok"],
    ["https://www.perplexity.ai/search", "Perplexity"],
    ["https://copilot.microsoft.com/", "Copilot"],
    ["https://cursor.com/agents", "Cursor"],
    ["https://CHATGPT.COM./", "ChatGPT"],
    ["https://sub.claude.ai/", "Claude.ai"],
  ])("refuses %s as %s", (url, product) => {
    expect(refusal(url)).toContain(product);
  });

  it.each([
    "https://example.com/",
    "https://x.com/home",
    "https://notchatgpt.com/",
    "https://chatgpt.com.example.org/",
    "https://github.com/cursor/cursor",
    "about:blank",
  ])("allows %s", (url) => {
    expect(refusal(url)).toBeUndefined();
  });

  it.each([
    undefined,
    "",
    "not a url",
    "file:///etc/passwd",
    "moz-extension://x/y.html",
    "view-source:https://example.com",
  ])("refuses a page it cannot identify: %s", (url) => {
    expect(refusal(url)).toContain("cannot tell what page");
  });
});
