import {
  type BrowserSource,
  type BrowserSourceBrowser,
  BrowserSourceError,
  BrowserSourceProvider,
} from "@hanzodev/contracts";
import { ProviderSnapshot, type UsageSummary } from "@hanzodev/usage";
import { ZapTransport, type ZapBrowserEntry } from "@hanzo/browser";
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Schema from "effect/Schema";

import {
  ACCOUNTS_CAPABILITY,
  BROWSER_METHODS,
  refusal,
  USAGE_CAPABILITY,
} from "./BrowserPolicy.ts";

/** This computer's ZAP router, as the Hanzo browser package's one client speaks to it. */
export interface ZapRouter {
  readonly browsers: () => Promise<ReadonlyArray<ZapBrowserEntry>>;
  readonly call: (
    provider: string,
    method: string,
    params: Record<string, unknown>,
  ) => Promise<unknown>;
}

export const routerOf = (transport: ZapTransport): ZapRouter => ({
  browsers: async () => (transport.isSocketPresent() ? transport.listBrowsers() : []),
  call: (provider, method, params) => transport.call(method, params, { provider }),
});

const ENGINES: Readonly<Record<string, { browser: BrowserSourceBrowser; label: string }>> = {
  firefox: { browser: "firefox", label: "Firefox" },
  chrome: { browser: "chrome", label: "Chrome" },
  safari: { browser: "safari", label: "Safari" },
};

/** `browser/<host>/<engine>-<id>` names the engine the extension runs in. */
const sourceOf = (entry: ZapBrowserEntry): Omit<BrowserSource, "providers"> => {
  const node = entry.id.split("/").at(-1) ?? entry.id;
  const engine = ENGINES[node.split("-")[0]?.toLowerCase() ?? ""];
  return {
    id: entry.id,
    browser: engine?.browser ?? "other",
    name: `${engine?.label ?? "Browser"} · Hanzo extension`,
    capabilities: entry.caps,
  };
};

type Snapshot = ProviderSnapshot;

const TabList = Schema.Struct({
  tabs: Schema.Array(
    Schema.Struct({
      id: Schema.Number,
      url: Schema.optional(Schema.String),
      title: Schema.optional(Schema.String),
      active: Schema.optional(Schema.Boolean),
    }),
  ),
});
const decodeTabs = Schema.decodeUnknownEffect(TabList);

const AccountList = Schema.Struct({ accounts: Schema.Array(BrowserSourceProvider) });
const decodeAccounts = Schema.decodeUnknownEffect(AccountList);

// Only snapshots cross: the schema's `account` is free text, so it is dropped here (HIP-1350).
const UsageReading = Schema.Struct({ snapshots: Schema.Array(ProviderSnapshot) });
const decodeUsage = Schema.decodeUnknownEffect(UsageReading);

export interface BrowserTab {
  readonly id: number;
  readonly title?: string;
  readonly url?: string;
  readonly active: boolean;
  /** Set for a page the agent may not use; its URL and title are withheld. */
  readonly blocked?: string;
}

export interface BrowserTabsResult {
  readonly source: BrowserSource;
  readonly tabs: ReadonlyArray<BrowserTab>;
}

export interface BrowserSourceTarget {
  readonly sourceId?: string | undefined;
}

export class BrowserSources extends Context.Service<
  BrowserSources,
  {
    /** The connected browsers. No router, or none joined, is an empty list. */
    readonly list: Effect.Effect<ReadonlyArray<BrowserSource>>;
    /** Usage snapshots the extensions report, metadata only, with no account. Empty when none do. */
    readonly usage: Effect.Effect<ReadonlyArray<Snapshot>>;
    readonly tabs: (
      target: BrowserSourceTarget,
    ) => Effect.Effect<BrowserTabsResult, BrowserSourceError>;
    readonly snapshot: (
      target: BrowserSourceTarget & {
        readonly tabId: number;
        readonly interactive?: boolean | undefined;
        readonly selector?: string | undefined;
      },
    ) => Effect.Effect<unknown, BrowserSourceError>;
    readonly read: (
      target: BrowserSourceTarget & {
        readonly tabId: number;
        readonly outline?: boolean | undefined;
        readonly filter?: string | undefined;
      },
    ) => Effect.Effect<unknown, BrowserSourceError>;
    readonly click: (
      target: BrowserSourceTarget & { readonly tabId: number; readonly selector: string },
    ) => Effect.Effect<unknown, BrowserSourceError>;
  }
>()("hanzo/browser/BrowserSources") {}

const failure = (reason: BrowserSourceError["reason"], message: string) =>
  new BrowserSourceError({ reason, message });

const messageOf = (cause: unknown) => (cause instanceof Error ? cause.message : String(cause));

/**
 * What a linked browser reports fills the panel where this desktop has no reading of its own. It
 * is added after the summary, never into it, so the desktop's linked report never carries it.
 */
export const withBrowserSnapshots = (
  summary: UsageSummary,
  browser: ReadonlyArray<ProviderSnapshot>,
): UsageSummary => {
  const own = new Set(summary.snapshots.filter((s) => s.windows.length > 0).map((s) => s.provider));
  const extra = browser.filter((s) => !own.has(s.provider));
  return extra.length === 0
    ? summary
    : {
        ...summary,
        snapshots: [
          ...summary.snapshots.filter((s) => !extra.some((e) => e.provider === s.provider)),
          ...extra,
        ],
      };
};

export const make = (router: ZapRouter) => {
  const browsers = Effect.tryPromise({
    try: () => router.browsers(),
    catch: (cause) => failure("no_router", `The ZAP router did not answer: ${messageOf(cause)}`),
  });

  const call = (provider: string, method: string, params: Record<string, unknown> = {}) =>
    Effect.tryPromise({
      try: () => router.call(provider, method, params),
      catch: (cause) => failure("rejected", messageOf(cause)),
    });

  const accountsOf = (entry: ZapBrowserEntry) =>
    entry.caps.includes(ACCOUNTS_CAPABILITY)
      ? call(entry.id, ACCOUNTS_CAPABILITY).pipe(
          Effect.flatMap(decodeAccounts),
          Effect.map((reply) => reply.accounts),
          Effect.orElseSucceed(() => []),
        )
      : Effect.succeed([]);

  const list = browsers.pipe(
    Effect.flatMap((entries) =>
      Effect.forEach(
        entries,
        (entry) =>
          Effect.map(accountsOf(entry), (providers) => ({ ...sourceOf(entry), providers })),
        { concurrency: "unbounded" },
      ),
    ),
    Effect.orElseSucceed((): ReadonlyArray<BrowserSource> => []),
  );

  const usage = browsers.pipe(
    Effect.flatMap((entries) =>
      Effect.forEach(
        entries.filter((entry) => entry.caps.includes(USAGE_CAPABILITY)),
        (entry) =>
          call(entry.id, USAGE_CAPABILITY).pipe(
            Effect.flatMap(decodeUsage),
            Effect.map((reading) =>
              reading.snapshots.map(({ account: _account, ...rest }): Snapshot => rest),
            ),
            Effect.orElseSucceed((): ReadonlyArray<Snapshot> => []),
          ),
        { concurrency: "unbounded" },
      ),
    ),
    Effect.map((all) => all.flat()),
    Effect.orElseSucceed((): ReadonlyArray<Snapshot> => []),
  );

  const resolve = Effect.fn("BrowserSources.resolve")(function* (target: BrowserSourceTarget) {
    const entries = yield* browsers;
    if (entries.length === 0) {
      return yield* failure(
        "not_found",
        "No Hanzo browser extension is connected to this computer's ZAP router. Open the browser where it is installed and sign in.",
      );
    }
    if (target.sourceId !== undefined) {
      const found = entries.find((entry) => entry.id === target.sourceId);
      if (found) return found;
      return yield* failure(
        "not_found",
        `No connected browser ${target.sourceId}. Connected: ${entries.map((entry) => entry.id).join(", ")}.`,
      );
    }
    if (entries.length > 1) {
      return yield* failure(
        "not_found",
        `Several browsers are connected; pass sourceId: ${entries.map((entry) => entry.id).join(", ")}.`,
      );
    }
    return entries[0]!;
  });

  const listTabs = (entry: ZapBrowserEntry) =>
    call(entry.id, BROWSER_METHODS.tabs).pipe(
      Effect.flatMap((reply) =>
        decodeTabs(reply).pipe(
          Effect.mapError(() => failure("rejected", "The browser sent an unreadable tab list.")),
        ),
      ),
      Effect.map((reply) => reply.tabs),
    );

  const tabs = Effect.fn("BrowserSources.tabs")(function* (target: BrowserSourceTarget) {
    const entry = yield* resolve(target);
    const found = yield* listTabs(entry);
    return {
      source: { ...sourceOf(entry), providers: [] },
      tabs: found.map((tab): BrowserTab => {
        const blocked = refusal(tab.url);
        return blocked === undefined
          ? {
              id: tab.id,
              active: tab.active === true,
              ...(tab.title === undefined ? {} : { title: tab.title }),
              ...(tab.url === undefined ? {} : { url: tab.url }),
            }
          : { id: tab.id, active: tab.active === true, blocked };
      }),
    } satisfies BrowserTabsResult;
  });

  /**
   * Runs one method on one named tab, after reading that tab's URL from the browser itself:
   * the origin is the tab's real one, never what the caller says it is.
   */
  const onTab = Effect.fn("BrowserSources.onTab")(function* (
    target: BrowserSourceTarget & { readonly tabId: number },
    method: string,
    params: Record<string, unknown>,
  ) {
    const entry = yield* resolve(target);
    const tab = (yield* listTabs(entry)).find((candidate) => candidate.id === target.tabId);
    if (tab === undefined) {
      return yield* failure(
        "not_found",
        `No tab ${target.tabId} in ${entry.id}. List the tabs first.`,
      );
    }
    const before = refusal(tab.url);
    if (before !== undefined) return yield* failure("refused_origin", before);
    const result = yield* call(entry.id, method, { ...params, tabId: target.tabId });
    // A click can navigate the tab onto a page the first check would have refused.
    const url =
      typeof result === "object" && result !== null && "url" in result
        ? (result as { readonly url?: unknown }).url
        : undefined;
    const after = typeof url === "string" ? refusal(url) : undefined;
    if (after !== undefined) return yield* failure("refused_origin", after);
    return result;
  });

  return BrowserSources.of({
    list,
    usage,
    tabs,
    snapshot: Effect.fn("BrowserSources.snapshot")(function* (input) {
      return yield* onTab(input, BROWSER_METHODS.snapshot, {
        ...(input.interactive === undefined ? {} : { interactive: input.interactive }),
        ...(input.selector === undefined ? {} : { selector: input.selector }),
      });
    }),
    read: Effect.fn("BrowserSources.read")(function* (input) {
      return yield* onTab(input, BROWSER_METHODS.read, {
        ...(input.outline === undefined ? {} : { outline: input.outline }),
        ...(input.filter === undefined ? {} : { filter: input.filter }),
      });
    }),
    click: Effect.fn("BrowserSources.click")(function* (input) {
      return yield* onTab(input, BROWSER_METHODS.act, { op: "click", selector: input.selector });
    }),
  });
};

export const layer = Layer.sync(BrowserSources, () => make(routerOf(new ZapTransport())));
