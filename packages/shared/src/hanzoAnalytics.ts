// @effect-diagnostics globalDate:off globalRandom:off globalFetch:off -- Analytics helpers run synchronously in DOM, React Native, and Node telemetry hooks outside an Effect runtime.

export const DEFAULT_HANZO_ANALYTICS_ENDPOINT = "https://analytics.hanzo.ai/v1/event";
export const DEFAULT_HANZO_WEBSITE_ID = "hanzo-dev";
export const HANZO_LOCAL_BEACON_PATH = "/_/beacon";
export const HANZO_VISITOR_STORAGE_KEY = "hanzo.analytics.visitor.v1";
export const HANZO_SESSION_STORAGE_PREFIX = "hanzo.analytics.session.";

export type HanzoAnalyticsSurface = "desktop" | "web" | "mobile" | "server";
export type HanzoAnalyticsDevice = "desktop" | "mobile" | "tablet";

export type HanzoAnalyticsCoreEventName =
  | "page_view"
  | "app_launched"
  | "desktop_app_launched"
  | "global_shortcut_triggered"
  | "deep_link_received"
  | "sign_up"
  | "login"
  | "org_switched"
  | "begin_checkout"
  | "purchase"
  | "project_created"
  | "project_reopened"
  | "thread_started"
  | "turn_submitted"
  | "voice_dictation_toggled"
  | "voice_action_reviewed"
  | "local_ai_opened"
  | "surface_handoff";

export type HanzoAnalyticsEventName = HanzoAnalyticsCoreEventName | (string & {});

export interface HanzoUtmParams {
  readonly utm_source?: string | undefined;
  readonly utm_medium?: string | undefined;
  readonly utm_campaign?: string | undefined;
  readonly utm_term?: string | undefined;
  readonly utm_content?: string | undefined;
}

export interface HanzoAnalyticsEventPayload extends HanzoUtmParams {
  readonly website: string;
  readonly name: string;
  readonly event_id: string;
  readonly hostname: string;
  readonly path: string;
  readonly url: string;
  readonly referrer: string;
  readonly title: string;
  readonly language: string;
  readonly screen: string;
  readonly device: HanzoAnalyticsDevice;
  readonly surface: HanzoAnalyticsSurface;
  readonly visitor_id: string;
  readonly session_id: string;
  readonly org_id: string | null;
  readonly user_id: string | null;
  readonly timestamp: string;
  readonly data: Readonly<Record<string, unknown>>;
}

export interface HanzoAnalyticsEnvelope {
  readonly type: "pageview" | "event";
  readonly payload: HanzoAnalyticsEventPayload;
}

const SENSITIVE_QUERY_KEYS = new Set([
  "code",
  "state",
  "token",
  "access_token",
  "refresh_token",
  "id_token",
  "secret",
  "pairing_token",
]);

function fnv1aHex(input: string): string {
  let h1 = 0x811c9dc5;
  let h2 = 0x01000193;
  for (let i = 0; i < input.length; i += 1) {
    const ch = input.charCodeAt(i);
    h1 ^= ch;
    h1 = Math.imul(h1, 0x01000193) >>> 0;
    h2 ^= ch + (i & 0xff);
    h2 = Math.imul(h2, 0x85ebca6b) >>> 0;
  }
  return `${h1.toString(16).padStart(8, "0")}${h2.toString(16).padStart(8, "0")}`;
}

function randomHexId(prefix: string): string {
  const randA = Math.floor(Math.random() * 0xffffffff)
    .toString(16)
    .padStart(8, "0");
  const randB = Math.floor(Math.random() * 0xffffffff)
    .toString(16)
    .padStart(8, "0");
  const ts = Date.now().toString(16);
  return `${prefix}_${ts}_${randA}${randB}`;
}

/**
 * Generates a deterministic deduplication `event_id` whenever `order_id` or `transaction_id`
 * is present in the event properties (Enhanced Ecommerce idempotency), or a unique ID otherwise.
 */
export function computeHanzoEventId(
  eventName: string,
  data?: Readonly<Record<string, unknown>>,
): string {
  const dedupeKeyRaw = data?.order_id ?? data?.transaction_id ?? data?.dedupe_key;
  if (typeof dedupeKeyRaw === "string" || typeof dedupeKeyRaw === "number") {
    const trimmed = String(dedupeKeyRaw).trim();
    if (trimmed.length > 0) {
      const normalizedEvent = eventName.trim().toLowerCase().replace(/[^a-z0-9_]+/gu, "_");
      return `evt_${normalizedEvent}_${fnv1aHex(`${normalizedEvent}:${trimmed}`)}`;
    }
  }
  return randomHexId("evt");
}

/**
 * Normalizes a URL or route path: strips trailing slashes (preserving `/`) and scrubs OAuth/secret
 * query parameters from sanitized URLs.
 */
export function normalizeHanzoAnalyticsPath(rawUrlOrPath: string): {
  readonly path: string;
  readonly sanitizedUrl: string;
  readonly hostname: string;
  readonly utm: HanzoUtmParams;
} {
  const trimmed = rawUrlOrPath.trim() || "/";
  try {
    const base = trimmed.startsWith("http://") || trimmed.startsWith("https://")
      ? new URL(trimmed)
      : new URL(trimmed.startsWith("/") ? trimmed : `/${trimmed}`, "http://localhost");
    // Hash-routed Electron URLs like `file:///...#/settings/general`
    let rawPathname = base.pathname || "/";
    if (base.hash.startsWith("#/")) {
      const hashPath = base.hash.slice(1).split("?")[0] ?? "/";
      rawPathname = hashPath || "/";
    }
    const normalizedPath =
      rawPathname.length > 1 ? rawPathname.replace(/\/+$/u, "") || "/" : "/";

    const utm: Record<string, string> = {};
    for (const key of [
      "utm_source",
      "utm_medium",
      "utm_campaign",
      "utm_term",
      "utm_content",
    ] as const) {
      const value = base.searchParams.get(key)?.trim();
      if (value) utm[key] = value;
    }

    for (const key of [...base.searchParams.keys()]) {
      if (SENSITIVE_QUERY_KEYS.has(key.toLowerCase())) {
        base.searchParams.delete(key);
      }
    }

    const query = base.searchParams.toString();
    const sanitizedUrl = query ? `${normalizedPath}?${query}` : normalizedPath;
    const hostname =
      trimmed.startsWith("http://") || trimmed.startsWith("https://")
        ? base.hostname
        : "localhost";

    return {
      path: normalizedPath,
      sanitizedUrl,
      hostname,
      utm,
    };
  } catch {
    return {
      path: "/",
      sanitizedUrl: "/",
      hostname: "localhost",
      utm: {},
    };
  }
}

export function classifyHanzoDevice(
  userAgent?: string | null,
  screenWidth?: number | null,
): HanzoAnalyticsDevice {
  const ua = (userAgent ?? "").toLowerCase();
  if (/ipad|tablet|playbook|silk|(android(?!.*mobile))/u.test(ua)) {
    return "tablet";
  }
  if (/iphone|ipod|android.*mobile|windows phone|mobi/u.test(ua)) {
    return "mobile";
  }
  if (typeof screenWidth === "number" && screenWidth > 0) {
    if (screenWidth < 768) return "mobile";
    if (screenWidth < 1024 && /touch/u.test(ua)) return "tablet";
  }
  return "desktop";
}

export function resolveDailySessionKey(now: Date = new Date()): string {
  const yyyy = now.getUTCFullYear();
  const mm = String(now.getUTCMonth() + 1).padStart(2, "0");
  const dd = String(now.getUTCDate()).padStart(2, "0");
  return `${HANZO_SESSION_STORAGE_PREFIX}${yyyy}-${mm}-${dd}`;
}

export interface HanzoAnalyticsStorageLike {
  getItem(key: string): string | null;
  setItem(key: string, value: string): void;
}

export function resolveVisitorAndSessionIds(options?: {
  readonly storage?: HanzoAnalyticsStorageLike | null | undefined;
  readonly hostname?: string | undefined;
  readonly now?: Date | undefined;
}): { readonly visitorId: string; readonly sessionId: string } {
  const now = options?.now ?? new Date();
  const hostTag = (options?.hostname ?? "localhost").toLowerCase();
  const sessionKey = resolveDailySessionKey(now);
  const storage = options?.storage ?? null;

  let visitorId = "";
  let sessionId = "";
  try {
    visitorId = storage?.getItem(HANZO_VISITOR_STORAGE_KEY)?.trim() ?? "";
    sessionId = storage?.getItem(sessionKey)?.trim() ?? "";
  } catch {
    // Ignore restricted storage environments
  }

  if (!visitorId) {
    visitorId = randomHexId("vis");
    try {
      storage?.setItem(HANZO_VISITOR_STORAGE_KEY, visitorId);
    } catch {
      // Ignore storage write errors
    }
  }

  if (!sessionId) {
    const dayBucket = sessionKey.slice(HANZO_SESSION_STORAGE_PREFIX.length);
    sessionId = `ses_${dayBucket.replace(/-/gu, "")}_${fnv1aHex(`${visitorId}:${hostTag}:${dayBucket}`)}`;
    try {
      storage?.setItem(sessionKey, sessionId);
    } catch {
      // Ignore storage write errors
    }
  }

  return { visitorId, sessionId };
}

export interface HanzoAnalyticsContext {
  readonly surface?: HanzoAnalyticsSurface | undefined;
  readonly orgId?: string | null | undefined;
  readonly userId?: string | null | undefined;
  readonly websiteId?: string | undefined;
}

export interface ResolvedHanzoAnalyticsContext {
  readonly surface: HanzoAnalyticsSurface;
  readonly orgId: string | null;
  readonly userId: string | null;
  readonly websiteId: string;
}

export interface HanzoAnalyticsClientOptions {
  readonly beaconUrl?: string | undefined;
  readonly websiteId?: string | undefined;
  readonly surface?: HanzoAnalyticsSurface | undefined;
  readonly storage?: HanzoAnalyticsStorageLike | null | undefined;
  readonly maxRecentEvents?: number | undefined;
  readonly sendImpl?: ((url: string, envelope: HanzoAnalyticsEnvelope) => void) | undefined;
}

export interface HanzoAnalyticsClient {
  readonly track: (
    eventName: HanzoAnalyticsEventName,
    data?: Readonly<Record<string, unknown>>,
  ) => HanzoAnalyticsEnvelope;
  readonly pageView: (
    urlOrPath?: string,
    title?: string,
    data?: Readonly<Record<string, unknown>>,
  ) => HanzoAnalyticsEnvelope;
  readonly setContext: (patch: HanzoAnalyticsContext) => void;
  readonly getContext: () => ResolvedHanzoAnalyticsContext;
  readonly getRecentEvents: () => ReadonlyArray<HanzoAnalyticsEnvelope>;
}

interface BrowserGlobalEnvironment {
  readonly window?: {
    readonly location?: {
      readonly href?: string;
      readonly hostname?: string;
    };
    readonly screen?: {
      readonly width?: number;
      readonly height?: number;
    };
    readonly localStorage?: HanzoAnalyticsStorageLike;
  };
  readonly document?: {
    readonly referrer?: string;
    readonly title?: string;
  };
  readonly navigator?: {
    readonly language?: string;
    readonly userAgent?: string;
    readonly sendBeacon?: (url: string, data?: Blob | string | null) => boolean;
  };
}

function defaultBrowserSend(url: string, envelope: HanzoAnalyticsEnvelope): void {
  const body = JSON.stringify(envelope);
  const nav = (globalThis as unknown as BrowserGlobalEnvironment).navigator;
  if (nav && typeof nav.sendBeacon === "function") {
    try {
      const blob = new Blob([body], { type: "application/json" });
      if (nav.sendBeacon(url, blob)) {
        return;
      }
    } catch {
      // Fall back to fetch keepalive
    }
  }
  if (typeof fetch === "function") {
    void fetch(url, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body,
      keepalive: true,
    }).catch(() => undefined);
  }
}

export function createHanzoAnalyticsClient(
  options: HanzoAnalyticsClientOptions = {},
): HanzoAnalyticsClient {
  const beaconUrl = options.beaconUrl ?? HANZO_LOCAL_BEACON_PATH;
  const maxRecent = options.maxRecentEvents ?? 100;
  const send = options.sendImpl ?? defaultBrowserSend;
  const recentEvents: Array<HanzoAnalyticsEnvelope> = [];

  let context: ResolvedHanzoAnalyticsContext = {
    surface: options.surface ?? "web",
    orgId: null,
    userId: null,
    websiteId: options.websiteId ?? DEFAULT_HANZO_WEBSITE_ID,
  };

  const resolveStorage = (): HanzoAnalyticsStorageLike | null => {
    if (options.storage !== undefined) return options.storage;
    const win = (globalThis as unknown as BrowserGlobalEnvironment).window;
    if (win?.localStorage) {
      try {
        return win.localStorage;
      } catch {
        return null;
      }
    }
    return null;
  };

  const emitEnvelope = (
    type: "pageview" | "event",
    name: string,
    explicitUrlOrPath?: string,
    explicitTitle?: string,
    data: Readonly<Record<string, unknown>> = {},
  ): HanzoAnalyticsEnvelope => {
    const env = globalThis as unknown as BrowserGlobalEnvironment;
    const rawHref = explicitUrlOrPath ?? env.window?.location?.href ?? "/";
    const parsedUrl = normalizeHanzoAnalyticsPath(rawHref);
    const hostname = env.window?.location?.hostname || parsedUrl.hostname;
    const referrer = env.document?.referrer ?? "";
    const title = explicitTitle ?? env.document?.title ?? "";
    const language = env.navigator?.language ?? "en-US";
    const screenWidth = env.window?.screen?.width ?? null;
    const screenHeight = env.window?.screen?.height ?? null;
    const screen =
      screenWidth && screenHeight ? `${screenWidth}x${screenHeight}` : "0x0";
    const ua = env.navigator?.userAgent ?? "";
    const device = classifyHanzoDevice(ua, screenWidth);
    const { visitorId, sessionId } = resolveVisitorAndSessionIds({
      storage: resolveStorage(),
      hostname,
    });

    const orgId =
      typeof data.org_id === "string"
        ? data.org_id
        : typeof data.orgId === "string"
          ? data.orgId
          : context.orgId;
    const surface =
      (data.surface as HanzoAnalyticsSurface | undefined) ?? context.surface;

    const envelope: HanzoAnalyticsEnvelope = {
      type,
      payload: {
        website: context.websiteId,
        name,
        event_id: computeHanzoEventId(name, data),
        hostname,
        path: parsedUrl.path,
        url: parsedUrl.sanitizedUrl,
        referrer,
        title,
        language,
        screen,
        device,
        surface,
        visitor_id: visitorId,
        session_id: sessionId,
        org_id: orgId ?? null,
        user_id: context.userId,
        timestamp: new Date().toISOString(),
        ...parsedUrl.utm,
        data: {
          ...data,
          ...(orgId ? { org_id: orgId } : {}),
          surface,
        },
      },
    };

    recentEvents.push(envelope);
    if (recentEvents.length > maxRecent) {
      recentEvents.splice(0, recentEvents.length - maxRecent);
    }

    send(beaconUrl, envelope);
    return envelope;
  };

  return {
    track: (eventName, data = {}) => emitEnvelope("event", eventName, undefined, undefined, data),
    pageView: (urlOrPath, title, data = {}) =>
      emitEnvelope("pageview", "page_view", urlOrPath, title, data),
    setContext: (patch) => {
      context = {
        surface: patch.surface ?? context.surface,
        orgId: patch.orgId !== undefined ? patch.orgId : context.orgId,
        userId: patch.userId !== undefined ? patch.userId : context.userId,
        websiteId: patch.websiteId ?? context.websiteId,
      };
    },
    getContext: () => context,
    getRecentEvents: () => [...recentEvents],
  };
}
