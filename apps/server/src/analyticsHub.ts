// @effect-diagnostics globalDate:off globalTimers:off processEnvInEffect:off globalFetch:off -- In-memory telemetry ring buffer and fire-and-forget cloud forwarding run outside Effect fibers.
import {
  computeHanzoEventId,
  DEFAULT_HANZO_ANALYTICS_ENDPOINT,
  DEFAULT_HANZO_WEBSITE_ID,
  type HanzoAnalyticsDevice,
  type HanzoAnalyticsEnvelope,
  type HanzoAnalyticsSurface,
  normalizeHanzoAnalyticsPath,
} from "@hanzodev/shared/hanzoAnalytics";

export const MAX_RECENT_ANALYTICS_EVENTS = 100;

export interface StoredHanzoAnalyticsRecord {
  readonly receivedAt: string;
  readonly type: "pageview" | "event";
  readonly payload: HanzoAnalyticsEnvelope["payload"];
}

const recentBuffer: Array<StoredHanzoAnalyticsRecord> = [];

function strOrFallback(value: unknown, fallback: string): string {
  return typeof value === "string" && value.trim().length > 0 ? value.trim() : fallback;
}

export function normalizeIncomingAnalyticsEnvelope(
  rawItem: unknown,
  headers?: Readonly<Record<string, string | undefined>>,
): HanzoAnalyticsEnvelope | null {
  if (!rawItem || typeof rawItem !== "object") return null;
  const obj = rawItem as Record<string, unknown>;
  const rawPayload =
    obj.payload && typeof obj.payload === "object"
      ? (obj.payload as Record<string, unknown>)
      : obj;

  const rawType = obj.type === "pageview" || rawPayload.name === "page_view" ? "pageview" : "event";
  const name = strOrFallback(
    rawPayload.name ?? obj.event ?? obj.name,
    rawType === "pageview" ? "page_view" : "custom_event",
  );
  const rawData =
    rawPayload.data && typeof rawPayload.data === "object"
      ? (rawPayload.data as Record<string, unknown>)
      : (obj.properties && typeof obj.properties === "object"
          ? (obj.properties as Record<string, unknown>)
          : {});

  const urlInput = strOrFallback(rawPayload.url ?? rawPayload.path, "/");
  const normalizedPath = normalizeHanzoAnalyticsPath(urlInput);
  const orgHeader = headers?.["x-org-id"]?.trim() || null;
  const orgId =
    typeof rawPayload.org_id === "string" && rawPayload.org_id.trim()
      ? rawPayload.org_id.trim()
      : typeof rawData.org_id === "string" && rawData.org_id.trim()
        ? rawData.org_id.trim()
        : orgHeader;

  const rawSurface = rawPayload.surface ?? rawData.surface;
  const surface: HanzoAnalyticsSurface =
    rawSurface === "desktop" ||
    rawSurface === "web" ||
    rawSurface === "mobile" ||
    rawSurface === "server"
      ? rawSurface
      : "web";

  const rawDevice = rawPayload.device;
  const device: HanzoAnalyticsDevice =
    rawDevice === "mobile" || rawDevice === "tablet" || rawDevice === "desktop"
      ? rawDevice
      : "desktop";

  const eventId = strOrFallback(rawPayload.event_id, computeHanzoEventId(name, rawData));

  return {
    type: rawType,
    payload: {
      website: strOrFallback(rawPayload.website, DEFAULT_HANZO_WEBSITE_ID),
      name,
      event_id: eventId,
      hostname: strOrFallback(rawPayload.hostname, normalizedPath.hostname),
      path: strOrFallback(rawPayload.path, normalizedPath.path),
      url: normalizedPath.sanitizedUrl,
      referrer: typeof rawPayload.referrer === "string" ? rawPayload.referrer : "",
      title: typeof rawPayload.title === "string" ? rawPayload.title : "",
      language: strOrFallback(rawPayload.language, "en-US"),
      screen: strOrFallback(rawPayload.screen, "0x0"),
      device,
      surface,
      visitor_id: strOrFallback(rawPayload.visitor_id, "vis_local"),
      session_id: strOrFallback(rawPayload.session_id, "ses_local"),
      org_id: orgId ?? null,
      user_id: typeof rawPayload.user_id === "string" ? rawPayload.user_id : null,
      timestamp: strOrFallback(rawPayload.timestamp, new Date().toISOString()),
      ...normalizedPath.utm,
      ...(typeof rawPayload.utm_source === "string" ? { utm_source: rawPayload.utm_source } : {}),
      ...(typeof rawPayload.utm_medium === "string" ? { utm_medium: rawPayload.utm_medium } : {}),
      ...(typeof rawPayload.utm_campaign === "string"
        ? { utm_campaign: rawPayload.utm_campaign }
        : {}),
      data: rawData,
    },
  };
}

function forwardEnvelopeToCloud(envelope: HanzoAnalyticsEnvelope): void {
  if (process.env.HANZO_ANALYTICS_FORWARD === "0" || process.env.VITEST === "true") {
    return;
  }
  const endpoint =
    process.env.HANZO_ANALYTICS_ENDPOINT?.trim() || DEFAULT_HANZO_ANALYTICS_ENDPOINT;
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 2_500);
  timeout.unref?.();
  void fetch(endpoint, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      ...(envelope.payload.org_id ? { "X-Org-Id": envelope.payload.org_id } : {}),
    },
    body: JSON.stringify(envelope),
    signal: controller.signal,
  })
    .catch(() => undefined)
    .finally(() => clearTimeout(timeout));
}

export function ingestAnalyticsBody(
  rawText: string,
  headers?: Readonly<Record<string, string | undefined>>,
): ReadonlyArray<StoredHanzoAnalyticsRecord> {
  const trimmed = rawText.trim();
  if (!trimmed) return [];
  let parsed: unknown;
  try {
    parsed = JSON.parse(trimmed);
  } catch {
    return [];
  }

  const items = Array.isArray(parsed) ? parsed : [parsed];
  const accepted: Array<StoredHanzoAnalyticsRecord> = [];
  const receivedAt = new Date().toISOString();

  for (const item of items) {
    const normalized = normalizeIncomingAnalyticsEnvelope(item, headers);
    if (!normalized) continue;
    const existing = recentBuffer.find(
      (entry) => entry.payload.event_id === normalized.payload.event_id,
    );
    if (existing) {
      accepted.push(existing);
      continue;
    }
    const stored: StoredHanzoAnalyticsRecord = {
      receivedAt,
      type: normalized.type,
      payload: normalized.payload,
    };
    recentBuffer.push(stored);
    accepted.push(stored);
    forwardEnvelopeToCloud(normalized);
  }

  if (recentBuffer.length > MAX_RECENT_ANALYTICS_EVENTS) {
    recentBuffer.splice(0, recentBuffer.length - MAX_RECENT_ANALYTICS_EVENTS);
  }

  return accepted;
}

export function readRecentAnalyticsSnapshot(): {
  readonly ok: true;
  readonly count: number;
  readonly capacity: number;
  readonly events: ReadonlyArray<StoredHanzoAnalyticsRecord>;
} {
  return {
    ok: true,
    count: recentBuffer.length,
    capacity: MAX_RECENT_ANALYTICS_EVENTS,
    events: [...recentBuffer],
  };
}

export function clearRecentAnalyticsForTesting(): void {
  recentBuffer.length = 0;
}
