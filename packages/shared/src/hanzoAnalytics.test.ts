// @effect-diagnostics globalDate:off
import { describe, expect, it } from "vite-plus/test";

import {
  classifyHanzoDevice,
  computeHanzoEventId,
  createHanzoAnalyticsClient,
  type HanzoAnalyticsEnvelope,
  normalizeHanzoAnalyticsPath,
  resolveVisitorAndSessionIds,
} from "./hanzoAnalytics.ts";

describe("hanzoAnalytics", () => {
  it("generates deterministic event_id for order_id / transaction_id and unique IDs otherwise", () => {
    const firstPurchase = computeHanzoEventId("purchase", {
      order_id: "ord_998877",
      value: 15,
      currency: "USD",
    });
    const duplicatePurchase = computeHanzoEventId("purchase", {
      order_id: "ord_998877",
      value: 15,
      currency: "USD",
    });
    const otherPurchase = computeHanzoEventId("purchase", {
      transaction_id: "txn_112233",
    });

    expect(firstPurchase).toBe(duplicatePurchase);
    expect(firstPurchase).toMatch(/^evt_purchase_[0-9a-f]{16}$/u);
    expect(otherPurchase).not.toBe(firstPurchase);

    const adhocA = computeHanzoEventId("thread_started", { projectId: "p1" });
    const adhocB = computeHanzoEventId("thread_started", { projectId: "p1" });
    expect(adhocA).not.toBe(adhocB);
  });

  it("normalizes paths, extracts utm_* tags, and strips sensitive OAuth query params", () => {
    const parsed = normalizeHanzoAnalyticsPath(
      "https://hanzo.app/settings/general/?utm_source=github&utm_campaign=launch&code=secret_oauth_code&state=xyz&org=lux",
    );
    expect(parsed.hostname).toBe("hanzo.app");
    expect(parsed.path).toBe("/settings/general");
    expect(parsed.sanitizedUrl).toBe(
      "/settings/general?utm_source=github&utm_campaign=launch&org=lux",
    );
    expect(parsed.sanitizedUrl).not.toContain("secret_oauth_code");
    expect(parsed.utm).toEqual({
      utm_source: "github",
      utm_campaign: "launch",
    });
  });

  it("rotates session_id daily while preserving visitor_id across days", () => {
    const map = new Map<string, string>();
    const storage = {
      getItem: (k: string) => map.get(k) ?? null,
      setItem: (k: string, v: string) => {
        map.set(k, v);
      },
    };
    const day1 = resolveVisitorAndSessionIds({
      storage,
      hostname: "hanzo.bot",
      now: new Date("2026-11-19T10:00:00Z"),
    });
    const day1Again = resolveVisitorAndSessionIds({
      storage,
      hostname: "hanzo.bot",
      now: new Date("2026-11-19T22:30:00Z"),
    });
    const day2 = resolveVisitorAndSessionIds({
      storage,
      hostname: "hanzo.bot",
      now: new Date("2026-11-20T01:00:00Z"),
    });

    expect(day1.visitorId).toBe(day1Again.visitorId);
    expect(day1.sessionId).toBe(day1Again.sessionId);
    expect(day2.visitorId).toBe(day1.visitorId);
    expect(day2.sessionId).not.toBe(day1.sessionId);
  });

  it("classifies desktop, mobile, and tablet devices and emits structured envelopes with X-Org-Id", () => {
    expect(classifyHanzoDevice("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)", 1440)).toBe(
      "desktop",
    );
    expect(classifyHanzoDevice("Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X)", 390)).toBe(
      "mobile",
    );
    expect(classifyHanzoDevice("Mozilla/5.0 (iPad; CPU OS 18_0 like Mac OS X)", 1024)).toBe(
      "tablet",
    );

    const sent: Array<{ url: string; envelope: HanzoAnalyticsEnvelope }> = [];
    const client = createHanzoAnalyticsClient({
      surface: "desktop",
      storage: null,
      sendImpl: (url, envelope) => sent.push({ url, envelope }),
    });
    client.setContext({ orgId: "lux", userId: "hanzo/z" });

    const pv = client.pageView("https://hanzo.app/welcome/?utm_source=cli", "Welcome");
    expect(pv.type).toBe("pageview");
    expect(pv.payload.org_id).toBe("lux");
    expect(pv.payload.user_id).toBe("hanzo/z");
    expect(pv.payload.surface).toBe("desktop");
    expect(pv.payload.path).toBe("/welcome");
    expect(pv.payload.utm_source).toBe("cli");

    const ev = client.track("begin_checkout", { plan: "pro", value: 15, order_id: "ord_pro_1" });
    expect(ev.type).toBe("event");
    expect(ev.payload.name).toBe("begin_checkout");
    expect(ev.payload.org_id).toBe("lux");
    expect(sent).toHaveLength(2);
    expect(client.getRecentEvents()).toHaveLength(2);
  });
});
