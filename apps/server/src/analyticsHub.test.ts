import { afterEach, describe, expect, it } from "vite-plus/test";

import {
  clearRecentAnalyticsForTesting,
  ingestAnalyticsBody,
  MAX_RECENT_ANALYTICS_EVENTS,
  readRecentAnalyticsSnapshot,
} from "./analyticsHub.ts";

describe("analyticsHub", () => {
  afterEach(() => {
    clearRecentAnalyticsForTesting();
  });

  it("ingests pageview and custom beacon events with X-Org-Id and caps buffer at 100", () => {
    const accepted = ingestAnalyticsBody(
      JSON.stringify({
        type: "event",
        payload: {
          name: "app_launched",
          surface: "desktop",
          url: "/welcome?code=secret123&utm_source=desktop",
          data: { version: "0.1.10" },
        },
      }),
      { "x-org-id": "hanzo" },
    );

    expect(accepted).toHaveLength(1);
    expect(accepted[0]?.payload.name).toBe("app_launched");
    expect(accepted[0]?.payload.org_id).toBe("hanzo");
    expect(accepted[0]?.payload.surface).toBe("desktop");
    expect(accepted[0]?.payload.url).toBe("/welcome?utm_source=desktop");

    for (let i = 0; i < 115; i += 1) {
      ingestAnalyticsBody(
        JSON.stringify({
          type: "event",
          payload: { name: "turn_submitted", data: { index: i } },
        }),
      );
    }

    const snapshot = readRecentAnalyticsSnapshot();
    expect(snapshot.count).toBe(MAX_RECENT_ANALYTICS_EVENTS);
    expect(snapshot.events.at(-1)?.payload.data.index).toBe(114);
  });

  it("deduplicates repeated deterministic event_id submissions in the ring buffer", () => {
    const purchaseBody = JSON.stringify({
      type: "event",
      payload: {
        name: "purchase",
        surface: "desktop",
        data: { order_id: "ord_dedup_99", value: 15 },
      },
    });
    const first = ingestAnalyticsBody(purchaseBody, { "x-org-id": "hanzo" });
    const second = ingestAnalyticsBody(purchaseBody, { "x-org-id": "hanzo" });

    expect(first[0]?.payload.event_id).toBe(second[0]?.payload.event_id);
    expect(readRecentAnalyticsSnapshot().count).toBe(1);
  });
});
