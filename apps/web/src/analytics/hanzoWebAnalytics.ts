import {
  createHanzoAnalyticsClient,
  type HanzoAnalyticsClient,
  type HanzoAnalyticsEnvelope,
  type HanzoAnalyticsEventName,
} from "@hanzodev/shared/hanzoAnalytics";

import { isElectron } from "../env";

export interface HanzoWindowNamespace {
  readonly track: (
    eventName: HanzoAnalyticsEventName,
    data?: Readonly<Record<string, unknown>>,
  ) => HanzoAnalyticsEnvelope;
  readonly pageView: (
    urlOrPath?: string,
    title?: string,
    data?: Readonly<Record<string, unknown>>,
  ) => HanzoAnalyticsEnvelope;
  readonly getRecentEvents: () => ReadonlyArray<HanzoAnalyticsEnvelope>;
}

declare global {
  interface Window {
    hanzo?: HanzoWindowNamespace;
  }
}

let sharedAnalyticsClient: HanzoAnalyticsClient | null = null;

function resolveBeaconUrl(): string {
  if (typeof window !== "undefined") {
    const protocol = window.location?.protocol ?? "";
    if (protocol === "tauri:" || protocol === "asset:" || protocol === "file:") {
      const baseUrl = window.desktopBridge
        ?.getLocalEnvironmentBootstraps?.()[0]
        ?.httpBaseUrl?.replace(/\/+$/u, "");
      if (baseUrl) {
        return `${baseUrl}/_/beacon`;
      }
    }
  }
  return "/_/beacon";
}

export function getHanzoWebAnalytics(): HanzoAnalyticsClient {
  if (!sharedAnalyticsClient) {
    sharedAnalyticsClient = createHanzoAnalyticsClient({
      surface: isElectron ? "desktop" : "web",
      beaconUrl: resolveBeaconUrl(),
    });
    if (typeof window !== "undefined") {
      window.hanzo = {
        track: (eventName, data) => sharedAnalyticsClient!.track(eventName, data),
        pageView: (urlOrPath, title, data) =>
          sharedAnalyticsClient!.pageView(urlOrPath, title, data),
        getRecentEvents: () => sharedAnalyticsClient!.getRecentEvents(),
      };
    }
  }
  return sharedAnalyticsClient;
}

export function trackHanzoEvent(
  eventName: HanzoAnalyticsEventName,
  data?: Readonly<Record<string, unknown>>,
): void {
  try {
    getHanzoWebAnalytics().track(eventName, data);
  } catch {
    // Telemetry must never interrupt UI actions.
  }
}
