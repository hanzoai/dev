import { useLocation } from "@tanstack/react-router";
import { useEffect, useRef } from "react";

import { getHanzoWebAnalytics } from "../../analytics/hanzoWebAnalytics";
import { getHanzoWebAuthController, useAuth } from "../../cloud/hanzoWebAuth";
import { isElectron } from "../../env";

export function HanzoAnalyticsCoordinator() {
  const { userId, orgId, isSignedIn } = useAuth();
  const location = useLocation({
    select: (loc) => ({
      pathname: loc.pathname,
      searchStr: loc.searchStr,
    }),
  });
  const launchedRef = useRef(false);
  const lastTrackedPathRef = useRef<string | null>(null);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- syncs active Hanzo IAM user and X-Org-Id into the shared analytics client and honors incoming ?org= handoff
  useEffect(() => {
    const analytics = getHanzoWebAnalytics();
    analytics.setContext({
      surface: isElectron ? "desktop" : "web",
      orgId,
      userId,
    });
    if (typeof window !== "undefined") {
      try {
        const params = new URLSearchParams(location.searchStr);
        const incomingOrg = params.get("org")?.trim();
        if (incomingOrg && isSignedIn && incomingOrg !== orgId) {
          getHanzoWebAuthController().switchOrganization(incomingOrg);
        }
      } catch {
        // Ignore malformed URLs
      }
    }
  }, [isSignedIn, location.searchStr, orgId, userId]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- emits app_launched once and page_view on initial load and SPA route transitions
  useEffect(() => {
    const analytics = getHanzoWebAnalytics();
    if (!launchedRef.current) {
      launchedRef.current = true;
      if (isElectron) {
        analytics.track("desktop_app_launched", {
          platform:
            typeof window !== "undefined"
              ? (window.desktopBridge?.getClientPlatform?.() ?? "desktop")
              : "desktop",
          initialPath: location.pathname,
        });
      }
      analytics.track("app_launched", {
        surface: isElectron ? "desktop" : "web",
        initialPath: location.pathname,
      });
    }
    const routeKey = `${location.pathname}${location.searchStr ?? ""}`;
    if (lastTrackedPathRef.current !== routeKey) {
      lastTrackedPathRef.current = routeKey;
      analytics.pageView(
        typeof window !== "undefined" ? window.location.href : location.pathname,
        typeof document !== "undefined" ? document.title : undefined,
      );
    }
  }, [location.pathname, location.searchStr]);

  return null;
}
