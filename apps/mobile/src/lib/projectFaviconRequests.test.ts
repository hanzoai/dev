import { describe, expect, it } from "vite-plus/test";

import {
  beginProjectFaviconRequest,
  createProjectFaviconRequest,
  hasLoadedProjectFavicon,
  markProjectFaviconFailed,
  markProjectFaviconLoaded,
} from "./projectFaviconRequests";

describe("project favicon requests", () => {
  it("ignores callbacks from a superseded URL", () => {
    const cacheKey = "environment-1:/workspace:v1-favicon.svg";
    const expiredUrl = "https://environment.example/v1/assets/expired/v1-favicon.svg";
    const refreshedUrl = "https://environment.example/v1/assets/refreshed/v1-favicon.svg";

    const expiredRequest = createProjectFaviconRequest(cacheKey, expiredUrl);
    const endExpiredRequest = beginProjectFaviconRequest(expiredRequest);
    markProjectFaviconLoaded(expiredRequest);
    const refreshedRequest = createProjectFaviconRequest(cacheKey, refreshedUrl);
    const endRefreshedRequest = beginProjectFaviconRequest(refreshedRequest);

    expect(markProjectFaviconLoaded(expiredRequest)).toBe(false);
    expect(markProjectFaviconFailed(expiredRequest)).toBe(false);
    expect(hasLoadedProjectFavicon(cacheKey)).toBe(true);
    expect(markProjectFaviconFailed(refreshedRequest)).toBe(true);
    expect(hasLoadedProjectFavicon(cacheKey)).toBe(false);

    endRefreshedRequest();
    endExpiredRequest();
  });

  it("does not supersede a request until the next request begins", () => {
    const cacheKey = "environment-1:/workspace:v3-favicon.svg";
    const committedUrl = "https://environment.example/v1/assets/current/v3-favicon.svg";
    const abandonedUrl = "https://environment.example/v1/assets/abandoned/v3-favicon.svg";
    const committedRequest = createProjectFaviconRequest(cacheKey, committedUrl);
    const endCommittedRequest = beginProjectFaviconRequest(committedRequest);

    createProjectFaviconRequest(cacheKey, abandonedUrl);

    expect(markProjectFaviconLoaded(committedRequest)).toBe(true);
    expect(hasLoadedProjectFavicon(cacheKey)).toBe(true);

    endCommittedRequest();
  });

  it("restores the remaining active URL when a newer request ends", () => {
    const cacheKey = "environment-1:/workspace:v4-favicon.svg";
    const firstRequest = createProjectFaviconRequest(
      cacheKey,
      "https://environment.example/v1/assets/first/v4-favicon.svg",
    );
    const secondRequest = createProjectFaviconRequest(
      cacheKey,
      "https://environment.example/v1/assets/second/v4-favicon.svg",
    );
    const endFirstRequest = beginProjectFaviconRequest(firstRequest);
    const endSecondRequest = beginProjectFaviconRequest(secondRequest);

    expect(markProjectFaviconLoaded(firstRequest)).toBe(false);
    endSecondRequest();
    expect(markProjectFaviconLoaded(firstRequest)).toBe(true);
    endFirstRequest();
    expect(markProjectFaviconLoaded(firstRequest)).toBe(false);
  });

  it("bounds remembered loaded revisions", () => {
    const firstCacheKey = "environment-1:/workspace:revision-0";
    let lastCacheKey = firstCacheKey;

    for (let revision = 0; revision < 300; revision++) {
      lastCacheKey = `environment-1:/workspace:revision-${revision}`;
      const request = createProjectFaviconRequest(
        lastCacheKey,
        `https://environment.example/v1/assets/revision-${revision}/favicon.svg`,
      );
      const endRequest = beginProjectFaviconRequest(request);
      markProjectFaviconLoaded(request);
      endRequest();
    }

    expect(hasLoadedProjectFavicon(firstCacheKey)).toBe(false);
    expect(hasLoadedProjectFavicon(lastCacheKey)).toBe(true);
  });
});
