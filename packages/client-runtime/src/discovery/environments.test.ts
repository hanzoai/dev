import { describe, expect, it } from "vite-plus/test";

import {
  createCloudSession,
  discoverEnvironments,
  mergeEnvironments,
  type DiscoveryFetch,
  type KnownEnvironment,
} from "./environments.ts";

const json = (status: number, body: unknown) =>
  new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json" } });

const mac = {
  id: "m1",
  kind: "desktop",
  name: "Mac",
  os: "darwin",
  arch: "arm64",
  version: "1.0.0",
  state: "online",
  linked_at: "2026-01-01T00:00:00Z",
};
const auth = { accessToken: "tok" };
const known: ReadonlyArray<KnownEnvironment> = [
  { id: "s1", label: "Saved", origin: "saved" },
  { id: "m1", label: "Mac (relay)", origin: "relay" },
];

describe("discoverEnvironments", () => {
  it("orders linked, cloud, saved and dedupes by id", async () => {
    const calls: Array<string> = [];
    const fetch: DiscoveryFetch = async (url, init) => {
      calls.push(`${init?.method} ${url}`);
      return json(200, { sources: [mac, { ...mac, id: "m2", name: "Gone", state: "revoked" }] });
    };
    const result = await discoverEnvironments(auth, known, { fetch });
    expect(result.environments.map((e) => `${e.type}:${e.id}`)).toEqual(["linked:m1", "saved:s1"]);
    expect(result.sources.status).toBe("ok");
    expect(result.showLinkPrompt).toBe(false);
    expect(calls).toEqual(["GET https://api.hanzo.ai/v1/sources"]);
  });

  it("offers cloud, never spawning it, when no machine is linked", async () => {
    const calls: Array<string> = [];
    const fetch: DiscoveryFetch = async (url, init) => {
      calls.push(`${init?.method} ${url}`);
      return json(200, { sources: [] });
    };
    const result = await discoverEnvironments(auth, known, { fetch });
    expect(result.environments.map((e) => `${e.type}:${e.id}`)).toEqual([
      "cloud:cloud",
      "saved:s1",
      "relay:m1",
    ]);
    expect(result.showLinkPrompt).toBe(true);
    expect(calls).toEqual(["GET https://api.hanzo.ai/v1/sources"]);
  });

  it.each([404, 501])("degrades %i to unavailable", async (status: number) => {
    const result = await discoverEnvironments(auth, known, { fetch: async () => json(status, {}) });
    expect(result.sources.status).toBe("unavailable");
    expect(result.cloud.status).toBe("unavailable");
    expect(result.showLinkPrompt).toBe(true);
    expect(result.environments.map((e) => e.id)).toEqual(["s1", "m1"]);
  });

  it("degrades a network failure to unavailable", async () => {
    const result = await discoverEnvironments(auth, [], {
      fetch: async () => {
        throw new TypeError("network");
      },
    });
    expect(result.sources.status).toBe("unavailable");
  });

  it("reports 500 as error", async () => {
    const result = await discoverEnvironments(auth, [], { fetch: async () => json(500, {}) });
    expect(result.sources.status).toBe("error");
  });

  it("makes no request without a token", async () => {
    let called = false;
    const result = await discoverEnvironments({ accessToken: null }, known, {
      fetch: async () => {
        called = true;
        return json(200, {});
      },
    });
    expect(called).toBe(false);
    expect(result.sources.status).toBe("error");
    expect(result.cloud.status).toBe("error");
    expect(result.environments.map((e) => e.id)).toEqual(["s1", "m1"]);
  });
});

describe("createCloudSession", () => {
  it("posts runtime auto and parses the session", async () => {
    let body = "";
    const result = await createCloudSession(auth, {
      fetch: async (_url, init) => {
        body = String(init?.body);
        return json(200, { id: "c1", label: "Cloud", zap: "wss://z", sandbox: "sb" });
      },
    });
    expect(body).toBe('{"runtime":"auto"}');
    expect(result.session).toEqual({ id: "c1", label: "Cloud", zap: "wss://z", sandbox: "sb" });
  });

  it("degrades 501 and rejects a missing token", async () => {
    expect(
      (await createCloudSession(auth, { fetch: async () => json(501, {}) })).report.status,
    ).toBe("unavailable");
    expect((await createCloudSession({ accessToken: null })).report.status).toBe("error");
  });
});

describe("mergeEnvironments", () => {
  it("shows a created session in place of the offer", () => {
    const merged = mergeEnvironments({
      machines: [],
      cloud: { id: "c1", label: "Cloud", zap: "wss://z" },
      known: [],
    });
    expect(merged).toHaveLength(1);
    expect(merged[0]?.type).toBe("cloud");
  });
});
