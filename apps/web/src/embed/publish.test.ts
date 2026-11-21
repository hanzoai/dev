import { afterEach, describe, expect, it, vi } from "vitest";

import { buildStatus, isGreen, label, publish } from "./publish";

const cloud = { api: "https://api.test", token: async () => "t", org: "hanzo" };

afterEach(() => vi.unstubAllGlobals());

describe("publish", () => {
  it("declares the repository on a branch with the platform's own field names", async () => {
    const calls: Array<{ url: string; init: RequestInit }> = [];
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string, init: RequestInit) => {
        calls.push({ url, init });
        return Response.json({
          build: { id: "b1", status: "queued" },
          declaration: { live: false, review: "https://git.test/review/1", mode: "branch", ref: "main" },
          notice: "A review was opened.",
        });
      }),
    );
    const out = await publish(cloud, { repo: "https://github.com/hanzoai/post.git", ref: "main", name: "Post App" });
    expect(calls[0]!.url).toBe("https://api.test/v1/platform/apps");
    expect(JSON.parse(String(calls[0]!.init.body))).toEqual({
      repo: "https://github.com/hanzoai/post.git",
      ref: "main",
      name: "post-app",
      partOf: "post-app",
      mode: "branch",
    });
    expect(new Headers(calls[0]!.init.headers).get("X-Org-Id")).toBe("hanzo");
    expect(out).toEqual({
      build: { id: "b1", status: "queued" },
      live: false,
      review: "https://git.test/review/1",
      notice: "A review was opened.",
    });
  });

  it("shows only an https review link and the platform's own refusal", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => Response.json({ declaration: { review: "javascript:alert(1)" } })),
    );
    expect((await publish(cloud, { repo: "r", ref: "", name: "x" })).review).toBeNull();
    vi.stubGlobal("fetch", vi.fn(async () => Response.json({ error: "No such repository." }, { status: 422 })));
    await expect(publish(cloud, { repo: "r", ref: "main", name: "x" })).rejects.toThrow("No such repository.");
  });

  it("reads one build's status from the builds list", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => Response.json({ builds: [{ id: "a", status: "failed" }, { id: "b1", status: "succeeded" }] })),
    );
    expect(await buildStatus(cloud, "b1")).toBe("succeeded");
    expect(await buildStatus(cloud, "zzz")).toBeNull();
    expect(isGreen("Succeeded")).toBe(true);
    expect(isGreen("building")).toBe(false);
    expect(label("My  App!")).toBe("my-app");
  });
});
