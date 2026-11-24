import { describe, expect, it } from "vite-plus/test";

import {
  formatMemoriesSystemContext,
  listConnectors,
  listDesignSystems,
  loadMemoriesSystemContext,
  parseConnectors,
  parseDesignSystems,
  parseMemories,
  shelfDraftText,
} from "./shelves.ts";

describe("cloud shelves", () => {
  it("reads connectors from /v1/tool/mcp/servers", () => {
    expect(
      parseConnectors({
        servers: [
          { id: "stripe", name: "Stripe", url: "https://mcp.stripe.com/x" },
          { id: "bare", url: "not a url" },
          { name: "no id" },
        ],
      }),
    ).toEqual([
      { id: "stripe", name: "Stripe", note: "mcp.stripe.com" },
      { id: "bare", name: "bare", note: "" },
    ]);
    expect(parseConnectors(null)).toEqual([]);
  });

  it("reads design systems from /v1/studio/brands", () => {
    expect(
      parseDesignSystems({
        data: [{ id: "b1", name: "Hanzo", tokens: { "--primary": "#fff", "--accent": "#000" } }],
      }),
    ).toEqual([{ id: "b1", name: "Hanzo", note: "#fff #000" }]);
  });

  it("reads memories, a bare list or wrapped", () => {
    expect(parseMemories([{ id: "m1", content: "Prefers Go" }, { content: "" }])).toEqual([
      { id: "m1", name: "Prefers Go", note: "" },
    ]);
    expect(parseMemories({ data: [{ content: "Lives in SF" }] })).toEqual([
      { id: "0", name: "Lives in SF", note: "" },
    ]);
  });

  it("words what choosing a row writes", () => {
    const row = { id: "x", name: "Stripe", note: "" };
    expect(shelfDraftText("connectors", row)).toBe("Use the Stripe connector to ");
    expect(shelfDraftText("design", row)).toBe("Use the Stripe design system: ");
  });

  it("formats and loads memories as system context without blocking on errors", async () => {
    expect(formatMemoriesSystemContext([])).toBeUndefined();
    expect(
      formatMemoriesSystemContext([
        { id: "m1", name: "Prefers Go", note: "" },
        { id: "m2", name: "Lives in SF", note: "" },
      ]),
    ).toBe("User memories:\n- Prefers Go\n- Lives in SF");

    const okFetch = async () =>
      new Response(JSON.stringify([{ id: "m1", content: "Prefers Go" }]), { status: 200 });
    expect(await loadMemoriesSystemContext({ accessToken: "tok" }, { fetch: okFetch })).toBe(
      "User memories:\n- Prefers Go",
    );
    expect(await loadMemoriesSystemContext({ accessToken: null }, { fetch: okFetch })).toBe(
      undefined,
    );
    const failFetch = async () => new Response("", { status: 500 });
    expect(await loadMemoriesSystemContext({ accessToken: "tok" }, { fetch: failFetch })).toBe(
      undefined,
    );
  });

  it("asks api.hanzo.ai with the person's token and says why it cannot", async () => {
    const seen: Array<{ url: string; auth: string | null }> = [];
    const fetch = async (url: string, init?: RequestInit) => {
      seen.push({ url, auth: new Headers(init?.headers).get("authorization") });
      return new Response(
        JSON.stringify({ servers: [{ id: "a", name: "A", url: "https://a.dev" }] }),
        { status: 200 },
      );
    };
    const rows = await listConnectors({ accessToken: "tok" }, { fetch });
    expect(rows.map((row) => row.id)).toEqual(["a"]);
    expect(seen[0]).toEqual({
      url: "https://api.hanzo.ai/v1/tool/mcp/servers",
      auth: "Bearer tok",
    });
    await expect(listDesignSystems({ accessToken: null }, { fetch })).rejects.toMatchObject({
      kind: "signed-out",
    });
    const denied = async () => new Response("", { status: 403 });
    await expect(listDesignSystems({ accessToken: "t" }, { fetch: denied })).rejects.toMatchObject({
      kind: "unauthorized",
    });
  });
});
