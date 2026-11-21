import { discoverEnvironments } from "@hanzodev/client-runtime/discovery";
import { describe, expect, it } from "vite-plus/test";

import { drawerMacs } from "./drawer-macs.logic";

const json = (status: number, body: unknown) =>
  new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json" } });
const machine = (id: string, kind: string, state: string) => ({
  id,
  kind,
  state,
  name: `Mac ${id}`,
});
const discover = (response: () => Response | Promise<Response>) =>
  discoverEnvironments({ accessToken: "t" }, [], { fetch: async () => response() });

describe("drawerMacs", () => {
  it("lists linked desktops, online first, skipping browsers and revoked", async () => {
    const discovery = await discover(() =>
      json(200, {
        sources: [
          machine("a", "desktop", "offline"),
          machine("b", "desktop", "online"),
          machine("c", "browser", "online"),
          machine("d", "desktop", "revoked"),
        ],
      }),
    );
    expect(drawerMacs(discovery)).toEqual([
      { id: "b", name: "Mac b", online: true },
      { id: "a", name: "Mac a", online: false },
    ]);
  });

  it.each([404, 501, 500])("is quietly empty when /v1/sources answers %i", async (status) => {
    expect(drawerMacs(await discover(() => json(status, {})))).toEqual([]);
  });

  it("is empty offline and before discovery runs", async () => {
    expect(drawerMacs(await discover(() => Promise.reject(new Error("offline"))))).toEqual([]);
    expect(drawerMacs(null)).toEqual([]);
  });

  it("does not offer a cloud session as a Mac", async () => {
    expect(drawerMacs(await discover(() => json(200, { sources: [] })))).toEqual([]);
  });
});
