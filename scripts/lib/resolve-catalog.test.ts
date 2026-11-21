import { assert, describe, it } from "@effect/vitest";

import { resolveCatalogDependencies } from "./resolve-catalog.ts";

const catalog = { effect: "4.0.0-rc.115", jose: "6.2.2", react: "19.2.0" };

describe("resolveCatalogDependencies", () => {
  it("resolves bare, named and override-selector catalog specs like pnpm", () => {
    assert.deepStrictEqual(
      resolveCatalogDependencies(
        {
          jose: "catalog:",
          "react-dom": "catalog:react",
          "@opencode/protocol>effect": "catalog:",
          "dbus-next>usocket": "-",
          lodash: "4.17.21",
        },
        catalog,
        "apps/desktop",
      ),
      {
        jose: "6.2.2",
        "react-dom": "19.2.0",
        "@opencode/protocol>effect": "4.0.0-rc.115",
        "dbus-next>usocket": "-",
        lodash: "4.17.21",
      },
    );
  });

  it("resolves version-qualified overrides without changing their selectors", () => {
    assert.deepStrictEqual(
      resolveCatalogDependencies(
        {
          "undici@^8": "catalog:",
          "ws@^8": "catalog:",
          "jose@^3": "catalog:",
          "@scope/parent@^1>undici@^8": "catalog:",
          "parent@^1>jose@^3": "catalog:",
        },
        { ...catalog, undici: "8.11.2", ws: "8.21.0" },
        "apps/desktop",
      ),
      {
        "undici@^8": "8.11.2",
        "ws@^8": "8.21.0",
        "jose@^3": "6.2.2",
        "@scope/parent@^1>undici@^8": "8.11.2",
        "parent@^1>jose@^3": "6.2.2",
      },
    );
  });
});
