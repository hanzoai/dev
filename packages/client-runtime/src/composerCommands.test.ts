import type { Plugin } from "@hanzodev/contracts";
import { describe, expect, it } from "vite-plus/test";

import { pluginAction, pluginSections, workspaceSlashCommands } from "./composerCommands.ts";
import { PLUS_ENTRY_IDS, plusEntries } from "./composerPlus.ts";

const plugin = (id: string, installed: boolean, enabled: boolean): Plugin => ({
  id,
  name: id.split("@")[0]!,
  marketplace: "m",
  version: null,
  description: null,
  installed,
  enabled,
});

describe("workspace slash commands", () => {
  it("offers /skills and /plugins", () => {
    expect(workspaceSlashCommands().map((entry) => entry.command)).toEqual(["skills", "plugins"]);
    for (const entry of workspaceSlashCommands())
      expect(entry.description.length).toBeGreaterThan(0);
  });

  it("installs what is not installed, then toggles", () => {
    expect(pluginAction(plugin("a@m", false, false))).toBe("install");
    expect(pluginAction(plugin("a@m", true, false))).toBe("enable");
    expect(pluginAction(plugin("a@m", true, true))).toBe("disable");
  });

  it("lists installed plugins first, those on before those off", () => {
    const sections = pluginSections([
      plugin("z@m", true, false),
      plugin("offer@m", false, false),
      plugin("b@m", true, true),
      plugin("a@m", true, true),
    ]);
    expect(sections.installed.map((entry) => entry.id)).toEqual(["a@m", "b@m", "z@m"]);
    expect(sections.offered.map((entry) => entry.id)).toEqual(["offer@m"]);
  });
});

describe("the + menu", () => {
  it("has every entry of hanzo.app's composer", () => {
    expect(PLUS_ENTRY_IDS).toEqual([
      "upload",
      "library",
      "screenshot",
      "createImage",
      "browserTabs",
      "webSearch",
      "research",
      "sketch",
      "thinking",
      "agents",
      "skills",
      "connectors",
      "design",
      "plugins",
      "devices",
      "memory",
    ]);
  });

  it("leaves out what a surface cannot do", () => {
    const entries = plusEntries(undefined, new Set(["screenshot", "sketch"]));
    expect(entries.map((entry) => entry.id)).not.toContain("screenshot");
    expect(entries.map((entry) => entry.id)).not.toContain("sketch");
    expect(entries).toHaveLength(PLUS_ENTRY_IDS.length - 2);
  });

  it("words every entry", () => {
    for (const entry of plusEntries()) {
      expect(entry.label.length).toBeGreaterThan(0);
      expect(entry.note.length).toBeGreaterThan(0);
    }
  });
});
