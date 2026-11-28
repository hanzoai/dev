import { describe, expect, it } from "vite-plus/test";

import { DEV_SETTINGS_PATHS, isDevSettingsPath, normalizeDevSettingsPath } from "./DevSettings";

describe("DevSettings path helpers", () => {
  it("recognizes all built-in Hanzo Dev settings paths", () => {
    for (const p of [
      "/settings/general",
      "/settings/appearance",
      "/settings/chat",
      "/settings/diffs",
      "/settings/keybindings",
      "/settings/voice",
      "/settings/snap-shot",
      "/settings/providers",
      "/settings/usage",
      "/settings/threads",
      "/settings/browser",
      "/settings/devices",
      "/settings/scheduled-tasks",
      "/settings/projects",
      "/settings/source-control",
      "/settings/worktrees",
      "/settings/connections",
      "/settings/hanzo",
      "/settings/access",
      "/settings/secrets",
      "/settings/maintenance",
      "/settings/labs",
      "/settings/about",
      "/settings/archived",
      "/settings/diagnostics",
      "/settings/open-source-licenses",
    ]) {
      expect(DEV_SETTINGS_PATHS.has(p)).toBe(true);
      expect(isDevSettingsPath(p)).toBe(true);
      expect(isDevSettingsPath(`${p}/`)).toBe(true);
    }
    expect(isDevSettingsPath("/settings/billing")).toBe(false);
    expect(isDevSettingsPath("/chat")).toBe(false);
  });

  it("normalizes root /settings, extra host sections, trailing slashes, and unknown fallbacks", () => {
    const extras = new Set(["/settings/account", "/settings/billing", "/settings/referrals"]);
    expect(normalizeDevSettingsPath("/settings", extras)).toBe("/settings/general");
    expect(normalizeDevSettingsPath("/settings/", extras)).toBe("/settings/general");
    expect(normalizeDevSettingsPath("/settings/appearance/", extras)).toBe("/settings/appearance");
    expect(normalizeDevSettingsPath("/settings/billing?org=hanzo", extras)).toBe(
      "/settings/billing",
    );
    expect(normalizeDevSettingsPath("/settings/unknown", extras)).toBe("/settings/general");
  });
});
