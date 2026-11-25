import { describe, expect, it } from "vite-plus/test";
import {
  BUILT_IN_THEME_IDS,
  BUILT_IN_THEMES,
  BUILT_IN_THEMES_BY_ID,
  CLAY_THEME,
  EMBER_THEME,
  getBuiltInTheme,
  getDefaultThemeColors,
  getThemeColorsForAppearance,
  GROVE_THEME,
  HANZO_DEV_DARK_THEME_COLORS,
  HANZO_DEV_LIGHT_THEME_COLORS,
  IRIS_THEME,
  isBuiltInThemeId,
  LEGACY_THEME_ID_ALIASES,
  OCEAN_THEME,
  RESERVED_THEME_IDS,
  resolveLegacyThemeId,
  resolveThemeId,
  ZEN_THEME,
  type AppearanceConfig,
} from "./themePalettes.ts";

describe("themePalettes", () => {
  it("exposes the canonical built-in themes without hanzo-chat", () => {
    expect(BUILT_IN_THEME_IDS).toEqual(["zen", "clay", "grove", "ocean", "ember", "iris"]);
    expect(BUILT_IN_THEMES.map((theme) => theme.id)).toEqual([...BUILT_IN_THEME_IDS]);
    expect(Object.keys(BUILT_IN_THEMES_BY_ID)).toEqual([...BUILT_IN_THEME_IDS]);
    expect(BUILT_IN_THEMES_BY_ID.zen).toBe(ZEN_THEME);
    expect(BUILT_IN_THEMES_BY_ID.clay).toBe(CLAY_THEME);
    expect(BUILT_IN_THEMES_BY_ID.grove).toBe(GROVE_THEME);
    expect(BUILT_IN_THEMES_BY_ID.ocean).toBe(OCEAN_THEME);
    expect(BUILT_IN_THEMES_BY_ID.ember).toBe(EMBER_THEME);
    expect(BUILT_IN_THEMES_BY_ID.iris).toBe(IRIS_THEME);
    expect(isBuiltInThemeId("zen")).toBe(true);
    expect(isBuiltInThemeId("hanzo-chat")).toBe(false);
    expect(isBuiltInThemeId("lelouch")).toBe(false);
  });

  it("silently migrates retired legacy theme IDs while keeping them reserved", () => {
    expect(LEGACY_THEME_ID_ALIASES["hanzo-chat"]).toBe("zen");
    expect(LEGACY_THEME_ID_ALIASES["hanzo-chat-dark"]).toBe("zen");
    expect(LEGACY_THEME_ID_ALIASES.lelouch).toBe("clay");

    expect(RESERVED_THEME_IDS.has("hanzo-chat")).toBe(true);
    expect(RESERVED_THEME_IDS.has("hanzo-chat-dark")).toBe(true);
    expect(RESERVED_THEME_IDS.has("lelouch")).toBe(true);

    expect(resolveLegacyThemeId("hanzo-chat")).toBe("zen");
    expect(resolveLegacyThemeId("hanzo-chat-dark")).toBe("zen");
    expect(resolveLegacyThemeId("lelouch")).toBe("clay");
    expect(resolveLegacyThemeId("custom-theme")).toBe("custom-theme");

    expect(resolveThemeId("hanzo-chat")).toBe("zen");
    expect(resolveThemeId("hanzo-chat-dark")).toBe("zen");
    expect(resolveThemeId("lelouch")).toBe("clay");
    expect(resolveThemeId("system")).toBe("system");
    expect(resolveThemeId("unknown-id")).toBe("zen");
    expect(resolveThemeId(null, "clay")).toBe("clay");

    expect(getBuiltInTheme("hanzo-chat")).toBe(ZEN_THEME);
    expect(getBuiltInTheme("hanzo-chat-dark")).toBe(ZEN_THEME);
    expect(getBuiltInTheme("lelouch")).toBe(CLAY_THEME);
    expect(getBuiltInTheme("unknown-id")).toBeNull();
  });

  it("aligns default colors and AppearanceConfig contract with @hanzo/theme", () => {
    expect(getDefaultThemeColors("light")).toBe(HANZO_DEV_LIGHT_THEME_COLORS);
    expect(getDefaultThemeColors("dark")).toBe(HANZO_DEV_DARK_THEME_COLORS);
    expect(getThemeColorsForAppearance(ZEN_THEME, "light")).toBe(ZEN_THEME.colors);
    expect(getThemeColorsForAppearance(ZEN_THEME, "dark")).toBe(ZEN_THEME.variants?.dark);

    const config: AppearanceConfig = {
      theme: resolveThemeId("hanzo-chat"),
      borderRadius: 8,
      fontSize: 14,
      typeRatio: 1.2,
      fontFamily: "sans",
      lineHeight: "normal",
      density: "comfortable",
      contrast: "default",
      accent: "default",
    };
    expect(config.theme).toBe("zen");
  });
});
