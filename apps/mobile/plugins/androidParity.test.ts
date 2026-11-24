import * as NodeFS from "node:fs";
import * as NodePath from "node:path";
import { describe, expect, it } from "vite-plus/test";

const MOBILE_ROOT = NodePath.resolve(import.meta.dirname, "..");

function toValidAndroidResourceName(value: string): string {
  return NodePath.parse(value)
    .name.toLowerCase()
    .replace(/[^a-z0-9]/g, "_");
}

function extractFontResources(pluginFilename: string): ReadonlyArray<string> {
  const source = NodeFS.readFileSync(NodePath.join(MOBILE_ROOT, "plugins", pluginFilename), "utf8");
  return [...source.matchAll(/@font\/([a-z0-9_]+)/g)].map((match) => match[1] ?? "");
}

describe("Android Zen font & alternate icon parity", () => {
  it("matches expo-font Android XML font resource names in alert dialog and popup menu plugins", () => {
    const appConfigSource = NodeFS.readFileSync(
      NodePath.join(MOBILE_ROOT, "app.config.ts"),
      "utf8",
    );
    const declaredFamilies = [...appConfigSource.matchAll(/fontFamily:\s*"([^"]+)"/g)].map(
      (match) => match[1] ?? "",
    );

    expect(declaredFamilies).toEqual(["Zen-Regular", "Zen-Medium", "Zen-Bold"]);

    const generatedXmlResources = new Set(
      declaredFamilies.map((family) => `xml_${toValidAndroidResourceName(family)}`),
    );

    const alertFonts = extractFontResources("withAndroidModernAlertDialog.cjs");
    const popupFonts = extractFontResources("withAndroidModernPopupMenu.cjs");

    expect(alertFonts.length).toBeGreaterThan(0);
    expect(popupFonts.length).toBeGreaterThan(0);
    for (const resource of [...alertFonts, ...popupFonts]) {
      expect(generatedXmlResources.has(resource)).toBe(true);
    }
  });

  it("declares Android adaptive icons alongside iOS icons for all alternate app icons", () => {
    const appConfigSource = NodeFS.readFileSync(
      NodePath.join(MOBILE_ROOT, "app.config.ts"),
      "utf8",
    );
    const pluginBlockStart = appConfigSource.indexOf('"expo-alternate-app-icons"');
    expect(pluginBlockStart).toBeGreaterThan(-1);
    const pluginBlock = appConfigSource.slice(pluginBlockStart, pluginBlockStart + 900);

    expect(pluginBlock).toContain('name: "DevHanzo"');
    expect(pluginBlock).toContain('name: "Dev"');
    const androidMatches = [...pluginBlock.matchAll(/android:\s*\{[\s\S]*?foregroundImage:/g)];
    expect(androidMatches).toHaveLength(2);
  });
});
