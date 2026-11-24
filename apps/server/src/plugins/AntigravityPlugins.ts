// @effect-diagnostics nodeBuiltinImport:off
/**
 * Antigravity (`agy`) plugin registry: reads installed plugins under
 * `<GEMINI_HOME>/config/plugins/<name>` and reads/writes enable/disable state in
 * `<GEMINI_HOME>/config/config.json` (`{ plugins: { [name]: { enabled: boolean } } }`).
 */
import * as NodeFSP from "node:fs/promises";
import * as NodePath from "node:path";

import {
  PluginId,
  PluginOperationError,
  type Plugin,
  type PluginListResult,
} from "@hanzodev/contracts";
import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";

export const ANTIGRAVITY_PLUGIN_MARKETPLACE = "antigravity";

const isValidPluginId = Schema.is(PluginId);

const failed = (message: string) => new PluginOperationError({ reason: "failed", message });

const readText = (file: string) => NodeFSP.readFile(file, "utf8").catch(() => undefined);

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const readManifest = async (
  pluginDir: string,
): Promise<{ readonly version: string | null; readonly description: string | null }> => {
  for (const rel of ["plugin.json", ".claude-plugin/plugin.json", "manifest.json"]) {
    const raw = await readText(NodePath.join(pluginDir, rel));
    if (!raw) continue;
    try {
      const parsed = JSON.parse(raw) as unknown;
      if (!isRecord(parsed)) continue;
      const version =
        typeof parsed.version === "string" && parsed.version.trim() ? parsed.version.trim() : null;
      const description =
        typeof parsed.description === "string" && parsed.description.trim()
          ? parsed.description.trim()
          : null;
      return { version, description };
    } catch {
      // Ignore malformed manifest.
    }
  }
  return { version: null, description: null };
};

export const isAntigravityPluginId = (id: string): boolean =>
  id.endsWith(`@${ANTIGRAVITY_PLUGIN_MARKETPLACE}`);

function parseJsonObject(raw: string): Record<string, unknown> | null {
  try {
    const parsed = JSON.parse(raw) as unknown;
    return isRecord(parsed) ? { ...parsed } : {};
  } catch {
    return null;
  }
}

function serializeJsonObject(value: Record<string, unknown>): string {
  return `${JSON.stringify(value, null, 2)}\n`;
}

export const listAntigravityPlugins = Effect.fn("AntigravityPlugins.list")(function* (
  geminiHome: string,
  query: string | undefined,
): Effect.fn.Return<PluginListResult, PluginOperationError> {
  return yield* Effect.promise(async () => {
    const pluginsDir = NodePath.join(geminiHome, "config", "plugins");
    const entries = await NodeFSP.readdir(pluginsDir, { withFileTypes: true }).catch(() => []);
    const dirs = entries
      .filter((entry) => entry.isDirectory() && !entry.name.startsWith("."))
      .map((entry) => entry.name)
      .sort((a, b) => a.localeCompare(b));
    if (dirs.length === 0) return { plugins: [], more: 0 };

    const configRaw = await readText(NodePath.join(geminiHome, "config", "config.json"));
    const parsedConfig = configRaw ? parseJsonObject(configRaw) : null;
    const pluginPrefs: Record<string, unknown> =
      parsedConfig && isRecord(parsedConfig.plugins) ? parsedConfig.plugins : {};

    const needle = query?.trim().toLowerCase() ?? "";
    const plugins: Plugin[] = [];
    for (const name of dirs) {
      const id = `${name}@${ANTIGRAVITY_PLUGIN_MARKETPLACE}`;
      if (!isValidPluginId(id)) continue;
      const manifest = await readManifest(NodePath.join(pluginsDir, name));
      const pref = pluginPrefs[name];
      const enabled = !(isRecord(pref) && pref.enabled === false);
      const plugin: Plugin = {
        id,
        name,
        provider: "antigravity",
        marketplace: ANTIGRAVITY_PLUGIN_MARKETPLACE,
        version: manifest.version,
        description: manifest.description,
        installed: true,
        enabled,
      };
      if (
        needle.length === 0 ||
        `${plugin.id} ${plugin.provider ?? ""} ${plugin.description ?? ""}`
          .toLowerCase()
          .includes(needle)
      ) {
        plugins.push(plugin);
      }
    }
    return { plugins, more: 0 };
  });
});

export const setAntigravityPluginEnabled = Effect.fn("AntigravityPlugins.setEnabled")(function* (
  geminiHome: string,
  id: PluginId,
  enabled: boolean,
): Effect.fn.Return<{ readonly plugin: Plugin }, PluginOperationError> {
  const name = id.slice(0, -(ANTIGRAVITY_PLUGIN_MARKETPLACE.length + 1));
  const configDir = NodePath.join(geminiHome, "config");
  const configPath = NodePath.join(configDir, "config.json");
  const raw = yield* Effect.promise(() => readText(configPath));
  let root: Record<string, unknown> = {};
  if (raw?.trim()) {
    const parsed = parseJsonObject(raw);
    if (parsed === null) {
      return yield* failed("Antigravity config.json is not valid JSON");
    }
    root = parsed;
  }
  const existingPlugins = isRecord(root.plugins) ? { ...root.plugins } : {};
  const existingEntry = isRecord(existingPlugins[name]) ? { ...existingPlugins[name] } : {};
  existingPlugins[name] = { ...existingEntry, enabled };
  root.plugins = existingPlugins;

  yield* Effect.tryPromise({
    try: async () => {
      await NodeFSP.mkdir(configDir, { recursive: true });
      await NodeFSP.writeFile(configPath, serializeJsonObject(root), "utf8");
    },
    catch: (cause) =>
      failed(cause instanceof Error ? cause.message : "could not update Antigravity config.json"),
  });

  const listing = yield* listAntigravityPlugins(geminiHome, name);
  const plugin = listing.plugins.find((entry) => entry.id === id);
  if (!plugin) return yield* failed(`${id} is not installed in Antigravity`);
  return { plugin };
});
