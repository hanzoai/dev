// @effect-diagnostics nodeBuiltinImport:off
/**
 * Codex plugin registry: reads installed plugins and bundled marketplace offers under
 * `CODEX_HOME` (`~/.codex`), and persists enable/disable state in `config.toml` under
 * `[plugins."<id>"]`.
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

import { PLUGIN_OFFER_LIMIT } from "./ClaudePlugins.ts";

const isValidPluginId = Schema.is(PluginId);

const failed = (message: string) => new PluginOperationError({ reason: "failed", message });

const readText = (file: string) => NodeFSP.readFile(file, "utf8").catch(() => undefined);

const listSubdirs = async (dir: string): Promise<string[]> => {
  const entries = await NodeFSP.readdir(dir, { withFileTypes: true }).catch(() => []);
  return entries
    .filter((entry) => entry.isDirectory() && !entry.name.startsWith("."))
    .map((entry) => entry.name)
    .sort((a, b) => a.localeCompare(b));
};

interface PluginManifest {
  readonly version: string | null;
  readonly description: string | null;
}

const readManifest = async (pluginDir: string): Promise<PluginManifest> => {
  for (const rel of [".codex-plugin/plugin.json", ".claude-plugin/plugin.json", "plugin.json"]) {
    const raw = await readText(NodePath.join(pluginDir, rel));
    if (!raw) continue;
    try {
      const parsed = JSON.parse(raw) as Record<string, unknown>;
      const version =
        typeof parsed.version === "string" && parsed.version.trim() ? parsed.version.trim() : null;
      const description =
        typeof parsed.description === "string" && parsed.description.trim()
          ? parsed.description.trim()
          : null;
      return { version, description };
    } catch {
      // Ignore malformed manifest and try next candidate.
    }
  }
  return { version: null, description: null };
};

/** Parses `[plugins."<id>"]` sections from Codex `config.toml`. */
export function parseCodexConfigPlugins(toml: string): Map<string, boolean> {
  const result = new Map<string, boolean>();
  let currentId: string | null = null;
  for (const rawLine of toml.split(/\r?\n/u)) {
    const line = rawLine.trim();
    if (!line || line.startsWith("#")) continue;
    const header = /^\[([^\]]+)\]/u.exec(line);
    if (header) {
      const section = header[1]!.trim();
      const quoted = /^plugins\."([^"]+)"$/u.exec(section);
      const bare = /^plugins\.([A-Za-z0-9._-]+)$/u.exec(section);
      currentId = quoted?.[1] ?? bare?.[1] ?? null;
      if (currentId && !result.has(currentId)) {
        result.set(currentId, true);
      }
      continue;
    }
    if (currentId !== null) {
      const enabledMatch = /^enabled\s*=\s*(true|false)\b/u.exec(line);
      if (enabledMatch) {
        result.set(currentId, enabledMatch[1] === "true");
      }
    }
  }
  return result;
}

/** Updates or appends `[plugins."<id>"]\nenabled = true|false` in Codex `config.toml`. */
export function setCodexConfigPluginEnabled(toml: string, id: string, enabled: boolean): string {
  const lines = toml.length > 0 ? toml.split(/\r?\n/u) : [];
  const targetHeader = `[plugins.${JSON.stringify(id)}]`;
  let sectionStart = -1;
  let sectionEnd = lines.length;

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i]!.trim();
    if (!trimmed.startsWith("[")) continue;
    if (sectionStart !== -1) {
      sectionEnd = i;
      break;
    }
    const header = /^\[([^\]]+)\]/u.exec(trimmed);
    if (!header) continue;
    const section = header[1]!.trim();
    const quoted = /^plugins\."([^"]+)"$/u.exec(section);
    const bare = /^plugins\.([A-Za-z0-9._-]+)$/u.exec(section);
    const matched = quoted?.[1] ?? bare?.[1] ?? null;
    if (matched === id) {
      sectionStart = i;
    }
  }

  if (sectionStart !== -1) {
    for (let i = sectionStart + 1; i < sectionEnd; i++) {
      if (/^\s*enabled\s*=\s*(?:true|false)\b/u.test(lines[i]!)) {
        lines[i] = `enabled = ${enabled ? "true" : "false"}`;
        return `${lines.join("\n").trimEnd()}\n`;
      }
    }
    lines.splice(sectionStart + 1, 0, `enabled = ${enabled ? "true" : "false"}`);
    return `${lines.join("\n").trimEnd()}\n`;
  }

  const trimmed = toml.trimEnd();
  const block = `${targetHeader}\nenabled = ${enabled ? "true" : "false"}\n`;
  return trimmed.length > 0 ? `${trimmed}\n\n${block}` : block;
}

const splitCodexId = (id: string): { readonly name: string; readonly marketplace: string } => {
  const at = id.lastIndexOf("@");
  return at > 0
    ? { name: id.slice(0, at), marketplace: id.slice(at + 1) || "codex" }
    : { name: id, marketplace: "codex" };
};

export const listCodexPlugins = Effect.fn("CodexPlugins.list")(function* (
  codexHome: string,
  query: string | undefined,
): Effect.fn.Return<PluginListResult, PluginOperationError> {
  const plugins = yield* Effect.promise(async () => {
    const configToml = (await readText(NodePath.join(codexHome, "config.toml"))) ?? "";
    const configured = parseCodexConfigPlugins(configToml);
    const installedById = new Map<string, Plugin>();

    // 1. Installed plugins in `<codexHome>/plugins/cache/<marketplace>/<name>`
    const cacheRoot = NodePath.join(codexHome, "plugins", "cache");
    for (const marketplace of await listSubdirs(cacheRoot)) {
      for (const name of await listSubdirs(NodePath.join(cacheRoot, marketplace))) {
        const id = `${name}@${marketplace}`;
        if (!isValidPluginId(id)) continue;
        const manifest = await readManifest(NodePath.join(cacheRoot, marketplace, name));
        installedById.set(id, {
          id,
          name,
          provider: "codex",
          marketplace,
          version: manifest.version,
          description: manifest.description,
          installed: true,
          enabled: configured.get(id) ?? true,
        });
      }
    }

    // 2. Installed plugins directly under `<codexHome>/plugins/<name>` (excluding `cache`)
    const pluginsRoot = NodePath.join(codexHome, "plugins");
    for (const name of await listSubdirs(pluginsRoot)) {
      if (name === "cache") continue;
      const id = `${name}@codex`;
      if (!isValidPluginId(id) || installedById.has(id)) continue;
      const manifest = await readManifest(NodePath.join(pluginsRoot, name));
      installedById.set(id, {
        id,
        name,
        provider: "codex",
        marketplace: "codex",
        version: manifest.version,
        description: manifest.description,
        installed: true,
        enabled: configured.get(id) ?? configured.get(name) ?? true,
      });
    }

    // 3. Plugins explicitly configured in `config.toml` that aren't in the cache dirs
    for (const [rawId, enabled] of configured.entries()) {
      const normalizedId = rawId.includes("@") ? rawId : `${rawId}@codex`;
      if (!isValidPluginId(normalizedId) || installedById.has(normalizedId)) continue;
      const { name, marketplace } = splitCodexId(normalizedId);
      installedById.set(normalizedId, {
        id: normalizedId,
        name,
        provider: "codex",
        marketplace,
        version: null,
        description: null,
        installed: true,
        enabled,
      });
    }

    // 4. Available offers in `<codexHome>/.tmp/bundled-marketplaces/<marketplace>/plugins/<name>`
    const offers: Plugin[] = [];
    const bundledRoot = NodePath.join(codexHome, ".tmp", "bundled-marketplaces");
    for (const marketplace of await listSubdirs(bundledRoot)) {
      const marketplacePluginsDir = NodePath.join(bundledRoot, marketplace, "plugins");
      for (const name of await listSubdirs(marketplacePluginsDir)) {
        const id = `${name}@${marketplace}`;
        if (!isValidPluginId(id) || installedById.has(id)) continue;
        const manifest = await readManifest(NodePath.join(marketplacePluginsDir, name));
        offers.push({
          id,
          name,
          provider: "codex",
          marketplace,
          version: manifest.version,
          description: manifest.description,
          installed: false,
          enabled: false,
        });
      }
    }

    const needle = query?.trim().toLowerCase() ?? "";
    const matches = (plugin: Plugin) =>
      needle.length === 0 ||
      `${plugin.id} ${plugin.provider ?? ""} ${plugin.description ?? ""}`
        .toLowerCase()
        .includes(needle);

    const installed = [...installedById.values()]
      .sort((a, b) => a.id.localeCompare(b.id))
      .filter(matches);
    const offered = offers.sort((a, b) => a.id.localeCompare(b.id)).filter(matches);
    return {
      plugins: [...installed, ...offered.slice(0, PLUGIN_OFFER_LIMIT)],
      more: Math.max(0, offered.length - PLUGIN_OFFER_LIMIT),
    };
  });
  return plugins;
});

export const setCodexPluginEnabled = Effect.fn("CodexPlugins.setEnabled")(function* (
  codexHome: string,
  id: PluginId,
  enabled: boolean,
): Effect.fn.Return<{ readonly plugin: Plugin }, PluginOperationError> {
  const configPath = NodePath.join(codexHome, "config.toml");
  const current = yield* Effect.promise(() => readText(configPath));
  const updated = setCodexConfigPluginEnabled(current ?? "", id, enabled);
  yield* Effect.tryPromise({
    try: async () => {
      await NodeFSP.mkdir(codexHome, { recursive: true });
      await NodeFSP.writeFile(configPath, updated, "utf8");
    },
    catch: (cause) =>
      failed(cause instanceof Error ? cause.message : "could not update config.toml"),
  });
  const listing = yield* listCodexPlugins(codexHome, id.split("@")[0]);
  const plugin = listing.plugins.find((entry) => entry.id === id);
  if (!plugin) return yield* failed(`${id} is not in the Codex plugin list after update`);
  return { plugin };
});

export const installCodexPlugin = Effect.fn("CodexPlugins.install")(function* (
  codexHome: string,
  id: PluginId,
): Effect.fn.Return<{ readonly plugin: Plugin }, PluginOperationError> {
  const { name, marketplace } = splitCodexId(id);
  const bundledSource = NodePath.join(
    codexHome,
    ".tmp",
    "bundled-marketplaces",
    marketplace,
    "plugins",
    name,
  );
  const targetDir = NodePath.join(codexHome, "plugins", "cache", marketplace, name);
  yield* Effect.tryPromise({
    try: async () => {
      const hasBundled = await NodeFSP.stat(bundledSource).then(
        (stat) => stat.isDirectory(),
        () => false,
      );
      if (hasBundled) {
        await NodeFSP.mkdir(NodePath.dirname(targetDir), { recursive: true });
        await NodeFSP.cp(bundledSource, targetDir, { recursive: true });
      }
    },
    catch: (cause) =>
      failed(cause instanceof Error ? cause.message : "could not install Codex plugin"),
  });
  return yield* setCodexPluginEnabled(codexHome, id, true);
});
