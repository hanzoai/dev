// @effect-diagnostics nodeBuiltinImport:off
/**
 * Unified multi-provider plugin registry: aggregates Claude Code, Codex, and Antigravity
 * plugins so list, install, and enable/disable work across every supported provider installed
 * on the machine.
 */
import * as NodePath from "node:path";

import {
  PluginOperationError,
  type Plugin,
  type PluginId,
  type PluginListResult,
} from "@hanzodev/contracts";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as Effect from "effect/Effect";

import * as AntigravityPlugins from "./AntigravityPlugins.ts";
import * as ClaudePlugins from "./ClaudePlugins.ts";
import * as CodexPlugins from "./CodexPlugins.ts";

export interface PluginRegistryDeps {
  readonly runClaudeCommand: ClaudePlugins.RunPluginCommand;
  readonly codexHome: string;
  readonly geminiHome: string;
}

export const defaultPluginRegistryHomes = Effect.gen(function* () {
  const home = yield* HostProcess.HomeDirectory;
  const env = yield* HostProcess.Environment;
  const codexHome = env.CODEX_HOME?.trim() || NodePath.join(home, ".codex");
  const geminiHome = env.GEMINI_HOME?.trim() || NodePath.join(home, ".gemini");
  return { codexHome, geminiHome };
});

export const listPlugins = Effect.fn("PluginRegistry.list")(function* (
  deps: PluginRegistryDeps,
  query: string | undefined,
): Effect.fn.Return<PluginListResult, PluginOperationError> {
  const [claudeResult, codexResult, antigravityResult] = yield* Effect.all([
    ClaudePlugins.listPlugins(deps.runClaudeCommand, query).pipe(Effect.result),
    CodexPlugins.listCodexPlugins(deps.codexHome, query).pipe(Effect.result),
    AntigravityPlugins.listAntigravityPlugins(deps.geminiHome, query).pipe(Effect.result),
  ]);

  const installed: Plugin[] = [];
  const offered: Plugin[] = [];
  const seen = new Set<string>();
  let extraMore = 0;

  const collect = (list: PluginListResult) => {
    extraMore += list.more;
    for (const plugin of list.plugins) {
      if (seen.has(plugin.id)) continue;
      seen.add(plugin.id);
      if (plugin.installed) installed.push(plugin);
      else offered.push(plugin);
    }
  };

  if (claudeResult._tag === "Success") collect(claudeResult.success);
  if (codexResult._tag === "Success") collect(codexResult.success);
  if (antigravityResult._tag === "Success") collect(antigravityResult.success);

  // Only surface Claude's error if no other provider contributed any plugins.
  if (claudeResult._tag === "Failure" && installed.length === 0 && offered.length === 0) {
    return yield* Effect.fail(claudeResult.failure);
  }

  return {
    plugins: [...installed, ...offered.slice(0, ClaudePlugins.PLUGIN_OFFER_LIMIT)],
    more: extraMore + Math.max(0, offered.length - ClaudePlugins.PLUGIN_OFFER_LIMIT),
  };
});

export const setPluginEnabled = Effect.fn("PluginRegistry.setEnabled")(function* (
  deps: PluginRegistryDeps,
  id: PluginId,
  enabled: boolean,
): Effect.fn.Return<{ readonly plugin: Plugin }, PluginOperationError> {
  if (AntigravityPlugins.isAntigravityPluginId(id)) {
    return yield* AntigravityPlugins.setAntigravityPluginEnabled(deps.geminiHome, id, enabled);
  }
  const codexListing = yield* CodexPlugins.listCodexPlugins(deps.codexHome, id.split("@")[0]).pipe(
    Effect.orElseSucceed(() => ({ plugins: [], more: 0 }) satisfies PluginListResult),
  );
  if (codexListing.plugins.some((entry) => entry.id === id && entry.installed)) {
    return yield* CodexPlugins.setCodexPluginEnabled(deps.codexHome, id, enabled);
  }
  return yield* ClaudePlugins.setPluginEnabled(deps.runClaudeCommand, id, enabled);
});

export const installPlugin = Effect.fn("PluginRegistry.install")(function* (
  deps: PluginRegistryDeps,
  id: PluginId,
): Effect.fn.Return<{ readonly plugin: Plugin }, PluginOperationError> {
  if (AntigravityPlugins.isAntigravityPluginId(id)) {
    return yield* new PluginOperationError({
      reason: "failed",
      message: "Antigravity plugins are managed through the Antigravity CLI.",
    });
  }
  const codexListing = yield* CodexPlugins.listCodexPlugins(deps.codexHome, id.split("@")[0]).pipe(
    Effect.orElseSucceed(() => ({ plugins: [], more: 0 }) satisfies PluginListResult),
  );
  if (codexListing.plugins.some((entry) => entry.id === id)) {
    return yield* CodexPlugins.installCodexPlugin(deps.codexHome, id);
  }
  return yield* ClaudePlugins.installPlugin(deps.runClaudeCommand, id);
});
