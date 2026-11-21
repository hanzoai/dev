/**
 * The plugin registry: Claude Code's own. `claude plugin list --json --available` names what is
 * installed and what the marketplaces offer; `install`, `enable` and `disable` change it. Hanzo
 * keeps no list of its own, so a plugin installed here is the one the CLI runs with.
 */
import {
  PluginOperationError,
  type Plugin,
  type PluginId,
  type PluginListResult,
} from "@hanzodev/contracts";
import {
  isCommandMissingCause,
  spawnAndCollect,
} from "@hanzodev/provider-core/server/snapshotProbe";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import { resolveSpawnCommand } from "@hanzodev/shared/shell";
import * as Effect from "effect/Effect";
import { ChildProcess } from "effect/process";

/** Offers listed at most; the rest are reached by narrowing the query. */
export const PLUGIN_OFFER_LIMIT = 40;

const CLAUDE_BINARY = "claude";

export interface PluginCommandResult {
  readonly stdout: string;
  readonly stderr: string;
  readonly code: number;
}

/** Runs `claude plugin <args>`; fails with `cli_missing` when there is no `claude`. */
export type RunPluginCommand = (
  args: ReadonlyArray<string>,
) => Effect.Effect<PluginCommandResult, PluginOperationError>;

const record = (value: unknown): Record<string, unknown> | null =>
  typeof value === "object" && value !== null && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : null;

const text = (value: unknown): string | null =>
  typeof value === "string" && value.trim().length > 0 ? value.trim() : null;

const splitId = (id: string): { readonly name: string; readonly marketplace: string | null } => {
  const at = id.lastIndexOf("@");
  return at > 0
    ? { name: id.slice(0, at), marketplace: id.slice(at + 1) || null }
    : { name: id, marketplace: null };
};

/** What `claude plugin list --json --available` printed, as plugins: installed first, then offers. */
export function parsePluginListing(stdout: string, query: string | undefined): PluginListResult {
  let parsed: unknown;
  try {
    parsed = JSON.parse(stdout);
  } catch {
    throw new SyntaxError("claude plugin list did not print JSON");
  }
  const root = record(parsed);
  const needle = query?.trim().toLowerCase() ?? "";
  const matches = (plugin: Plugin) =>
    needle.length === 0 ||
    `${plugin.id} ${plugin.description ?? ""}`.toLowerCase().includes(needle);

  const installed: Plugin[] = [];
  for (const row of Array.isArray(root?.installed) ? root.installed : []) {
    const item = record(row);
    const id = text(item?.id);
    if (!item || !id) continue;
    const { name, marketplace } = splitId(id);
    installed.push({
      id,
      name,
      marketplace,
      version: text(item.version),
      description: null,
      installed: true,
      enabled: item.enabled === true,
    });
  }
  const have = new Set(installed.map((plugin) => plugin.id));
  const offers: Plugin[] = [];
  for (const row of Array.isArray(root?.available) ? root.available : []) {
    const item = record(row);
    const id = text(item?.pluginId);
    const name = text(item?.name);
    if (!item || !id || !name || have.has(id)) continue;
    offers.push({
      id,
      name,
      marketplace: text(item.marketplaceName),
      version: text(item.version),
      description: text(item.description),
      installed: false,
      enabled: false,
    });
  }
  const offered = offers.filter(matches);
  return {
    plugins: [...installed.filter(matches), ...offered.slice(0, PLUGIN_OFFER_LIMIT)],
    more: Math.max(0, offered.length - PLUGIN_OFFER_LIMIT),
  };
}

const failed = (message: string) => new PluginOperationError({ reason: "failed", message });

const run = Effect.fn("ClaudePlugins.run")(function* (
  command: RunPluginCommand,
  args: ReadonlyArray<string>,
) {
  const result = yield* command(args);
  if (result.code !== 0) {
    const said = (result.stderr || result.stdout).trim();
    return yield* failed(said.length > 0 ? said.slice(0, 500) : `claude exited ${result.code}`);
  }
  return result;
});

export const listPlugins = Effect.fn("ClaudePlugins.list")(function* (
  command: RunPluginCommand,
  query: string | undefined,
) {
  const result = yield* run(command, ["plugin", "list", "--json", "--available"]);
  return yield* Effect.try({
    try: () => parsePluginListing(result.stdout, query),
    catch: (cause) => failed(cause instanceof Error ? cause.message : "unreadable plugin list"),
  });
});

/** Runs `action` on plugin `id`, then reads the plugin as the CLI now has it. */
const change = Effect.fn("ClaudePlugins.change")(function* (
  command: RunPluginCommand,
  action: "install" | "enable" | "disable",
  id: PluginId,
) {
  yield* run(command, ["plugin", action, id]);
  const listing = yield* listPlugins(command, id.split("@")[0]);
  const plugin = listing.plugins.find((candidate) => candidate.id === id);
  if (!plugin) return yield* failed(`${id} is not in the plugin list after ${action}`);
  return { plugin };
});

export const installPlugin = (command: RunPluginCommand, id: PluginId) =>
  change(command, "install", id);

export const setPluginEnabled = (command: RunPluginCommand, id: PluginId, enabled: boolean) =>
  change(command, enabled ? "enable" : "disable", id);

const spawnClaude = Effect.fn("ClaudePlugins.spawn")(function* (args: ReadonlyArray<string>) {
  const env = yield* HostProcess.Environment;
  const spawn = yield* resolveSpawnCommand(CLAUDE_BINARY, args, { env });
  return yield* spawnAndCollect(
    CLAUDE_BINARY,
    ChildProcess.make(spawn.command, spawn.args, { env, shell: spawn.shell }),
    { maxStdoutBytes: 16 * 1024 * 1024 },
  );
});

/** The `claude` on this machine's PATH, run with the server's environment. */
export const runClaudePluginCommand: RunPluginCommand = (args) =>
  spawnClaude(args).pipe(
    Effect.mapError(
      (cause) =>
        new PluginOperationError({
          reason: isCommandMissingCause(cause) ? "cli_missing" : "failed",
          message: isCommandMissingCause(cause)
            ? "Claude Code is not installed on this machine."
            : cause instanceof Error && cause.message
              ? cause.message
              : "claude could not be run",
        }),
    ),
  );
