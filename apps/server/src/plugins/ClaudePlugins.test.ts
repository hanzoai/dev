import { expect, it } from "@effect/vitest";
import { PluginId, PluginOperationError } from "@hanzodev/contracts";
import * as Effect from "effect/Effect";
import * as Exit from "effect/Exit";

import {
  installPlugin,
  listPlugins,
  parsePluginListing,
  PLUGIN_OFFER_LIMIT,
  setPluginEnabled,
  type RunPluginCommand,
} from "./ClaudePlugins.ts";

const listing = (enabled: boolean, installed = true) =>
  JSON.stringify({
    installed: installed
      ? [{ id: "frontend-design@official", version: "1.1.1", scope: "user", enabled }]
      : [],
    available: [
      {
        pluginId: "frontend-design@official",
        name: "frontend-design",
        marketplaceName: "official",
      },
      {
        pluginId: "hanzo-essentials@hanzo",
        name: "hanzo-essentials",
        marketplaceName: "hanzo",
        description: "Daily dev tools",
        version: "2.0.0",
      },
      {
        pluginId: "lux-skills@lux",
        name: "lux-skills",
        marketplaceName: "lux",
        description: "Lux chain skills",
      },
    ],
  });

/** A stand-in for `claude plugin ...` that remembers what it was asked. */
const fakeClaude =
  (state: { enabled: boolean; installed: boolean }, calls: string[][]): RunPluginCommand =>
  (args) =>
    Effect.sync(() => {
      calls.push([...args]);
      const [, action, id] = args;
      if (action === "list")
        return { stdout: listing(state.enabled, state.installed), stderr: "", code: 0 };
      if (action === "install") state.installed = true;
      if (action === "enable") state.enabled = true;
      if (action === "disable") state.enabled = false;
      return { stdout: `done ${id}`, stderr: "", code: 0 };
    });

it("lists installed plugins first, then offers, without repeating an installed one", () => {
  const result = parsePluginListing(listing(true), undefined);
  expect(result.plugins.map((plugin) => [plugin.id, plugin.installed, plugin.enabled])).toEqual([
    ["frontend-design@official", true, true],
    ["hanzo-essentials@hanzo", false, false],
    ["lux-skills@lux", false, false],
  ]);
  expect(result.plugins[1]).toMatchObject({
    name: "hanzo-essentials",
    marketplace: "hanzo",
    version: "2.0.0",
    description: "Daily dev tools",
  });
  expect(result.more).toBe(0);
});

it("narrows by name or description", () => {
  expect(parsePluginListing(listing(true), "chain").plugins.map((plugin) => plugin.id)).toEqual([
    "lux-skills@lux",
  ]);
});

it("caps the offers and counts the rest", () => {
  const available = Array.from({ length: PLUGIN_OFFER_LIMIT + 5 }, (_, i) => ({
    pluginId: `p${i}@m`,
    name: `p${i}`,
    marketplaceName: "m",
  }));
  const result = parsePluginListing(JSON.stringify({ installed: [], available }), undefined);
  expect(result.plugins).toHaveLength(PLUGIN_OFFER_LIMIT);
  expect(result.more).toBe(5);
});

it.effect("installs, turns on and turns off through the CLI", () =>
  Effect.gen(function* () {
    const calls: string[][] = [];
    const state = { enabled: false, installed: false };
    const run = fakeClaude(state, calls);
    const id = PluginId.make("frontend-design@official");

    const installed = yield* installPlugin(run, id);
    expect(calls[0]).toEqual(["plugin", "install", id]);
    expect(installed.plugin).toMatchObject({ installed: true, enabled: false });

    const on = yield* setPluginEnabled(run, id, true);
    expect(calls.some((call) => call[1] === "enable" && call[2] === id)).toBe(true);
    expect(on.plugin.enabled).toBe(true);

    const off = yield* setPluginEnabled(run, id, false);
    expect(off.plugin.enabled).toBe(false);
  }),
);

it.effect("reports what the CLI said when it fails", () =>
  Effect.gen(function* () {
    const run: RunPluginCommand = () =>
      Effect.succeed({ stdout: "", stderr: "Plugin not found in any marketplace", code: 1 });
    const exit = yield* Effect.exit(installPlugin(run, PluginId.make("nope@nowhere")));
    expect(Exit.isFailure(exit)).toBe(true);
    const error = yield* installPlugin(run, PluginId.make("nope@nowhere")).pipe(Effect.flip);
    expect(error).toBeInstanceOf(PluginOperationError);
    expect(error.message).toContain("not found");
  }),
);

it.effect("reads a missing claude as cli_missing", () =>
  Effect.gen(function* () {
    const run: RunPluginCommand = () =>
      Effect.fail(
        new PluginOperationError({
          reason: "cli_missing",
          message: "Claude Code is not installed on this machine.",
        }),
      );
    const error = yield* listPlugins(run, undefined).pipe(Effect.flip);
    expect(error.reason).toBe("cli_missing");
  }),
);
