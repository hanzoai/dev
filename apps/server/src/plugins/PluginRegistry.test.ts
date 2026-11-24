// @effect-diagnostics nodeBuiltinImport:off
import * as NodeFSP from "node:fs/promises";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";

import { PluginId, PluginOperationError } from "@hanzodev/contracts";
import * as Effect from "effect/Effect";
import { afterEach, describe, expect, it } from "@effect/vitest";

import type { RunPluginCommand } from "./ClaudePlugins.ts";
import { parseCodexConfigPlugins, setCodexConfigPluginEnabled } from "./CodexPlugins.ts";
import { installPlugin, listPlugins, setPluginEnabled } from "./PluginRegistry.ts";

const tempDirs: string[] = [];

async function makeTempHomes() {
  const root = await NodeFSP.mkdtemp(NodePath.join(NodeOS.tmpdir(), "hz-plugins-"));
  tempDirs.push(root);
  const codexHome = NodePath.join(root, ".codex");
  const geminiHome = NodePath.join(root, ".gemini");
  await NodeFSP.mkdir(codexHome, { recursive: true });
  await NodeFSP.mkdir(geminiHome, { recursive: true });
  return { codexHome, geminiHome };
}

afterEach(async () => {
  await Promise.all(
    tempDirs.splice(0).map((dir) => NodeFSP.rm(dir, { recursive: true, force: true })),
  );
});

const missingClaude: RunPluginCommand = () =>
  Effect.fail(
    new PluginOperationError({
      reason: "cli_missing",
      message: "Claude Code is not installed on this machine.",
    }),
  );

describe("Codex config.toml plugin helpers", () => {
  it("parses and updates [plugins.<id>] sections without clobbering other settings", () => {
    const initial = [
      'model = "gpt-5.4"',
      "",
      '[plugins."github@openai-bundled"]',
      "enabled = true",
      "",
      "[mcp_servers.demo]",
      'command = "demo"',
      "",
    ].join("\n");

    const parsed = parseCodexConfigPlugins(initial);
    expect(parsed.get("github@openai-bundled")).toBe(true);

    const disabled = setCodexConfigPluginEnabled(initial, "github@openai-bundled", false);
    expect(disabled).toContain('model = "gpt-5.4"');
    expect(disabled).toContain("[mcp_servers.demo]");
    expect(parseCodexConfigPlugins(disabled).get("github@openai-bundled")).toBe(false);

    const added = setCodexConfigPluginEnabled(disabled, "linear@openai-bundled", true);
    expect(parseCodexConfigPlugins(added).get("linear@openai-bundled")).toBe(true);
    expect(parseCodexConfigPlugins(added).get("github@openai-bundled")).toBe(false);
  });
});

describe("PluginRegistry", () => {
  it.effect(
    "lists, installs, and toggles Codex and Antigravity plugins even when Claude CLI is absent",
    () =>
      Effect.gen(function* () {
        const { codexHome, geminiHome } = yield* Effect.promise(() => makeTempHomes());

        // Seed an installed Codex plugin + a bundled Codex marketplace offer.
        const installedCodexDir = NodePath.join(
          codexHome,
          "plugins",
          "cache",
          "openai-bundled",
          "git",
        );
        yield* Effect.promise(async () => {
          await NodeFSP.mkdir(NodePath.join(installedCodexDir, ".codex-plugin"), {
            recursive: true,
          });
          await NodeFSP.writeFile(
            NodePath.join(installedCodexDir, ".codex-plugin", "plugin.json"),
            JSON.stringify({ version: "1.2.0", description: "Git helpers" }),
          );
          const bundledOfferDir = NodePath.join(
            codexHome,
            ".tmp",
            "bundled-marketplaces",
            "openai-bundled",
            "plugins",
            "slack",
          );
          await NodeFSP.mkdir(bundledOfferDir, { recursive: true });
          await NodeFSP.writeFile(
            NodePath.join(bundledOfferDir, "plugin.json"),
            JSON.stringify({ version: "0.4.0", description: "Slack connector" }),
          );

          // Seed an installed Antigravity plugin.
          const agyDir = NodePath.join(geminiHome, "config", "plugins", "chrome-devtools");
          await NodeFSP.mkdir(agyDir, { recursive: true });
          await NodeFSP.writeFile(
            NodePath.join(agyDir, "plugin.json"),
            JSON.stringify({ version: "2.0.0", description: "Chrome DevTools MCP" }),
          );
        });

        const deps = { runClaudeCommand: missingClaude, codexHome, geminiHome };

        const listed = yield* listPlugins(deps, undefined);
        expect(
          listed.plugins.map((p) => `${p.id}:${p.provider}:${p.installed}:${p.enabled}`),
        ).toEqual([
          "git@openai-bundled:codex:true:true",
          "chrome-devtools@antigravity:antigravity:true:true",
          "slack@openai-bundled:codex:false:false",
        ]);

        // Toggle Codex plugin off.
        const toggledCodex = yield* setPluginEnabled(
          deps,
          PluginId.make("git@openai-bundled"),
          false,
        );
        expect(toggledCodex.plugin.enabled).toBe(false);
        expect(toggledCodex.plugin.provider).toBe("codex");

        // Install bundled Codex offer.
        const installedOffer = yield* installPlugin(deps, PluginId.make("slack@openai-bundled"));
        expect(installedOffer.plugin.installed).toBe(true);
        expect(installedOffer.plugin.enabled).toBe(true);

        // Toggle Antigravity plugin off.
        const toggledAgy = yield* setPluginEnabled(
          deps,
          PluginId.make("chrome-devtools@antigravity"),
          false,
        );
        expect(toggledAgy.plugin.enabled).toBe(false);
        expect(toggledAgy.plugin.provider).toBe("antigravity");

        const agyConfig = JSON.parse(
          yield* Effect.promise(() =>
            NodeFSP.readFile(NodePath.join(geminiHome, "config", "config.json"), "utf8"),
          ),
        ) as { plugins?: Record<string, { enabled?: boolean }> };
        expect(agyConfig.plugins?.["chrome-devtools"]?.enabled).toBe(false);
      }),
  );

  it.effect("merges Claude, Codex, and Antigravity plugins when all registries are present", () =>
    Effect.gen(function* () {
      const { codexHome, geminiHome } = yield* Effect.promise(() => makeTempHomes());
      yield* Effect.promise(() =>
        NodeFSP.mkdir(NodePath.join(codexHome, "plugins", "demo"), { recursive: true }),
      );

      const runClaude: RunPluginCommand = () =>
        Effect.succeed({
          code: 0,
          stderr: "",
          stdout: JSON.stringify({
            installed: [{ id: "figma@claude-plugins-official", version: "1.0.0", enabled: true }],
            available: [],
          }),
        });

      const listed = yield* listPlugins(
        { runClaudeCommand: runClaude, codexHome, geminiHome },
        undefined,
      );
      expect(listed.plugins.map((p) => `${p.id}:${p.provider}`)).toEqual([
        "figma@claude-plugins-official:claude",
        "demo@codex:codex",
      ]);
    }),
  );
});
