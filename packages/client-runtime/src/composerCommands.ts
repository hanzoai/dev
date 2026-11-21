/**
 * The slash commands every composer shares beside the provider's own: `/skills` picks a skill
 * and `/plugins` manages plugins. A surface draws them in its `/` menu and carries out `command`.
 */
import type { Plugin } from "@hanzodev/contracts";
import { mFallback, msg } from "gt-i18n";
import type { MFunctionType } from "gt-i18n/types";

export type WorkspaceSlashCommand = "skills" | "plugins";

const COMMANDS: ReadonlyArray<{
  readonly command: WorkspaceSlashCommand;
  readonly description: string;
}> = [
  { command: "skills", description: msg("List skills and pick one to use") },
  { command: "plugins", description: msg("List, install and turn plugins on or off") },
];

/** The commands, worded by `resolve`. */
export function workspaceSlashCommands(
  resolve: MFunctionType = mFallback,
): ReadonlyArray<{ readonly command: WorkspaceSlashCommand; readonly description: string }> {
  return COMMANDS.map(({ command, description }) => ({
    command,
    description: resolve(description),
  }));
}

/** What the one control on a plugin's row does next. */
export function pluginAction(
  plugin: Pick<Plugin, "installed" | "enabled">,
): "install" | "enable" | "disable" {
  if (!plugin.installed) return "install";
  return plugin.enabled ? "disable" : "enable";
}

/** The list as the person reads it: what is installed (on first), then what marketplaces offer. */
export function pluginSections(plugins: ReadonlyArray<Plugin>): {
  readonly installed: ReadonlyArray<Plugin>;
  readonly offered: ReadonlyArray<Plugin>;
} {
  const byName = (a: Plugin, b: Plugin) => a.name.localeCompare(b.name);
  const installed = plugins.filter((plugin) => plugin.installed);
  return {
    installed: [
      ...installed.filter((plugin) => plugin.enabled).sort(byName),
      ...installed.filter((plugin) => !plugin.enabled).sort(byName),
    ],
    offered: plugins.filter((plugin) => !plugin.installed),
  };
}
