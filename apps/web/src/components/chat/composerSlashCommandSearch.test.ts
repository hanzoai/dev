import { workspaceSlashCommands } from "@hanzodev/client-runtime/composer-commands";
import type { ServerProviderSkill } from "@hanzodev/contracts";
import { describe, expect, it } from "vite-plus/test";

import type { ComposerCommandItem } from "./ComposerCommandMenu";
import {
  searchSlashCommandItems,
  slashCommandItemsForPromptPosition,
} from "./composerSlashCommandSearch";

type Slash = Extract<ComposerCommandItem, { type: "slash-command" }>;

const builtIns: Slash[] = [
  {
    id: "slash:model",
    type: "slash-command",
    command: "model",
    label: "/model",
    description: "Switch response model for this thread",
  },
  ...workspaceSlashCommands().map(({ command, description }): Slash => ({
    id: `slash:${command}`,
    type: "slash-command",
    command,
    label: `/${command}`,
    description,
  })),
];

const skill = (name: string): Extract<ComposerCommandItem, { type: "skill" }> => ({
  id: `skill:claudeAgent:${name}`,
  type: "skill",
  provider: "claudeAgent" as never,
  skill: { name, path: `/skills/${name}`, enabled: true } as ServerProviderSkill,
  label: `/skill:${name}`,
  description: "",
});

describe("/skills and /plugins", () => {
  it("are found by their names", () => {
    expect(searchSlashCommandItems(builtIns, "skills").map((item) => item.id)[0]).toBe(
      "slash:skills",
    );
    expect(searchSlashCommandItems(builtIns, "plug").map((item) => item.id)[0]).toBe(
      "slash:plugins",
    );
  });

  it("rank first over a skill named like them", () => {
    const items = searchSlashCommandItems(
      [...builtIns, skill("skills-audit"), skill("plugin-dev")],
      "skills",
    );
    expect(items[0]?.id).toBe("slash:skills");
    const plugins = searchSlashCommandItems([...builtIns, skill("plugin-dev")], "plugins");
    expect(plugins[0]?.id).toBe("slash:plugins");
  });

  it("work anywhere in a message, not only at its start", () => {
    const later = slashCommandItemsForPromptPosition(builtIns, false).map((item) => item.id);
    expect(later).toContain("slash:skills");
    expect(later).toContain("slash:plugins");
  });
});
