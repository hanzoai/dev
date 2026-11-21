// THE one keymap, for Dev and for any host that draws it. The table is Dev's
// (`@hanzodev/shared/keybindings` DEFAULT_KEYBINDINGS, with `when` clauses), extended with
// the host's window commands (sections, focus jumps); the resolver is Dev's; a person's
// rebindings are one list, kept in this browser. A host resolves its window-level keys here
// and lets Dev's own scopes keep the commands that act inside a thread.
import type { KeybindingCommand, KeybindingRule } from "@hanzodev/contracts";
import {
  compileResolvedKeybindingsConfig,
  DEFAULT_RESOLVED_KEYBINDINGS,
  mergeWithDefaultKeybindings,
} from "@hanzodev/shared/keybindings";

import {
  resolveShortcutCommand,
  shortcutLabelForCommand,
  type ShortcutEventLike,
  type ShortcutMatchContext,
} from "../keybindings";

const STORAGE_KEY = "hanzo.keymap";

function readOverrides(): ReadonlyArray<KeybindingRule> {
  try {
    const raw = window.localStorage.getItem(STORAGE_KEY);
    return raw ? (JSON.parse(raw) as ReadonlyArray<KeybindingRule>) : [];
  } catch {
    return [];
  }
}

let bindings = mergeWithDefaultKeybindings(compileResolvedKeybindingsConfig(readOverrides()));

/** Rebind: the rules replace the defaults of the commands they name. An empty list restores the defaults. */
export function setKeymapOverrides(rules: ReadonlyArray<KeybindingRule>): void {
  try {
    if (rules.length === 0) window.localStorage.removeItem(STORAGE_KEY);
    else window.localStorage.setItem(STORAGE_KEY, JSON.stringify(rules));
  } catch {
    // Not kept this session; the rebinding still applies until the page closes.
  }
  bindings = rules.length
    ? mergeWithDefaultKeybindings(compileResolvedKeybindingsConfig(rules))
    : [...DEFAULT_RESOLVED_KEYBINDINGS];
}

/** The command a key press asks for, or null. */
export function resolveCommand(
  event: ShortcutEventLike,
  context: Partial<ShortcutMatchContext> = {},
): KeybindingCommand | null {
  return resolveShortcutCommand(event, bindings, { context });
}

/** The shortcut a command has now, as a person reads it (⌘K on a Mac, Ctrl+K elsewhere). */
export function shortcutFor(command: KeybindingCommand): string | null {
  return shortcutLabelForCommand(bindings, command);
}

export interface KeymapRow {
  readonly group: string;
  readonly command: KeybindingCommand;
  readonly label: string;
}

/** What the shortcuts overlay lists: every command a person reaches by key, grouped. */
export const KEYMAP_ROWS: ReadonlyArray<KeymapRow> = [
  { group: "Go", command: "commandPalette.toggle", label: "Command palette" },
  { group: "Go", command: "chat.new", label: "New chat or thread" },
  { group: "Go", command: "sidebar.toggle", label: "Show or hide the sidebar" },
  ...Array.from({ length: 9 }, (_, index) => ({
    group: "Go",
    command: `section.${index + 1}` as KeybindingCommand,
    label: `Section ${index + 1}`,
  })),
  { group: "Go", command: "navigation.back", label: "Back" },
  { group: "Go", command: "navigation.forward", label: "Forward" },
  { group: "Go", command: "settings.open", label: "Settings" },
  { group: "Go", command: "keybindings.open", label: "All shortcuts" },
  { group: "Focus", command: "composer.focus", label: "Composer" },
  { group: "Focus", command: "sidebar.focus", label: "Thread list" },
  { group: "Focus", command: "workspace.focusNext", label: "Next panel" },
  { group: "Focus", command: "workspace.focusPrevious", label: "Previous panel" },
  { group: "Thread", command: "thread.previous", label: "Previous thread" },
  { group: "Thread", command: "thread.next", label: "Next thread" },
  { group: "Thread", command: "thread.settle", label: "Mark done" },
  { group: "Composer", command: "modelPicker.toggle", label: "Model" },
  { group: "Composer", command: "composer.effort", label: "Effort" },
  { group: "Composer", command: "composer.mode", label: "Mode" },
  { group: "Composer", command: "composer.dictation", label: "Dictation" },
  { group: "Panels", command: "terminal.toggle", label: "Terminal" },
  { group: "Panels", command: "rightPanel.toggle", label: "Right panel" },
  { group: "Panels", command: "preview.toggle", label: "Browser" },
  { group: "Panels", command: "diff.toggle", label: "Diff" },
  { group: "Panels", command: "filePicker.toggle", label: "Open file" },
];
