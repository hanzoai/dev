export type KeybindingCommand = string;

export interface KeymapEvent {
  readonly key: string;
  readonly code?: string;
  readonly metaKey: boolean;
  readonly ctrlKey: boolean;
  readonly shiftKey: boolean;
  readonly altKey: boolean;
}

export interface KeymapRule {
  readonly key: string;
  readonly command: KeybindingCommand;
  readonly when?: string;
}

export interface KeymapRow {
  readonly group: string;
  readonly command: KeybindingCommand;
  readonly label: string;
}

/** The command a key press asks for, or null. `editableFocus` is true while a field has focus. */
export function resolveCommand(
  event: KeymapEvent,
  context?: { editableFocus?: boolean; terminalFocus?: boolean; previewFocus?: boolean },
): KeybindingCommand | null;

/** The shortcut a command has now, as a person reads it, or null when it is unbound. */
export function shortcutFor(command: KeybindingCommand): string | null;

/** Rebind commands; an empty list restores the defaults. Kept in this browser. */
export function setKeymapOverrides(rules: ReadonlyArray<KeymapRule>): void;

/** Every command the shortcuts overlay lists, grouped. */
export const KEYMAP_ROWS: ReadonlyArray<KeymapRow>;
