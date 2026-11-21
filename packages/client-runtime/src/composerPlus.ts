/**
 * The composer's `+` menu, one list for every surface (web, mobile). Same entries as hanzo.app's
 * composer, each tied to something Hanzo Dev does: a surface draws `plusEntries` and carries out
 * each entry's `effect`; one that cannot carry an effect out names it in `unsupported`, so the
 * entry is left out of the menu instead of standing there doing nothing.
 */
import { mFallback, msg } from "gt-i18n";
import type { MFunctionType } from "gt-i18n/types";

export type PlusEntryId =
  | "upload"
  | "library"
  | "screenshot"
  | "createImage"
  | "browserTabs"
  | "webSearch"
  | "research"
  | "sketch"
  | "thinking"
  | "agents"
  | "skills"
  | "connectors"
  | "design"
  | "plugins"
  | "devices"
  | "memory";

/** What choosing an entry does. */
export type PlusEffect =
  /** The device's file picker; what is picked is attached. */
  | { readonly kind: "attach" }
  /** The screen, captured and attached as an image. */
  | { readonly kind: "capture" }
  /** A drawing pad; the drawing is attached as an image. */
  | { readonly kind: "sketch" }
  /** Starts the draft with `text` and focuses the composer. */
  | { readonly kind: "insert"; readonly text: string }
  /** Opens the composer's `@` list: files, agents, threads, models and machines. */
  | { readonly kind: "mention" }
  /** Opens the composer's `$` list of skills. */
  | { readonly kind: "skills" }
  /** Shows the browser beside the conversation. */
  | { readonly kind: "browser" }
  /** A list to read and choose from: the plugins, and the cloud's connectors, design systems and memories. */
  | { readonly kind: "shelf"; readonly shelf: "plugins" | "connectors" | "design" | "memory" };

export type PlusGroup = "make" | "bring" | "keep";

export interface PlusEntry {
  readonly id: PlusEntryId;
  readonly group: PlusGroup;
  readonly label: string;
  readonly note: string;
  readonly effect: PlusEffect;
  /** The key that does it from the keyboard, where there is one. */
  readonly shortcut?: string;
}

interface Source {
  readonly id: PlusEntryId;
  readonly group: PlusGroup;
  readonly label: string;
  readonly note: string;
  readonly effect: PlusEffect;
  readonly shortcut?: string;
}

// Each phrase is a literal `msg`, so extraction finds it; `resolve` turns it into the reader's language.
const SOURCES: ReadonlyArray<Source> = [
  {
    id: "upload",
    group: "make",
    label: msg("Add photos & files"),
    note: msg("Upload from computer"),
    effect: { kind: "attach" },
    shortcut: "Ctrl+U",
  },
  {
    id: "library",
    group: "make",
    label: msg("Add from library"),
    note: msg("Browse and search your files"),
    effect: { kind: "mention" },
  },
  {
    id: "screenshot",
    group: "make",
    label: msg("Take a screenshot"),
    note: msg("Capture your screen"),
    effect: { kind: "capture" },
  },
  {
    id: "createImage",
    group: "make",
    label: msg("Create image"),
    note: msg("Visualize anything"),
    effect: { kind: "insert", text: msg("Create an image of ") },
  },
  {
    id: "browserTabs",
    group: "make",
    label: msg("Add browser tabs"),
    note: msg("Ask about your open tabs"),
    effect: { kind: "browser" },
  },
  {
    id: "webSearch",
    group: "make",
    label: msg("Web search"),
    note: msg("Find real-time news and info"),
    effect: { kind: "insert", text: msg("Search the web for ") },
  },
  {
    id: "research",
    group: "make",
    label: msg("Deep research"),
    note: msg("Get a detailed report"),
    effect: { kind: "insert", text: msg("Run deep research on ") },
  },
  {
    id: "sketch",
    group: "make",
    label: msg("Sketch"),
    note: msg("Draw and attach an image"),
    effect: { kind: "sketch" },
  },
  {
    id: "thinking",
    group: "make",
    label: msg("Thinking"),
    note: msg("For detailed answers"),
    effect: { kind: "insert", text: "ultrathink " },
  },
  {
    id: "agents",
    group: "bring",
    label: msg("Agents"),
    note: msg("Bring specialized agents into this chat"),
    effect: { kind: "mention" },
    shortcut: "@",
  },
  {
    id: "skills",
    group: "bring",
    label: msg("Skills"),
    note: msg("Use workspace skills and workflows"),
    effect: { kind: "skills" },
    shortcut: "$",
  },
  {
    id: "connectors",
    group: "bring",
    label: msg("Connectors"),
    note: msg("Sync connected knowledge sources"),
    effect: { kind: "shelf", shelf: "connectors" },
  },
  {
    id: "design",
    group: "bring",
    label: msg("Design system"),
    note: msg("Apply brand tokens and UI components"),
    effect: { kind: "shelf", shelf: "design" },
  },
  {
    id: "plugins",
    group: "bring",
    label: msg("Plugins"),
    note: msg("Connect external tools and APIs"),
    effect: { kind: "shelf", shelf: "plugins" },
    shortcut: "/plugins",
  },
  {
    id: "devices",
    group: "bring",
    label: msg("Devices"),
    note: msg("Run on a connected machine"),
    effect: { kind: "mention" },
  },
  {
    id: "memory",
    group: "keep",
    label: msg("Memory"),
    note: msg("Remember context across chats"),
    effect: { kind: "shelf", shelf: "memory" },
  },
];

/** The entries a surface offers, in menu order, worded by `resolve`. */
export function plusEntries(
  resolve: MFunctionType = mFallback,
  unsupported: ReadonlySet<PlusEntryId> = new Set(),
): ReadonlyArray<PlusEntry> {
  return SOURCES.filter((entry) => !unsupported.has(entry.id)).map((entry) => ({
    id: entry.id,
    group: entry.group,
    label: resolve(entry.label),
    note: resolve(entry.note),
    // An insert is the person's own words to finish, so in their language.
    effect:
      entry.effect.kind === "insert" && entry.id !== "thinking"
        ? { kind: "insert", text: resolve(entry.effect.text) }
        : entry.effect,
    ...(entry.shortcut === undefined ? {} : { shortcut: entry.shortcut }),
  }));
}

/** Every entry's id, in menu order: hanzo.app's `+` has all of them. */
export const PLUS_ENTRY_IDS: ReadonlyArray<PlusEntryId> = SOURCES.map((entry) => entry.id);
