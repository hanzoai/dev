import type { AppSymbolName } from "../../../components/ui/AppSymbol";
import { enabledNavigationTabs, type NavigationTab } from "./navigation-tab-order";

/**
 * hanzo.app's pinned nav (@hanzo/app Rail `DEFAULT_NAV`): Chat, Dev, Design, Library, Tabs.
 * The drawer lists exactly these, in this order, then Recents, then the account.
 */
export const WORKSPACE_SECTIONS = ["chat", "dev", "design", "library", "tabs"] as const;
export type WorkspaceSection = (typeof WORKSPACE_SECTIONS)[number];

/** Chat is where the app opens. */
export const DEFAULT_SECTION: WorkspaceSection = "chat";

/** SF symbols for hanzo.app's lucide marks (@hanzo/build MARKS): MessageSquare, CodeXml, WandSparkles, Library, SquareTerminal. */
export const SECTION_ICONS = {
  chat: "bubble.left",
  dev: "chevron.left.forwardslash.chevron.right",
  design: "wand.and.stars",
  library: "books.vertical",
  tabs: "terminal",
} as const satisfies Readonly<Record<WorkspaceSection, AppSymbolName>>;

/** Design, Library and Tabs have no native screen: hanzo.app's route is drawn in a web view. */
export const WEB_SECTIONS = ["design", "library", "tabs"] as const;
export type WebSection = (typeof WEB_SECTIONS)[number];

export function isWebSection(section: WorkspaceSection): section is WebSection {
  return WEB_SECTIONS.some((candidate) => candidate === section);
}

/**
 * What Dev holds: every list the app had before sections. Threads and Pulls/Issues are the
 * workspace tabs; Macs are the linked machines from discovery.
 */
export type DevContent = NavigationTab | "macs";

/** The Dev section's own switcher, in order: the user's tab order, Macs after the lists. */
export function devContents(
  order: readonly unknown[] | undefined,
  hidden: readonly unknown[] | undefined,
): readonly DevContent[] {
  return [...enabledNavigationTabs(order, hidden), "macs"];
}

/**
 * The section a workspace tab belongs to. Pull requests, issues and usage are Dev's. "threads"
 * is the tab Chat, Dev's thread list and the web sections all ride on, so it keeps the section
 * that was asked for.
 */
export function sectionOfTab(tab: NavigationTab, requested: WorkspaceSection): WorkspaceSection {
  return tab === "threads" ? requested : "dev";
}

export interface SectionState {
  readonly section: WorkspaceSection;
  /** Whether Dev shows the linked Macs instead of a workspace list. */
  readonly macs: boolean;
}

export const INITIAL_SECTION_STATE: SectionState = { section: DEFAULT_SECTION, macs: false };

export function sectionReducer(
  state: SectionState,
  action:
    | { readonly type: "open"; readonly section: WorkspaceSection }
    | { readonly type: "macs"; readonly macs: boolean },
): SectionState {
  const next =
    action.type === "open"
      ? { section: action.section, macs: action.section === "dev" ? state.macs : false }
      : { section: state.section, macs: action.macs };
  return next.section === state.section && next.macs === state.macs ? state : next;
}
