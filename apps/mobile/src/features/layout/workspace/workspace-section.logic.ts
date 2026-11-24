import type { AppSymbolName } from "../../../components/ui/AppSymbol";
import { enabledNavigationTabs, type NavigationTab } from "./navigation-tab-order";

/**
 * hanzo.app's pinned nav (@hanzo/app Rail `DEFAULT_NAV`): Chat, Dev, Library, Design, Plugins.
 * The drawer lists exactly these, in this order, then Recents, then the account.
 */
export const WORKSPACE_SECTIONS = ["chat", "dev", "library", "design", "plugins"] as const;
export type WorkspaceSection = (typeof WORKSPACE_SECTIONS)[number];

/** Chat is where the app opens. */
export const DEFAULT_SECTION: WorkspaceSection = "chat";

/** SF symbols for hanzo.app's lucide marks: MessageSquare, Code, Library, Palette, Plug. */
export const SECTION_ICONS = {
  chat: "bubble.left",
  dev: "chevron.left.forwardslash.chevron.right",
  library: "books.vertical",
  design: "wand.and.stars",
  plugins: "cable.connector",
} as const satisfies Readonly<Record<WorkspaceSection, AppSymbolName>>;

/** Library, Design and Plugins have no native screen: hanzo.app's route is drawn in a web view. */
export const WEB_SECTIONS = ["library", "design", "plugins"] as const;
export type WebSection = (typeof WEB_SECTIONS)[number];

export function isWebSection(section: WorkspaceSection): section is WebSection {
  return WEB_SECTIONS.some((candidate) => candidate === section);
}

/**
 * What Dev holds: every list the app had before sections. Threads and Pulls/Issues are the
 * workspace tabs; Projects lists the connected workspaces; Macs are the linked machines.
 */
export type DevContent = NavigationTab | "projects" | "macs";

/** The Dev section's own switcher, in order: the user's tab order, then Projects and Macs. */
export function devContents(
  order: readonly unknown[] | undefined,
  hidden: readonly unknown[] | undefined,
): readonly DevContent[] {
  return [...enabledNavigationTabs(order, hidden), "projects", "macs"];
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
  /** Whether Dev shows the projects list instead of a workspace list. */
  readonly projects: boolean;
  /** Whether Dev shows the linked Macs instead of a workspace list. */
  readonly macs: boolean;
}

export const INITIAL_SECTION_STATE: SectionState = {
  section: DEFAULT_SECTION,
  projects: false,
  macs: false,
};

export function sectionReducer(
  state: SectionState,
  action:
    | { readonly type: "open"; readonly section: WorkspaceSection }
    | { readonly type: "projects"; readonly projects: boolean }
    | { readonly type: "macs"; readonly macs: boolean },
): SectionState {
  const next =
    action.type === "open"
      ? {
          section: action.section,
          projects: action.section === "dev" ? state.projects : false,
          macs: action.section === "dev" ? state.macs : false,
        }
      : action.type === "projects"
        ? {
            section: state.section,
            projects: action.projects,
            macs: action.projects ? false : state.macs,
          }
        : {
            section: state.section,
            projects: action.macs ? false : state.projects,
            macs: action.macs,
          };
  return next.section === state.section &&
    next.projects === state.projects &&
    next.macs === state.macs
    ? state
    : next;
}
