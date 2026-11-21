import {
  NAVIGATION_TAB_ORDER,
  resolveHiddenNavigationTabs,
  resolveNavigationTabOrder,
  type NavigationTab,
} from "./navigation-tab-order";

// The numbers of the hanzo.app phone drawer (@hanzo/app slide.ts), ported as is.
/** Pane left in sight at the trailing edge while the drawer is open, pt. */
export const DRAWER_STRIP = 64;
/** The drawer's widest, pt. */
export const DRAWER_REACH = 360;
/** pt a finger moves before a drag is a drag. */
export const DRAWER_SLOP = 8;
/** A flick, pt per ms: faster than this decides by its direction, whatever the distance. */
export const DRAWER_FLICK = 0.4;
/** The slide and the dim run on one clock. */
export const DRAWER_DURATION = 300;
/** Where a drag may begin, pt from the leading edge. */
export const DRAWER_EDGE = 24;

export type DrawerDestination = NavigationTab | "settings";

export function drawerReach(windowWidth: number): number {
  return Math.min(Math.max(0, windowWidth - DRAWER_STRIP), DRAWER_REACH);
}

/** Where the pane stands while a finger holds it: never past home or open. */
export function drawerPull(from: number, dx: number, reach: number): number {
  "worklet";
  return Math.min(reach, Math.max(0, from + dx));
}

/** Whether a drawer let go at `at`, moving `speed` pt/ms, stays open. */
export function drawerSettles(at: number, reach: number, speed: number): boolean {
  "worklet";
  if (speed <= -DRAWER_FLICK) return false;
  if (speed >= DRAWER_FLICK) return true;
  return at > reach / 2;
}

/** What a finger's first movement is: a pull along the line, a scroll, or nothing yet. */
export function drawerAxis(dx: number, dy: number): "x" | "y" | null {
  const x = Math.abs(dx);
  const y = Math.abs(dy);
  if (x < DRAWER_SLOP && y < DRAWER_SLOP) return null;
  return x > y * 1.2 ? "x" : "y";
}

/**
 * Every place the bottom bar reached, in its order. The shared pull-request/issue slot is a bar
 * constraint, not a drawer one; only tabs the user hid leave the list. Settings is always there.
 */
export function drawerDestinations(
  order: readonly unknown[] | undefined,
  hidden: readonly unknown[] | undefined,
  customizable: boolean,
): readonly DrawerDestination[] {
  const excluded = customizable ? resolveHiddenNavigationTabs(hidden) : [];
  return [...resolveNavigationTabOrder(order).filter((tab) => !excluded.includes(tab)), "settings"];
}

/** The destinations the bar offered: its four slots, the overflow, and Settings. */
export function barDestinations(
  order: readonly unknown[] | undefined,
  hidden: readonly unknown[] | undefined,
): readonly DrawerDestination[] {
  const hiddenTabs = resolveHiddenNavigationTabs(hidden);
  return [
    ...resolveNavigationTabOrder(order).filter((tab) => !hiddenTabs.includes(tab)),
    "settings",
  ];
}

export const ALL_DRAWER_DESTINATIONS: readonly DrawerDestination[] = [
  ...NAVIGATION_TAB_ORDER,
  "settings",
];

export interface DrawerState {
  readonly open: boolean;
}

export type DrawerAction =
  | { readonly type: "open" }
  | { readonly type: "close" }
  | { readonly type: "toggle" };

export function drawerReducer(state: DrawerState, action: DrawerAction): DrawerState {
  const open = action.type === "toggle" ? !state.open : action.type === "open";
  return open === state.open ? state : { open };
}

/** Plan or role under the account name. SuperAdmin is exactly the reserved `admin` org. */
export function drawerAccountCaption(account: {
  readonly owner: string;
  readonly planName: string | null;
}): { readonly kind: "superAdmin" | "plan"; readonly plan: string | null } {
  return account.owner === "admin"
    ? { kind: "superAdmin", plan: null }
    : { kind: "plan", plan: account.planName };
}

/** What to show for the avatar when there is no picture. */
export function drawerAccountInitial(name: string): string {
  const first = [...name.trim()][0];
  return first ? first.toUpperCase() : "?";
}

/**
 * hanzo.app's raised pane (@hanzo/app Drawer.tsx and its tokens): no scale; the start corners
 * round to --radius-composer (28), a 1px --border-strong edge, shadow `0 24px 60px -16px`.
 */
export const DRAWER_CARD_RADIUS = 28;
export const DRAWER_CARD_SHADOW = { offsetY: 24, radius: 30, opacityDark: 0.75, opacityLight: 0.18 } as const;
export const FLOATING_HEADER_HEIGHT = 52;

export type PhoneHeaderMode = "floating" | "native";

/**
 * The home chat screen has no bar: two circles float over it. Every other screen of the drawer
 * phone (Settings, Pull requests, Issues...) keeps the native header and its menu.
 */
export function phoneHeaderMode(input: {
  readonly drawer: boolean;
  readonly settingsOpen: boolean;
  readonly selectedTab: string;
}): PhoneHeaderMode {
  return input.drawer && !input.settingsOpen && input.selectedTab === "threads"
    ? "floating"
    : "native";
}

/** The native header is shown unless the floating circles replace it. */
export function workspaceHeaderShown(mode: PhoneHeaderMode): boolean {
  return mode !== "floating";
}

/** The safe-area top a screen sees under the floating header. */
export function floatingHeaderTop(safeTop: number, mode: PhoneHeaderMode): number {
  return mode === "floating" ? safeTop + FLOATING_HEADER_HEIGHT : safeTop;
}

export interface DrawerThreadRow {
  readonly key: string;
  readonly title: string;
  readonly pinned: boolean;
}

/** The drawer's cloud threads, filtered by the search field; untitled ones read "New chat". */
export function drawerThreadRows(
  threads: readonly { readonly key: string; readonly title: string; readonly pinned?: boolean }[],
  query: string,
  untitled: string,
): readonly DrawerThreadRow[] {
  const needle = query.trim().toLowerCase();
  return threads
    .map((thread) => ({
      key: thread.key,
      title: thread.title === "" ? untitled : thread.title,
      pinned: thread.pinned === true,
    }))
    .filter((row) => needle === "" || row.title.toLowerCase().includes(needle));
}

/** The account row's name: what the account calls itself, then its email, then its subject. */
export function drawerAccountName(
  account: {
    readonly displayName: string | null;
    readonly email: string | null;
    readonly subject: string;
  } | null,
  fallback: string,
): string {
  if (account === null) return fallback;
  return (
    [account.displayName, account.email, account.subject]
      .map((value) => value?.trim() ?? "")
      .find((value) => value !== "") ?? fallback
  );
}
