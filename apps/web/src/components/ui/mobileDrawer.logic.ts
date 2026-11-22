// The numbers of the Hanzo phone drawer (hanzo.app slide.ts, Dev iOS), in CSS px.
/** Pane left in sight at the trailing edge while the drawer is open. */
const DRAWER_STRIP = 64;
/** The drawer's widest. */
const DRAWER_REACH = 360;
/** px a finger moves before a drag is a drag. */
const DRAWER_SLOP = 8;
/** A flick, px per ms: faster than this decides by its direction, whatever the distance. */
export const DRAWER_FLICK = 0.4;
/** The slide and the dim run on one clock. */
export const DRAWER_DURATION_MS = 260;
export const DRAWER_EASING = "cubic-bezier(.2,.8,.2,1)";
/** Where a drag may begin when the drawer is closed, px from the leading edge. */
const DRAWER_EDGE = 24;
/** Dim over the content at full open. */
const DRAWER_DIM = 0.45;

export function drawerReach(windowWidth: number): number {
  return Math.min(Math.max(0, windowWidth - DRAWER_STRIP), DRAWER_REACH);
}

/** How far open the drawer stands while a finger holds it: never past closed or open. */
export function drawerPull(from: number, dx: number, reach: number): number {
  return Math.min(reach, Math.max(0, from + dx));
}

/** Whether a drawer let go at `at`, moving `speed` px/ms (positive opens), stays open. */
export function drawerSettles(at: number, reach: number, speed: number): boolean {
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

/** Whether a touch that lands at `x` may begin a drag: the edge when closed, anywhere when open. */
export function drawerGrabs(x: number, open: boolean): boolean {
  return open || x <= DRAWER_EDGE;
}

/** Dim for a drawer `at` px open. */
export function drawerDim(at: number, reach: number): number {
  return reach <= 0 ? 0 : (at / reach) * DRAWER_DIM;
}

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
  readonly owner: string | null | undefined;
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
