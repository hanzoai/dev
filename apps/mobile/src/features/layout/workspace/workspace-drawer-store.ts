import { useSyncExternalStore } from "react";

import { drawerReducer, type DrawerAction, type DrawerState } from "./workspace-drawer.logic";

let state: DrawerState = { open: false };
const listeners = new Set<() => void>();

export function dispatchWorkspaceDrawer(action: DrawerAction): void {
  const next = drawerReducer(state, action);
  if (next === state) return;
  state = next;
  for (const listener of listeners) listener();
}

export const openWorkspaceDrawer = () => dispatchWorkspaceDrawer({ type: "open" });
export const closeWorkspaceDrawer = () => dispatchWorkspaceDrawer({ type: "close" });

const subscribe = (listener: () => void) => {
  listeners.add(listener);
  return () => listeners.delete(listener);
};
const getOpen = () => state.open;

export function useWorkspaceDrawerOpen(): boolean {
  return useSyncExternalStore(subscribe, getOpen, getOpen);
}
