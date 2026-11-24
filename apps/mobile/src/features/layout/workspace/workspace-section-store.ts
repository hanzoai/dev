import { useSyncExternalStore } from "react";

import {
  INITIAL_SECTION_STATE,
  sectionReducer,
  type SectionState,
  type WorkspaceSection,
} from "./workspace-section.logic";

let state: SectionState = INITIAL_SECTION_STATE;
const listeners = new Set<() => void>();

function dispatch(action: Parameters<typeof sectionReducer>[1]): void {
  const next = sectionReducer(state, action);
  if (next === state) return;
  state = next;
  for (const listener of listeners) listener();
}

export const openWorkspaceSection = (section: WorkspaceSection) =>
  dispatch({ type: "open", section });
export const showDevProjects = (projects: boolean) => dispatch({ type: "projects", projects });
export const showDevMacs = (macs: boolean) => dispatch({ type: "macs", macs });

const subscribe = (listener: () => void) => {
  listeners.add(listener);
  return () => listeners.delete(listener);
};
const getState = () => state;

export function useWorkspaceSection(): SectionState {
  return useSyncExternalStore(subscribe, getState, getState);
}
