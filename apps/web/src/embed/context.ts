import { createContext, useContext } from "react";

import type { AppRouter } from "../router";
import type { DevRuntime } from "./runtime";

/** What a host's `DevProvider` or `DevSettings` gives the pieces it draws, and the core's root route. */
export interface EmbedState {
  readonly runtime: DevRuntime | null;
  readonly router: AppRouter;
  /** The two places the core draws into: the host's sidebar and its pane. */
  readonly slots: EmbedSlots;
}

export interface EmbedSlots {
  readonly threads: Slot;
  readonly workspace: Slot;
}

export interface Slot {
  readonly set: (element: HTMLElement | null) => void;
  readonly subscribe: (notify: () => void) => () => void;
  readonly get: () => HTMLElement | null;
}

export function createSlot(): Slot {
  let element: HTMLElement | null = null;
  const listeners = new Set<() => void>();
  return {
    set: (next) => {
      if (next === element) return;
      element = next;
      for (const notify of listeners) notify();
    },
    subscribe: (notify) => {
      listeners.add(notify);
      return () => listeners.delete(notify);
    },
    get: () => element,
  };
}

/** Whether the host shows Dev's code-only sidebar icons (source control, issues). */
let tools = true;
const toolListeners = new Set<() => void>();
export const embedTools = {
  set(next: boolean) {
    if (next === tools) return;
    tools = next;
    for (const notify of toolListeners) notify();
  },
  get: () => tools,
  subscribe(notify: () => void) {
    toolListeners.add(notify);
    return () => toolListeners.delete(notify);
  },
};

export const EmbedContext = createContext<EmbedState | null>(null);

export function useEmbed(): EmbedState | null {
  return useContext(EmbedContext);
}

let embedded = false;

/** True once a host has mounted the core; the root route reads it before React does. */
export function isEmbedded(): boolean {
  return embedded;
}

export function markEmbedded(): void {
  embedded = true;
}
