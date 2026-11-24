import { scopedThreadKey } from "@hanzodev/client-runtime/environment";
import type { ScopedThreadRef } from "@hanzodev/contracts";
import { create } from "zustand";
import { createJSONStorage, persist } from "zustand/middleware";

import type { DraftId } from "./composerDraftStore";
import { createMemoryStorage } from "./lib/storage";

export const MEMORY_SWITCH_STORAGE_KEY = "hanzodev:thread-memory:v1";

export type MemorySwitchTarget = ScopedThreadRef | DraftId | string;

export function memorySwitchKey(target: MemorySwitchTarget): string {
  return typeof target === "string" ? target : scopedThreadKey(target);
}

interface MemorySwitchState {
  readonly enabledByKey: Readonly<Record<string, boolean>>;
  readonly isMemoryEnabled: (target: MemorySwitchTarget) => boolean;
  readonly setMemoryEnabled: (target: MemorySwitchTarget, enabled: boolean) => void;
  readonly toggleMemoryEnabled: (target: MemorySwitchTarget) => boolean;
  readonly transferMemorySetting: (from: MemorySwitchTarget, to: MemorySwitchTarget) => void;
}

const memorySwitchStorage = createJSONStorage<Pick<MemorySwitchState, "enabledByKey">>(() =>
  typeof localStorage !== "undefined" ? localStorage : createMemoryStorage(),
);

export const useMemorySwitchStore = create<MemorySwitchState>()(
  persist(
    (set, get) => ({
      enabledByKey: {},
      isMemoryEnabled: (target) => get().enabledByKey[memorySwitchKey(target)] === true,
      setMemoryEnabled: (target, enabled) => {
        const key = memorySwitchKey(target);
        set((state) => {
          if ((state.enabledByKey[key] === true) === enabled) return state;
          const next = { ...state.enabledByKey };
          if (enabled) {
            next[key] = true;
          } else {
            delete next[key];
          }
          return { enabledByKey: next };
        });
      },
      toggleMemoryEnabled: (target) => {
        const next = !get().isMemoryEnabled(target);
        get().setMemoryEnabled(target, next);
        return next;
      },
      transferMemorySetting: (from, to) => {
        const fromKey = memorySwitchKey(from);
        const toKey = memorySwitchKey(to);
        if (fromKey === toKey) return;
        const enabled = get().enabledByKey[fromKey] === true;
        if (!enabled) return;
        set((state) => {
          const next = { ...state.enabledByKey };
          delete next[fromKey];
          next[toKey] = true;
          return { enabledByKey: next };
        });
      },
    }),
    {
      name: MEMORY_SWITCH_STORAGE_KEY,
      storage: memorySwitchStorage,
      partialize: (state) => ({ enabledByKey: state.enabledByKey }),
    },
  ),
);
