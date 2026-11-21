import type { UsageSettings, UsageSummary } from "@hanzodev/usage/schema";

import { fetchUsage, fetchUsageSettings, refreshUsage, saveUsageSettings } from "./usageClient";

export interface UsageState {
  readonly summary: UsageSummary | null;
  readonly settings: UsageSettings | null;
  readonly error: string | null;
  readonly refreshing: boolean;
}

const POLL_MS = 30_000;
const listeners = new Set<() => void>();
let state: UsageState = { summary: null, settings: null, error: null, refreshing: false };
let timer: ReturnType<typeof setInterval> | undefined;

function set(patch: Partial<UsageState>) {
  state = { ...state, ...patch };
  for (const listener of listeners) listener();
}

async function poll() {
  try {
    set({ summary: await fetchUsage(), error: null });
  } catch (cause) {
    set({ error: cause instanceof Error ? cause.message : String(cause) });
  }
}

export function subscribeUsage(listener: () => void): () => void {
  listeners.add(listener);
  if (listeners.size === 1) {
    void poll();
    void fetchUsageSettings()
      .then((settings) => set({ settings }))
      .catch(() => undefined);
    timer = setInterval(() => void poll(), POLL_MS);
  }
  return () => {
    listeners.delete(listener);
    if (listeners.size === 0 && timer !== undefined) {
      clearInterval(timer);
      timer = undefined;
    }
  };
}

export const getUsageState = () => state;

export async function refreshUsageNow() {
  set({ refreshing: true });
  try {
    set({ summary: await refreshUsage(), error: null });
  } catch (cause) {
    set({ error: cause instanceof Error ? cause.message : String(cause) });
  } finally {
    set({ refreshing: false });
  }
}

export async function updateUsageSettings(next: UsageSettings) {
  set({ settings: next });
  await saveUsageSettings(next).catch(() => undefined);
  await poll();
}
