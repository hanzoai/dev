import { useSyncExternalStore } from "react";

import { parseLocalEngines, trimBaseUrl, type LocalEngine } from "@hanzodev/shared/localAi";

import { trackHanzoEvent } from "../analytics/hanzoWebAnalytics";

export type LocalAiTabId = "engines" | "node";

export interface LocalAiModalState {
  readonly open: boolean;
  readonly tab: LocalAiTabId;
  readonly filterQuery: string;
}

const CUSTOM_ENGINES_KEY = "hanzo.dev.localAi.engines";
const CUSTOM_HANZOD_URL_KEY = "hanzo.dev.localAi.hanzodUrl";

let modalState: LocalAiModalState = {
  open: false,
  tab: "engines",
  filterQuery: "",
};

const listeners = new Set<() => void>();

function emit(): void {
  for (const listener of listeners) {
    listener();
  }
}

function subscribeLocalAiModal(listener: () => void): () => void {
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}

function getLocalAiModalState(): LocalAiModalState {
  return modalState;
}

export function openLocalAiManager(
  options?: LocalAiTabId | { readonly tab?: LocalAiTabId; readonly query?: string },
): void {
  const tab = typeof options === "string" ? options : (options?.tab ?? modalState.tab);
  const filterQuery = typeof options === "object" && options?.query ? options.query : "";
  modalState = {
    open: true,
    tab,
    filterQuery,
  };
  emit();
  trackHanzoEvent("local_ai_opened", { tab });
}

export function closeLocalAiManager(): void {
  if (!modalState.open) return;
  modalState = { ...modalState, open: false };
  emit();
}

export function setLocalAiManagerTab(tab: LocalAiTabId): void {
  modalState = { ...modalState, tab };
  emit();
}

export function useLocalAiModalState(): LocalAiModalState {
  return useSyncExternalStore(subscribeLocalAiModal, getLocalAiModalState, getLocalAiModalState);
}

export function readCustomLocalEngines(): LocalEngine[] {
  if (typeof window === "undefined") return [];
  try {
    return parseLocalEngines(window.localStorage.getItem(CUSTOM_ENGINES_KEY));
  } catch {
    return [];
  }
}

export function writeCustomLocalEngines(engines: ReadonlyArray<LocalEngine>): void {
  if (typeof window === "undefined") return;
  try {
    if (engines.length > 0) {
      const rows = engines.map((engine) => ({ ...engine, baseUrl: trimBaseUrl(engine.baseUrl) }));
      window.localStorage.setItem(CUSTOM_ENGINES_KEY, JSON.stringify(rows));
    } else {
      window.localStorage.removeItem(CUSTOM_ENGINES_KEY);
    }
  } catch {
    // Ignore restricted storage environments.
  }
}

export function readCustomHanzodUrl(): string {
  if (typeof window === "undefined") return "";
  try {
    return window.localStorage.getItem(CUSTOM_HANZOD_URL_KEY)?.trim() ?? "";
  } catch {
    return "";
  }
}

export function writeCustomHanzodUrl(url: string): void {
  if (typeof window === "undefined") return;
  try {
    const trimmed = url.trim().replace(/\/+$/u, "");
    if (trimmed) window.localStorage.setItem(CUSTOM_HANZOD_URL_KEY, trimmed);
    else window.localStorage.removeItem(CUSTOM_HANZOD_URL_KEY);
  } catch {
    // Ignore restricted storage environments.
  }
}
