import { useSyncExternalStore } from "react";

export type LocalAiTabId = "models" | "node" | "engine";

export interface LocalAiModalState {
  readonly open: boolean;
  readonly tab: LocalAiTabId;
  readonly filterQuery: string;
}

const CUSTOM_ENGINE_URL_KEY = "hanzo.dev.localAi.engineUrl";
const CUSTOM_HANZOD_URL_KEY = "hanzo.dev.localAi.hanzodUrl";
const SELECTED_LOCAL_MODEL_KEY = "hanzo.dev.localAi.selectedModel";

let modalState: LocalAiModalState = {
  open: false,
  tab: "models",
  filterQuery: "",
};

const listeners = new Set<() => void>();

function emit(): void {
  for (const listener of listeners) {
    listener();
  }
}

export function subscribeLocalAiModal(listener: () => void): () => void {
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}

export function getLocalAiModalState(): LocalAiModalState {
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

export function readSelectedLocalAiModel(): string {
  if (typeof window === "undefined") return "zen4:8b";
  try {
    const raw = window.localStorage.getItem(SELECTED_LOCAL_MODEL_KEY)?.trim();
    if (raw && !/qwen2/iu.test(raw)) return raw;
  } catch {
    // Ignore restricted storage environments.
  }
  return "zen4:8b";
}

export function writeSelectedLocalAiModel(modelId: string): void {
  if (typeof window === "undefined" || /qwen2/iu.test(modelId)) return;
  try {
    window.localStorage.setItem(SELECTED_LOCAL_MODEL_KEY, modelId.trim());
  } catch {
    // Ignore restricted storage environments.
  }
}

export function readCustomLocalEngineUrl(): string {
  if (typeof window === "undefined") return "";
  try {
    return window.localStorage.getItem(CUSTOM_ENGINE_URL_KEY)?.trim() ?? "";
  } catch {
    return "";
  }
}

export function writeCustomLocalEngineUrl(url: string): void {
  if (typeof window === "undefined") return;
  try {
    const trimmed = url.trim().replace(/\/+$/u, "");
    if (trimmed) window.localStorage.setItem(CUSTOM_ENGINE_URL_KEY, trimmed);
    else window.localStorage.removeItem(CUSTOM_ENGINE_URL_KEY);
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
