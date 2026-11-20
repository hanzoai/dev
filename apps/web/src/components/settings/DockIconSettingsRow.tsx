import { useSyncExternalStore } from "react";
import { msg, useGT, useMessages } from "gt-react";
import { isMacPlatform } from "../../lib/utils";
import { Toggle, ToggleGroup } from "../ui/toggle-group";
import { SettingResetButton, SettingsRow } from "./settingsLayout";
import { useSearchableSetting } from "./settingsSearch";

export type DockIconMode = "branded" | "minimal";

export const DEFAULT_DOCK_ICON_MODE: DockIconMode = "branded";

const DOCK_ICON_MODE_LABELS: Record<DockIconMode, string> = {
  branded: msg("Branded"),
  minimal: msg("Minimal"),
};

interface DockIconDesktopBridge {
  readonly getClientPlatform?: () => string;
  readonly getDockIconMode?: () => Promise<string>;
  readonly setDockIconMode?: (mode: DockIconMode) => Promise<string>;
  readonly onDockIconModeChange?: (listener: (mode: DockIconMode) => void) => () => void;
}

interface TauriWindowGlobals {
  readonly desktopBridge?: DockIconDesktopBridge;
  readonly __TAURI_INTERNALS__?: {
    readonly invoke?: (cmd: string, args?: Record<string, unknown>) => Promise<unknown>;
  };
  readonly __TAURI__?: {
    readonly core?: {
      readonly invoke?: (cmd: string, args?: Record<string, unknown>) => Promise<unknown>;
    };
    readonly event?: {
      readonly listen?: (
        event: string,
        handler: (event: { readonly payload: unknown }) => void,
      ) => Promise<() => void>;
    };
  };
}

export interface DockIconBridge {
  readonly getMode: () => Promise<DockIconMode>;
  readonly setMode: (mode: DockIconMode) => Promise<DockIconMode>;
  readonly subscribe: (listener: (mode: DockIconMode) => void) => () => void;
}

export function normalizeDockIconMode(value: unknown): DockIconMode {
  if (typeof value === "string" && value.trim().toLowerCase() === "minimal") {
    return "minimal";
  }
  return DEFAULT_DOCK_ICON_MODE;
}

function getWindowGlobals(): TauriWindowGlobals | null {
  if (typeof window === "undefined") return null;
  return window as unknown as TauriWindowGlobals;
}

function isMacHost(win: TauriWindowGlobals): boolean {
  const bridgePlatform = win.desktopBridge?.getClientPlatform?.();
  if (typeof bridgePlatform === "string" && bridgePlatform.length > 0) {
    return bridgePlatform === "darwin" || isMacPlatform(bridgePlatform);
  }
  return typeof navigator !== "undefined" && isMacPlatform(navigator.platform);
}

export function resolveDockIconBridge(): DockIconBridge | null {
  const win = getWindowGlobals();
  if (!win || !isMacHost(win)) return null;

  const desktopBridge = win.desktopBridge;
  if (
    desktopBridge &&
    typeof desktopBridge.getDockIconMode === "function" &&
    typeof desktopBridge.setDockIconMode === "function"
  ) {
    const getDockIconMode = desktopBridge.getDockIconMode;
    const setDockIconMode = desktopBridge.setDockIconMode;
    const onDockIconModeChange = desktopBridge.onDockIconModeChange;
    return {
      getMode: async () => normalizeDockIconMode(await getDockIconMode()),
      setMode: async (mode) => normalizeDockIconMode(await setDockIconMode(mode)),
      subscribe: (listener) =>
        typeof onDockIconModeChange === "function"
          ? onDockIconModeChange((next) => listener(normalizeDockIconMode(next)))
          : () => {},
    };
  }

  const invoke = win.__TAURI_INTERNALS__?.invoke ?? win.__TAURI__?.core?.invoke;
  if (typeof invoke !== "function") return null;

  const tauriListen = win.__TAURI__?.event?.listen;
  return {
    getMode: async () => normalizeDockIconMode(await invoke("get_dock_icon_mode")),
    setMode: async (mode) => normalizeDockIconMode(await invoke("set_dock_icon_mode", { mode })),
    subscribe: (listener) => {
      if (typeof tauriListen !== "function") return () => {};
      let disposed = false;
      let unlisten: (() => void) | null = null;
      void tauriListen("desktop:dock-icon-mode-changed", (event) => {
        if (!disposed) {
          listener(normalizeDockIconMode(event.payload));
        }
      })
        .then((fn) => {
          if (disposed) {
            fn();
          } else {
            unlisten = fn;
          }
        })
        .catch(() => {});
      return () => {
        disposed = true;
        unlisten?.();
      };
    },
  };
}

let cachedMode: DockIconMode = DEFAULT_DOCK_ICON_MODE;
let activeBridgeUnsubscribe: (() => void) | null = null;
const storeListeners = new Set<() => void>();

function notifyDockIconStore(next: DockIconMode): void {
  if (cachedMode === next) return;
  cachedMode = next;
  for (const listener of storeListeners) {
    listener();
  }
}

function subscribeDockIconStore(listener: () => void): () => void {
  const isFirstSubscriber = storeListeners.size === 0;
  storeListeners.add(listener);

  if (isFirstSubscriber) {
    const bridge = resolveDockIconBridge();
    if (bridge) {
      activeBridgeUnsubscribe = bridge.subscribe((next) => {
        notifyDockIconStore(next);
      });
      void bridge
        .getMode()
        .then((mode) => {
          notifyDockIconStore(mode);
        })
        .catch(() => {});
    }
  }

  return () => {
    storeListeners.delete(listener);
    if (storeListeners.size === 0 && activeBridgeUnsubscribe !== null) {
      activeBridgeUnsubscribe();
      activeBridgeUnsubscribe = null;
    }
  };
}

function getDockIconSnapshot(): DockIconMode {
  return cachedMode;
}

export async function applyDockIconMode(nextMode: DockIconMode): Promise<DockIconMode> {
  const bridge = resolveDockIconBridge();
  if (!bridge) return cachedMode;
  const previousMode = cachedMode;
  notifyDockIconStore(nextMode);
  try {
    const applied = await bridge.setMode(nextMode);
    notifyDockIconStore(applied);
    return applied;
  } catch {
    notifyDockIconStore(previousMode);
    return previousMode;
  }
}

export function resetDockIconStoreForTests(): void {
  activeBridgeUnsubscribe?.();
  activeBridgeUnsubscribe = null;
  storeListeners.clear();
  cachedMode = DEFAULT_DOCK_ICON_MODE;
}

export function DockIconSettingsRow() {
  const gt = useGT();
  const m = useMessages();
  const searchableSetting = useSearchableSetting();
  const bridge = resolveDockIconBridge();
  const mode = useSyncExternalStore(
    subscribeDockIconStore,
    getDockIconSnapshot,
    () => DEFAULT_DOCK_ICON_MODE,
  );

  if (!bridge) return null;

  return (
    <SettingsRow
      {...searchableSetting("dock-icon")}
      description={gt("Choose between the branded H badge or a minimal macOS Dock icon.")}
      resetAction={
        mode !== DEFAULT_DOCK_ICON_MODE ? (
          <SettingResetButton
            ariaLabel={gt("Reset Dock icon to default")}
            onClick={() => {
              void applyDockIconMode(DEFAULT_DOCK_ICON_MODE);
            }}
          />
        ) : null
      }
      control={
        <ToggleGroup
          aria-label={gt("Dock icon")}
          variant="segmented"
          value={[mode]}
          onValueChange={(next) => {
            const candidate = next[0];
            if ((candidate === "branded" || candidate === "minimal") && candidate !== mode) {
              void applyDockIconMode(candidate);
            }
          }}
        >
          <Toggle value="branded">{m(DOCK_ICON_MODE_LABELS.branded)}</Toggle>
          <Toggle value="minimal">{m(DOCK_ICON_MODE_LABELS.minimal)}</Toggle>
        </ToggleGroup>
      }
    />
  );
}
