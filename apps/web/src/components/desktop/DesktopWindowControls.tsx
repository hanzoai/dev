import { useGT } from "gt-react";
import { useEffect, useState, type CSSProperties, type MouseEvent as ReactMouseEvent } from "react";

import { isElectron, isTauri } from "../../env";
import { isMacPlatform } from "../../lib/utils";

interface TauriWindowHandle {
  readonly label?: string;
  readonly startDragging?: () => Promise<void>;
  readonly minimize?: () => Promise<void>;
  readonly toggleMaximize?: () => Promise<void>;
  readonly isMaximized?: () => Promise<boolean>;
  readonly close?: () => Promise<void>;
}

function getTauriWindow(): TauriWindowHandle | null {
  if (typeof window === "undefined") return null;
  const winGlobal = window as unknown as {
    readonly __TAURI__?: {
      readonly window?: { readonly getCurrentWindow?: () => TauriWindowHandle };
    };
  };
  try {
    return winGlobal.__TAURI__?.window?.getCurrentWindow?.() ?? null;
  } catch {
    return null;
  }
}

function invokeWindowPlugin(command: string): Promise<unknown> {
  if (typeof window === "undefined") return Promise.resolve(null);
  const winGlobal = window as unknown as {
    readonly __TAURI_INTERNALS__?: {
      readonly invoke?: (cmd: string, args?: Record<string, unknown>) => Promise<unknown>;
      readonly metadata?: {
        readonly currentWindow?: { readonly label?: string };
        readonly currentWebview?: { readonly windowLabel?: string };
      };
    };
    readonly __TAURI__?: {
      readonly core?: {
        readonly invoke?: (cmd: string, args?: Record<string, unknown>) => Promise<unknown>;
      };
    };
  };
  const label =
    getTauriWindow()?.label ??
    winGlobal.__TAURI_INTERNALS__?.metadata?.currentWindow?.label ??
    winGlobal.__TAURI_INTERNALS__?.metadata?.currentWebview?.windowLabel ??
    "main";
  const fullCmd =
    command.includes("|") || command.startsWith("desktop_") ? command : `plugin:window|${command}`;
  if (winGlobal.__TAURI_INTERNALS__?.invoke) {
    return winGlobal.__TAURI_INTERNALS__.invoke(fullCmd, { label });
  }
  if (winGlobal.__TAURI__?.core?.invoke) {
    return winGlobal.__TAURI__.core.invoke(fullCmd, { label });
  }
  return Promise.resolve(null);
}

const INTERACTIVE_TARGET =
  "button, input, select, textarea, label, summary, a:not([data-tauri-drag-region]), [role='button'], [role='link'], [role='menuitem'], [role='tab'], [role='combobox'], [aria-haspopup], [contenteditable='true'], [data-no-drag], [data-tauri-drag-region='false'], [data-slot='window-controls'], [data-sidebar='trigger']";

export function dragDesktopWindow(event: ReactMouseEvent): void {
  if (event.button !== 0 || !(event.target instanceof Element)) return;
  if (event.target.closest(INTERACTIVE_TARGET)) return;
  if (event.detail > 1) return;
  const win = getTauriWindow();
  if (win?.startDragging) {
    void win.startDragging().catch(() => invokeWindowPlugin("desktop_start_dragging"));
    return;
  }
  void invokeWindowPlugin("desktop_start_dragging");
}

export function toggleDesktopMaximize(event: ReactMouseEvent): void {
  if (!(event.target instanceof Element) || event.target.closest(INTERACTIVE_TARGET)) return;
  const win = getTauriWindow();
  if (win?.toggleMaximize) {
    void win.toggleMaximize().catch(() => invokeWindowPlugin("desktop_toggle_maximize"));
    return;
  }
  void invokeWindowPlugin("desktop_toggle_maximize");
}

const BUTTON_BASE_STYLE: CSSProperties = {
  width: 28,
  height: 28,
  borderRadius: 6,
  border: "none",
  backgroundColor: "transparent",
  color: "#a1a1aa",
  display: "inline-flex",
  alignItems: "center",
  justifyContent: "center",
  cursor: "pointer",
  padding: 0,
  transition: "background-color 0.14s ease, color 0.14s ease",
};

export function DesktopWindowControls({ forceRender = false }: { readonly forceRender?: boolean }) {
  const gt = useGT();
  const [maximized, setMaximized] = useState(false);
  const [hovered, setHovered] = useState<"min" | "max" | "close" | null>(null);
  const show =
    forceRender ||
    ((isElectron || isTauri) &&
      typeof navigator !== "undefined" &&
      !isMacPlatform(navigator.platform));

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- subscribes to window resize and queries Tauri window isMaximized IPC
  useEffect(() => {
    if (!show || typeof window === "undefined") return;
    const sync = () => {
      const win = getTauriWindow();
      const pending = win?.isMaximized ? win.isMaximized() : invokeWindowPlugin("is_maximized");
      void Promise.resolve(pending)
        .then((value) => setMaximized(Boolean(value)))
        .catch(() => undefined);
    };
    sync();
    window.addEventListener("resize", sync);
    return () => window.removeEventListener("resize", sync);
  }, [show]);

  if (!show) return null;

  return (
    <div
      data-slot="window-controls"
      data-tauri-drag-region="false"
      data-no-drag=""
      onMouseDown={(event) => event.stopPropagation()}
      style={{
        display: "inline-flex",
        alignItems: "center",
        gap: 4,
        flexShrink: 0,
        userSelect: "none",
      }}
    >
      <button
        type="button"
        aria-label={gt("Minimize")}
        data-tauri-drag-region="false"
        onMouseEnter={() => setHovered("min")}
        onMouseLeave={() => setHovered(null)}
        onClick={() => {
          const win = getTauriWindow();
          if (win?.minimize) void win.minimize().catch(() => invokeWindowPlugin("minimize"));
          else void invokeWindowPlugin("minimize");
        }}
        style={{
          ...BUTTON_BASE_STYLE,
          backgroundColor: hovered === "min" ? "rgba(255, 255, 255, 0.08)" : "transparent",
          color: hovered === "min" ? "#fafafa" : "#a1a1aa",
        }}
      >
        <svg
          width="12"
          height="12"
          viewBox="0 0 12 12"
          fill="none"
          stroke="currentColor"
          strokeWidth="1.5"
        >
          <line x1="2" y1="6" x2="10" y2="6" strokeLinecap="round" />
        </svg>
      </button>

      <button
        type="button"
        aria-label={maximized ? gt("Restore") : gt("Maximize")}
        data-tauri-drag-region="false"
        onMouseEnter={() => setHovered("max")}
        onMouseLeave={() => setHovered(null)}
        onClick={() => {
          const win = getTauriWindow();
          const act = win?.toggleMaximize
            ? win.toggleMaximize().catch(() => invokeWindowPlugin("desktop_toggle_maximize"))
            : invokeWindowPlugin("desktop_toggle_maximize");
          void Promise.resolve(act).then(() => setMaximized((prev) => !prev));
        }}
        style={{
          ...BUTTON_BASE_STYLE,
          backgroundColor: hovered === "max" ? "rgba(255, 255, 255, 0.08)" : "transparent",
          color: hovered === "max" ? "#fafafa" : "#a1a1aa",
        }}
      >
        {maximized ? (
          <svg
            width="12"
            height="12"
            viewBox="0 0 12 12"
            fill="none"
            stroke="currentColor"
            strokeWidth="1.2"
          >
            <rect x="3.5" y="1.5" width="6.5" height="6.5" rx="1" />
            <path d="M1.5 4.5v5a1 1 0 0 0 1 1h5" strokeLinecap="round" />
          </svg>
        ) : (
          <svg
            width="12"
            height="12"
            viewBox="0 0 12 12"
            fill="none"
            stroke="currentColor"
            strokeWidth="1.2"
          >
            <rect x="2" y="2" width="8" height="8" rx="1.5" />
          </svg>
        )}
      </button>

      <button
        type="button"
        aria-label={gt("Close")}
        data-tauri-drag-region="false"
        onMouseEnter={() => setHovered("close")}
        onMouseLeave={() => setHovered(null)}
        onClick={() => {
          const win = getTauriWindow();
          if (win?.close) void win.close().catch(() => invokeWindowPlugin("close"));
          else void invokeWindowPlugin("close");
        }}
        style={{
          ...BUTTON_BASE_STYLE,
          backgroundColor: hovered === "close" ? "#e5484d" : "transparent",
          color: hovered === "close" ? "#fafafa" : "#a1a1aa",
        }}
      >
        <svg
          width="12"
          height="12"
          viewBox="0 0 12 12"
          fill="none"
          stroke="currentColor"
          strokeWidth="1.5"
        >
          <line x1="2.5" y1="2.5" x2="9.5" y2="9.5" strokeLinecap="round" />
          <line x1="9.5" y1="2.5" x2="2.5" y2="9.5" strokeLinecap="round" />
        </svg>
      </button>
    </div>
  );
}

export function DesktopWindowControlsOverlay() {
  const show =
    (isElectron || isTauri) &&
    typeof navigator !== "undefined" &&
    !isMacPlatform(navigator.platform);
  if (!show) return null;

  return (
    <div
      data-no-drag=""
      data-tauri-drag-region="false"
      style={{
        position: "fixed",
        top: 0,
        right: 12,
        height: "var(--workspace-topbar-height, 44px)",
        display: "flex",
        alignItems: "center",
        zIndex: 60,
        pointerEvents: "auto",
      }}
    >
      <DesktopWindowControls />
    </div>
  );
}
