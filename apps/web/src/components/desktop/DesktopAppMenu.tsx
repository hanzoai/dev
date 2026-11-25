import type { DesktopWslState } from "@hanzodev/contracts";
import { useGT } from "gt-react";
import {
  ChartNoAxesColumnIcon,
  CpuIcon,
  FolderOpenIcon,
  MessageSquarePlusIcon,
  PowerIcon,
  RefreshCwIcon,
  SearchIcon,
  SettingsIcon,
  TerminalIcon,
} from "lucide-react";
import { useEffect, useRef, useState, type CSSProperties } from "react";

import { isElectron, isTauri } from "../../env";
import { isMacPlatform, isWindowsPlatform } from "../../lib/utils";
import { closeDesktopWindow } from "./DesktopWindowControls";

const TRIGGER_BASE_STYLE: CSSProperties = {
  width: 28,
  height: 28,
  borderRadius: 6,
  border: "none",
  display: "inline-flex",
  alignItems: "center",
  justifyContent: "center",
  cursor: "pointer",
  padding: 0,
  transition: "background-color 0.14s ease, color 0.14s ease",
};

const POPOVER_STYLE: CSSProperties = {
  position: "fixed",
  top: 42,
  left: 10,
  width: 236,
  padding: 5,
  borderRadius: 10,
  backgroundColor: "var(--hz-menu-bg, #121214)",
  border: "1px solid var(--hz-menu-border, rgba(255, 255, 255, 0.12))",
  boxShadow: "var(--hz-menu-shadow, 0 16px 40px rgba(0, 0, 0, 0.78))",
  backdropFilter: "none",
  WebkitBackdropFilter: "none",
  zIndex: 200,
  display: "flex",
  flexDirection: "column",
  gap: 2,
};

const SHORTCUT_STYLE: CSSProperties = {
  fontFamily: "inherit",
  fontSize: 11,
  color: "var(--hz-menu-kbd-fg, rgba(245, 245, 250, 0.44))",
  flexShrink: 0,
};

export function DesktopAppMenuButton({
  onNewThread,
  onOpenFolder,
  onSearch,
  onLocalModelsAndNode,
  onAccountAndUsage,
  onSettings,
}: {
  readonly onNewThread: () => void;
  readonly onOpenFolder: () => void;
  readonly onSearch: () => void;
  readonly onLocalModelsAndNode: () => void;
  readonly onAccountAndUsage: () => void;
  readonly onSettings: () => void;
}) {
  const gt = useGT();
  const [open, setOpen] = useState(false);
  const [hoveredTrigger, setHoveredTrigger] = useState(false);
  const [hoveredItem, setHoveredItem] = useState<string | null>(null);
  const [wslState, setWslState] = useState<DesktopWslState | null>(null);
  const wrapRef = useRef<HTMLDivElement | null>(null);

  const show =
    (isElectron || isTauri) &&
    typeof navigator !== "undefined" &&
    !isMacPlatform(navigator.platform);
  const isWin = typeof navigator !== "undefined" && isWindowsPlatform(navigator.platform);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- queries Windows WSL2 availability once for the Reopen in WSL menu action and badge
  useEffect(() => {
    if (!show || !isWin || typeof window === "undefined") return;
    void window.desktopBridge
      ?.getWslState?.()
      .then((next) => setWslState(next))
      .catch(() => undefined);
  }, [isWin, show]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- closes menu on outside mousedown/Escape and handles Ctrl+R / Alt+F4 / Ctrl+O shortcuts
  useEffect(() => {
    if (!show || typeof window === "undefined") return;
    const onMouseDown = (event: MouseEvent) => {
      if (open && wrapRef.current && !wrapRef.current.contains(event.target as Node)) {
        setOpen(false);
      }
    };
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape" && open) {
        setOpen(false);
        return;
      }
      if ((event.ctrlKey || event.metaKey) && !event.shiftKey && !event.altKey) {
        const lower = event.key.toLowerCase();
        if (lower === "r") {
          event.preventDefault();
          window.location.reload();
        } else if (lower === "o") {
          event.preventDefault();
          onOpenFolder();
        }
      }
      if (event.altKey && event.key === "F4") {
        event.preventDefault();
        closeDesktopWindow();
      }
    };
    document.addEventListener("mousedown", onMouseDown);
    window.addEventListener("keydown", onKeyDown);
    return () => {
      document.removeEventListener("mousedown", onMouseDown);
      window.removeEventListener("keydown", onKeyDown);
    };
  }, [onOpenFolder, open, show]);

  if (!show) return null;

  const activeWslDistro =
    wslState?.distro ??
    wslState?.distros.find((d) => d.isDefault)?.name ??
    wslState?.distros[0]?.name ??
    "Ubuntu";

  const runItem = (fn: () => void) => {
    setOpen(false);
    fn();
  };

  const itemStyle = (id: string, danger = false): CSSProperties => ({
    width: "100%",
    height: 30,
    padding: "0 9px",
    border: "none",
    borderRadius: 7,
    backgroundColor:
      hoveredItem === id
        ? danger
          ? "rgba(239, 68, 68, 0.16)"
          : "var(--hz-wco-hover-bg, rgba(255, 255, 255, 0.08))"
        : "transparent",
    color:
      hoveredItem === id
        ? danger
          ? "#f87171"
          : "var(--hz-menu-item-hover-fg, #fafafa)"
        : "var(--hz-menu-item-fg, rgba(245, 245, 245, 0.88))",
    fontSize: 12.5,
    fontWeight: 500,
    display: "flex",
    alignItems: "center",
    gap: 8,
    cursor: "pointer",
    textAlign: "left",
    transition: "background-color 0.1s ease, color 0.1s ease",
  });

  return (
    <div
      ref={wrapRef}
      data-no-drag=""
      data-tauri-drag-region="false"
      onMouseDown={(event) => event.stopPropagation()}
      style={{
        position: "relative",
        display: "inline-flex",
        alignItems: "center",
        gap: 4,
        pointerEvents: "auto",
      }}
    >
      <button
        type="button"
        aria-label={gt("Application menu")}
        aria-haspopup="menu"
        aria-expanded={open}
        data-slot="desktop-app-menu-trigger"
        data-tauri-drag-region="false"
        onMouseEnter={() => setHoveredTrigger(true)}
        onMouseLeave={() => setHoveredTrigger(false)}
        onClick={() => setOpen((prev) => !prev)}
        style={{
          ...TRIGGER_BASE_STYLE,
          backgroundColor:
            open || hoveredTrigger
              ? "var(--hz-wco-hover-bg, rgba(255, 255, 255, 0.08))"
              : "transparent",
          color:
            open || hoveredTrigger
              ? "var(--hz-wco-fg-hover, #fafafa)"
              : "var(--hz-wco-fg, #a1a1aa)",
        }}
      >
        <svg
          width="15"
          height="9"
          viewBox="0 0 19 11"
          fill="none"
          stroke="currentColor"
          strokeWidth="2"
          strokeLinecap="round"
          aria-hidden="true"
        >
          <path d="M1 1h17M1 10h10.7" />
        </svg>
      </button>

      {isWin && wslState?.enabled ? (
        <span
          data-slot="wsl-badge"
          style={{
            height: 20,
            padding: "0 6px",
            borderRadius: 5,
            fontSize: 10.5,
            fontWeight: 600,
            display: "inline-flex",
            alignItems: "center",
            backgroundColor: "var(--hz-wco-hover-bg, rgba(255, 255, 255, 0.08))",
            color: "var(--hz-wco-fg-hover, #fafafa)",
          }}
        >
          {/* oxlint-disable-next-line hanzo/no-untranslated-copy -- technical WSL distro badge */}
          {`WSL: ${activeWslDistro}`}
        </span>
      ) : null}

      {open ? (
        <div
          role="menu"
          aria-label={gt("Application menu")}
          data-slot="desktop-app-menu"
          data-tauri-drag-region="false"
          style={POPOVER_STYLE}
        >
          <button
            type="button"
            role="menuitem"
            onMouseEnter={() => setHoveredItem("new-thread")}
            onMouseLeave={() => setHoveredItem(null)}
            onClick={() => runItem(onNewThread)}
            style={itemStyle("new-thread")}
          >
            <MessageSquarePlusIcon size={14} aria-hidden="true" />
            <span style={{ flex: 1, minWidth: 0 }}>{gt("New thread")}</span>
            <kbd style={SHORTCUT_STYLE}>Ctrl+N</kbd>
          </button>

          <button
            type="button"
            role="menuitem"
            onMouseEnter={() => setHoveredItem("open-folder")}
            onMouseLeave={() => setHoveredItem(null)}
            onClick={() => runItem(onOpenFolder)}
            style={itemStyle("open-folder")}
          >
            <FolderOpenIcon size={14} aria-hidden="true" />
            <span style={{ flex: 1, minWidth: 0 }}>{gt("Open folder…")}</span>
            <kbd style={SHORTCUT_STYLE}>Ctrl+O</kbd>
          </button>

          <button
            type="button"
            role="menuitem"
            onMouseEnter={() => setHoveredItem("search")}
            onMouseLeave={() => setHoveredItem(null)}
            onClick={() => runItem(onSearch)}
            style={itemStyle("search")}
          >
            <SearchIcon size={14} aria-hidden="true" />
            <span style={{ flex: 1, minWidth: 0 }}>{gt("Search…")}</span>
            <kbd style={SHORTCUT_STYLE}>Ctrl+K</kbd>
          </button>

          {isWin && wslState?.available ? (
            <button
              type="button"
              role="menuitem"
              onMouseEnter={() => setHoveredItem("wsl-toggle")}
              onMouseLeave={() => setHoveredItem(null)}
              onClick={() =>
                runItem(() => {
                  void window.desktopBridge
                    ?.setWslBackendEnabled?.(!wslState.enabled)
                    .then(() => window.location.reload());
                })
              }
              style={itemStyle("wsl-toggle")}
            >
              <TerminalIcon size={14} aria-hidden="true" />
              <span style={{ flex: 1, minWidth: 0 }}>
                {wslState.enabled
                  ? gt("Reopen in Windows")
                  : gt("Reopen in WSL ({distro})", { distro: activeWslDistro })}
              </span>
            </button>
          ) : null}

          <div
            role="separator"
            style={{
              height: 1,
              margin: "4px 6px",
              backgroundColor: "var(--hz-menu-sep, rgba(255, 255, 255, 0.08))",
            }}
          />

          <button
            type="button"
            role="menuitem"
            onMouseEnter={() => setHoveredItem("local-node")}
            onMouseLeave={() => setHoveredItem(null)}
            onClick={() => runItem(onLocalModelsAndNode)}
            style={itemStyle("local-node")}
          >
            <CpuIcon size={14} aria-hidden="true" />
            <span style={{ flex: 1, minWidth: 0 }}>{gt("Local Models & Node")}</span>
          </button>

          <button
            type="button"
            role="menuitem"
            onMouseEnter={() => setHoveredItem("account-usage")}
            onMouseLeave={() => setHoveredItem(null)}
            onClick={() => runItem(onAccountAndUsage)}
            style={itemStyle("account-usage")}
          >
            <ChartNoAxesColumnIcon size={14} aria-hidden="true" />
            <span style={{ flex: 1, minWidth: 0 }}>{gt("Account & Usage")}</span>
          </button>

          <button
            type="button"
            role="menuitem"
            onMouseEnter={() => setHoveredItem("settings")}
            onMouseLeave={() => setHoveredItem(null)}
            onClick={() => runItem(onSettings)}
            style={itemStyle("settings")}
          >
            <SettingsIcon size={14} aria-hidden="true" />
            <span style={{ flex: 1, minWidth: 0 }}>{gt("Settings")}</span>
            <kbd style={SHORTCUT_STYLE}>Ctrl+,</kbd>
          </button>

          <div
            role="separator"
            style={{
              height: 1,
              margin: "4px 6px",
              backgroundColor: "var(--hz-menu-sep, rgba(255, 255, 255, 0.08))",
            }}
          />

          <button
            type="button"
            role="menuitem"
            onMouseEnter={() => setHoveredItem("reload")}
            onMouseLeave={() => setHoveredItem(null)}
            onClick={() => runItem(() => window.location.reload())}
            style={itemStyle("reload")}
          >
            <RefreshCwIcon size={14} aria-hidden="true" />
            <span style={{ flex: 1, minWidth: 0 }}>{gt("Reload window")}</span>
            <kbd style={SHORTCUT_STYLE}>Ctrl+R</kbd>
          </button>

          <button
            type="button"
            role="menuitem"
            onMouseEnter={() => setHoveredItem("quit")}
            onMouseLeave={() => setHoveredItem(null)}
            onClick={() => runItem(closeDesktopWindow)}
            style={itemStyle("quit", true)}
          >
            <PowerIcon size={14} aria-hidden="true" />
            <span style={{ flex: 1, minWidth: 0 }}>{gt("Quit Hanzo Dev")}</span>
            <kbd style={SHORTCUT_STYLE}>Alt+F4</kbd>
          </button>
        </div>
      ) : null}
    </div>
  );
}
