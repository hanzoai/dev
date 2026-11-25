import { useSyncExternalStore } from "react";

import { isElectron, isTauri } from "~/env";

import { isMacPlatform, isWindowsPlatform } from "./utils";

const WCO_CLASS_NAME = "wco";
const TAURI_FRAMELESS_CLASS_NAME = "tauri-frameless";
const TAURI_LINUX_CLASS_NAME = "tauri-linux";
const TAURI_FRAMELESS_STYLE_ID = "hanzo-tauri-frameless-chrome";
const ELECTRON_CLASS_NAME = "electron";
const ELECTRON_WINDOWS_CLASS_NAME = "electron-windows";
// macOS has no overlay API; traffic lights stay inside this topbar at every zoom level.
const MACOS_TITLEBAR_HEIGHT = 52;
const TAURI_FRAMELESS_TITLEBAR_HEIGHT = 44;

const TAURI_FRAMELESS_CSS = `
:root {
  --hz-wco-fg: #52525b;
  --hz-wco-fg-hover: #09090b;
  --hz-wco-hover-bg: rgba(0, 0, 0, 0.06);
  --hz-wco-divider: rgba(0, 0, 0, 0.12);
  --hz-menu-bg: #ffffff;
  --hz-menu-border: rgba(0, 0, 0, 0.10);
  --hz-menu-shadow: 0 16px 36px rgba(0, 0, 0, 0.14);
  --hz-menu-item-fg: rgba(15, 15, 18, 0.88);
  --hz-menu-item-hover-fg: #09090b;
  --hz-menu-kbd-fg: rgba(15, 15, 18, 0.45);
  --hz-menu-sep: rgba(0, 0, 0, 0.08);
  --hz-login-bg: #fafafa;
  --hz-login-fg: #09090b;
  --hz-login-muted: #52525b;
  --hz-login-card-border: rgba(0, 0, 0, 0.10);
  --hz-login-primary-bg: #09090b;
  --hz-login-primary-fg: #fafafa;
  --hz-login-secondary-bg: #ffffff;
  --hz-login-secondary-fg: #09090b;
  --hz-login-secondary-border: rgba(0, 0, 0, 0.12);
  --hz-login-guest-bg: #f4f4f5;
  --hz-login-guest-fg: #27272a;
  --hz-login-guest-border: rgba(0, 0, 0, 0.12);
}
html.dark {
  --hz-wco-fg: #a1a1aa;
  --hz-wco-fg-hover: #fafafa;
  --hz-wco-hover-bg: rgba(255, 255, 255, 0.08);
  --hz-wco-divider: rgba(255, 255, 255, 0.12);
  --hz-menu-bg: #121214;
  --hz-menu-border: rgba(255, 255, 255, 0.12);
  --hz-menu-shadow: 0 16px 40px rgba(0, 0, 0, 0.78);
  --hz-menu-item-fg: rgba(245, 245, 245, 0.88);
  --hz-menu-item-hover-fg: #fafafa;
  --hz-menu-kbd-fg: rgba(245, 245, 250, 0.44);
  --hz-menu-sep: rgba(255, 255, 255, 0.08);
  --hz-login-bg: #000000;
  --hz-login-fg: #fafafa;
  --hz-login-muted: #a1a1aa;
  --hz-login-card-border: rgba(255, 255, 255, 0.10);
  --hz-login-primary-bg: #ffffff;
  --hz-login-primary-fg: #09090b;
  --hz-login-secondary-bg: #141417;
  --hz-login-secondary-fg: #fafafa;
  --hz-login-secondary-border: rgba(255, 255, 255, 0.12);
  --hz-login-guest-bg: #121214;
  --hz-login-guest-fg: #e4e4e7;
  --hz-login-guest-border: rgba(255, 255, 255, 0.14);
}
html.tauri-frameless {
  --native-titlebar-height: 44px;
  --workspace-topbar-height: 44px;
  --workspace-controls-top: 0px;
  --workspace-controls-left: 14px;
  --workspace-controls-right: 124px;
  --workspace-native-controls-inset: 124px;
}
html.tauri-frameless [data-chat-header],
html.tauri-frameless [data-slot="title-bar"],
html.tauri-frameless [data-app-sidebar] > div > div:first-child {
  background-color: var(--background);
  border-bottom-color: transparent !important;
}
html:has([data-testid="hanzo-dev-signin-landing"]) [data-slot="window-controls-divider"],
html[data-window-mode="login"] [data-slot="window-controls-divider"] {
  display: none !important;
}
html.tauri-linux {
  background: transparent !important;
}
html.tauri-linux:not([data-maximized="true"]) body {
  border-radius: 12px !important;
  overflow: hidden !important;
  box-shadow: inset 0 0 0 1px var(--hz-wco-divider, rgba(255, 255, 255, 0.10));
}
@media (max-width: 640px) {
  html.tauri-linux:not([data-maximized="true"]) body {
    border-radius: 24px !important;
  }
}
html.tauri-linux[data-window-mode="login"]:not([data-maximized="true"]) body {
  border-radius: 24px !important;
}
html.tauri-linux[data-maximized="true"] body {
  border-radius: 0 !important;
  box-shadow: none !important;
}
`;

interface WindowControlsOverlayLike {
  readonly visible: boolean;
  getTitlebarAreaRect(): DOMRect;
  addEventListener(type: "geometrychange", listener: EventListener): void;
  removeEventListener(type: "geometrychange", listener: EventListener): void;
}

interface NavigatorWithWindowControlsOverlay extends Navigator {
  readonly windowControlsOverlay?: WindowControlsOverlayLike;
}

function getWindowControlsOverlay(): WindowControlsOverlayLike | null {
  if (typeof navigator === "undefined") {
    return null;
  }

  return (navigator as NavigatorWithWindowControlsOverlay).windowControlsOverlay ?? null;
}

function isNonMacFramelessDesktop(): boolean {
  if (typeof navigator === "undefined") return false;
  return (isTauri || isElectron) && !isMacPlatform(navigator.platform);
}

export function ensureHanzoThemeChromeStylesheet(): void {
  if (typeof document === "undefined") return;
  if (document.getElementById(TAURI_FRAMELESS_STYLE_ID)) return;
  const style = document.createElement("style");
  style.id = TAURI_FRAMELESS_STYLE_ID;
  style.textContent = TAURI_FRAMELESS_CSS;
  document.head.appendChild(style);
}

/** Height of the native caption strip drawn over the page, or 0 when the window has none. */
export function windowControlsOverlayHeight(): number {
  const overlay = getWindowControlsOverlay();
  if (overlay?.visible) return overlay.getTitlebarAreaRect().height;
  return isNonMacFramelessDesktop() ? TAURI_FRAMELESS_TITLEBAR_HEIGHT : 0;
}

export function onWindowControlsOverlayGeometryChange(listener: () => void): () => void {
  const overlay = getWindowControlsOverlay();
  if (!overlay) {
    return () => {};
  }

  overlay.addEventListener("geometrychange", listener);
  return () => {
    overlay.removeEventListener("geometrychange", listener);
  };
}

export function syncDocumentWindowControlsOverlayClass(): () => void {
  if (typeof document === "undefined") {
    return () => {};
  }

  ensureHanzoThemeChromeStylesheet();

  const overlay = getWindowControlsOverlay();
  const nonMacFrameless = overlay === null && isNonMacFramelessDesktop();
  const isLinuxDesktop =
    nonMacFrameless &&
    typeof navigator !== "undefined" &&
    (/linux/i.test(navigator.platform) || /linux/i.test(navigator.userAgent));

  const update = () => {
    const activeWco = (overlay !== null && overlay.visible) || nonMacFrameless;
    document.documentElement.classList.toggle(WCO_CLASS_NAME, activeWco);
    document.documentElement.classList.toggle(TAURI_FRAMELESS_CLASS_NAME, nonMacFrameless);
    document.documentElement.classList.toggle(TAURI_LINUX_CLASS_NAME, isLinuxDesktop);
  };

  update();
  const disposeOverlay = onWindowControlsOverlayGeometryChange(update);
  return () => {
    disposeOverlay();
    document.documentElement.classList.remove(
      WCO_CLASS_NAME,
      TAURI_FRAMELESS_CLASS_NAME,
      TAURI_LINUX_CLASS_NAME,
    );
  };
}

function subscribeTitlebarArea(listener: () => void): () => void {
  const overlay = getWindowControlsOverlay();
  overlay?.addEventListener("geometrychange", listener);
  return () => overlay?.removeEventListener("geometrychange", listener);
}

function getNativeTitlebarHeight(): number {
  if (isElectron && isMacPlatform(navigator.platform)) return MACOS_TITLEBAR_HEIGHT;
  const overlay = getWindowControlsOverlay();
  if (overlay?.visible) return overlay.getTitlebarAreaRect().bottom;
  if (isNonMacFramelessDesktop()) return TAURI_FRAMELESS_TITLEBAR_HEIGHT;
  return 0;
}

export function useNativeTitlebarHeight(): number {
  return useSyncExternalStore(subscribeTitlebarArea, getNativeTitlebarHeight, () => 0);
}

function getElectronPlatformClassNames(
  platform: string,
):
  | readonly [typeof ELECTRON_CLASS_NAME]
  | readonly [typeof ELECTRON_CLASS_NAME, typeof ELECTRON_WINDOWS_CLASS_NAME] {
  return isWindowsPlatform(platform)
    ? [ELECTRON_CLASS_NAME, ELECTRON_WINDOWS_CLASS_NAME]
    : [ELECTRON_CLASS_NAME];
}

export function syncDocumentElectronPlatformClasses(platform: string): () => void {
  if (typeof document === "undefined") {
    return () => {};
  }

  ensureHanzoThemeChromeStylesheet();
  const classNames = getElectronPlatformClassNames(platform);
  document.documentElement.classList.add(...classNames);
  return () => {
    document.documentElement.classList.remove(...classNames);
  };
}
