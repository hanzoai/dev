import "./embedded";
import { RouterProvider } from "@tanstack/react-router";
import { useEffect, useEffectEvent, useState, type ReactNode } from "react";

import { ensureClientAppearance } from "../appInit";
import { I18nProvider } from "../i18n/I18nProvider";
import { whenLocaleReady } from "../i18n/runtime";
import { AppAtomRegistryProvider } from "../rpc/atomRegistry";
import { createSlot, EmbedContext, useEmbed, type EmbedState } from "./context";
import { ensureEmbeddedRouter } from "./DevProvider";
import {
  EmbedSettingsHostContext,
  type DevSettingsExtraSection,
  type DevSettingsNavGroup,
  type EmbedSettingsHost,
} from "./settingsHost";

export type { DevSettingsExtraSection, DevSettingsNavGroup };

/** Built-in Hanzo Dev settings paths rendered natively by `<DevSettings>`. */
export const DEV_SETTINGS_PATHS: ReadonlySet<string> = new Set([
  "/settings/general",
  "/settings/appearance",
  "/settings/chat",
  "/settings/diffs",
  "/settings/keybindings",
  "/settings/voice",
  "/settings/snap-shot",
  "/settings/providers",
  "/settings/usage",
  "/settings/threads",
  "/settings/browser",
  "/settings/devices",
  "/settings/scheduled-tasks",
  "/settings/projects",
  "/settings/source-control",
  "/settings/worktrees",
  "/settings/connections",
  "/settings/hanzo",
  "/settings/access",
  "/settings/secrets",
  "/settings/maintenance",
  "/settings/labs",
  "/settings/about",
  "/settings/archived",
  "/settings/diagnostics",
  "/settings/open-source-licenses",
]);

function cleanSettingsPath(pathname: string): string {
  const raw = pathname.split(/[?#]/)[0] || "/settings/general";
  return raw.length > 1 ? raw.replace(/\/+$/, "") : raw;
}

/** True when `pathname` is a known built-in Hanzo Dev settings route. */
export function isDevSettingsPath(pathname: string): boolean {
  return DEV_SETTINGS_PATHS.has(cleanSettingsPath(pathname));
}

/** Resolve a host settings pathname to a canonical built-in Dev path or extra-section path. */
export function normalizeDevSettingsPath(
  pathname: string,
  extraPaths?: ReadonlySet<string>,
): string {
  const clean = cleanSettingsPath(pathname);
  if (clean === "/settings") return "/settings/general";
  if (extraPaths?.has(clean)) return clean;
  if (DEV_SETTINGS_PATHS.has(clean)) return clean;
  return "/settings/general";
}

export interface DevSettingsProps {
  /** Active host URL pathname (e.g. `"/settings/general"`, `"/settings/appearance"`, `"/settings/billing"`). */
  readonly pathname?: string;
  /** Called when the user selects a settings section or jumps via search. */
  readonly onNavigate?: (pathname: string) => void;
  /** Called when the user clicks the top-right Close (×) button or presses Escape. */
  readonly onClose?: () => void;
  /** Additional host-owned settings sections (e.g. Account, Billing, Referrals, Integrations). */
  readonly extraSections?: readonly DevSettingsExtraSection[];
  /** Optional content rendered at the top of `/settings/usage` (e.g. cloud plan meter & weekly reset). */
  readonly usageHeader?: ReactNode;
  /** Optional content rendered in the settings sidebar footer (e.g. feedback button). */
  readonly sidebarFooter?: ReactNode;
  /** Optional fallback content rendered on `/settings/providers` when no local environment is connected. */
  readonly providersFallback?: ReactNode;
  /** Additional className applied to the full-size settings root. */
  readonly className?: string;
}

/**
 * Hanzo Dev's canonical inline Settings surface for host apps (`hanzo.app`, Desktop).
 * Reuses Dev's real Settings routes, search, and Theme Editor with optional host slots.
 */
export function DevSettings({
  pathname = "/settings/general",
  onNavigate,
  onClose,
  extraSections = [],
  usageHeader,
  sidebarFooter,
  providersFallback,
  className,
}: DevSettingsProps): ReactNode {
  const parentEmbed = useEmbed();
  const extraPaths = new Set(extraSections.map((section) => cleanSettingsPath(section.path)));
  const resolvedPath = normalizeDevSettingsPath(pathname, extraPaths);
  const derivedExtra = extraPaths.has(resolvedPath) ? resolvedPath : null;
  const [extraSelection, setExtraSelection] = useState<{
    readonly forPath: string;
    readonly value: string | null;
  }>(() => ({ forPath: resolvedPath, value: derivedExtra }));
  const activeExtraPath =
    extraSelection.forPath === resolvedPath ? extraSelection.value : derivedExtra;
  const [ready, setReady] = useState(false);

  const router = parentEmbed?.router ?? ensureEmbeddedRouter();
  const [ownSlots] = useState(() => ({ threads: createSlot(), workspace: createSlot() }));
  const embedState: EmbedState = parentEmbed ?? {
    runtime: null,
    router,
    slots: ownSlots,
  };

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- activates client appearance vars, loads the shared router, and waits for i18n locale catalogs once
  useEffect(() => {
    ensureClientAppearance();
    let live = true;
    void router
      .load()
      .then(whenLocaleReady)
      .then(() => {
        if (live) setReady(true);
      });
    return () => {
      live = false;
    };
  }, [router]);

  const routerTarget = derivedExtra ? "/settings/general" : resolvedPath;
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- synchronizes the internal memory-history router to the host URL pathname
  useEffect(() => {
    if (router.history.location.pathname !== routerTarget) {
      void router.navigate({ to: routerTarget as "/settings/general", replace: true });
    }
  }, [router, routerTarget]);

  const onHistoryChange = useEffectEvent(() => {
    if (activeExtraPath !== null) return;
    const current = cleanSettingsPath(router.history.location.pathname);
    if (!current.startsWith("/settings/")) return;
    if (current !== resolvedPath) {
      onNavigate?.(current);
    }
  });

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- forwards internal TanStack Router links (diagnostics, licenses, search jumps) back to the host URL
  useEffect(() => router.history.subscribe(onHistoryChange), [router]);

  const handleSelectExtraPath = (nextPath: string | null) => {
    setExtraSelection({
      forPath: resolvedPath,
      value: nextPath ? cleanSettingsPath(nextPath) : null,
    });
  };

  const handlePathChange = (nextPath: string) => {
    const normalized = cleanSettingsPath(nextPath);
    if (normalized !== resolvedPath) {
      onNavigate?.(normalized);
    }
  };

  const settingsHost: EmbedSettingsHost = {
    inline: true,
    ...(onClose ? { onClose } : {}),
    extraSections,
    activeExtraPath,
    onSelectExtraPath: handleSelectExtraPath,
    onPathChange: handlePathChange,
    ...(usageHeader !== undefined ? { usageHeader } : {}),
    ...(sidebarFooter !== undefined ? { sidebarFooter } : {}),
    ...(providersFallback !== undefined ? { providersFallback } : {}),
  };

  const attachWorkspace = (el: HTMLDivElement | null) => {
    embedState.slots.workspace.set(el);
  };

  const content = (
    <EmbedSettingsHostContext.Provider value={settingsHost}>
      <div
        data-dev-settings-root=""
        className={className}
        style={{
          display: "flex",
          flex: 1,
          width: "100%",
          height: "100%",
          minWidth: 0,
          minHeight: 0,
          position: "relative",
        }}
      >
        {!parentEmbed && ready ? <RouterProvider router={router} /> : null}
        <div
          ref={attachWorkspace}
          style={{
            display: "flex",
            flex: 1,
            width: "100%",
            height: "100%",
            minWidth: 0,
            minHeight: 0,
          }}
        />
      </div>
    </EmbedSettingsHostContext.Provider>
  );

  if (parentEmbed) return content;

  return (
    <EmbedContext.Provider value={embedState}>
      <I18nProvider>
        <AppAtomRegistryProvider>{content}</AppAtomRegistryProvider>
      </I18nProvider>
    </EmbedContext.Provider>
  );
}
