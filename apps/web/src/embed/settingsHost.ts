import { createContext, useContext, type ReactNode } from "react";

export type DevSettingsNavGroup =
  | "Account"
  | "Preferences"
  | "Agents"
  | "Code"
  | "Connections"
  | "More";

export interface DevSettingsExtraSection {
  /** Unique path under `/settings/`, e.g. `/settings/billing` or `/settings/account`. */
  readonly path: string;
  /** Sidebar and breadcrumb label. */
  readonly label: string;
  /** Group where the entry appears in the settings sidebar. Defaults to `"Account"`. */
  readonly group?: DevSettingsNavGroup;
  /** Optional icon rendered in the settings sidebar (`size-4`). */
  readonly icon?: ReactNode;
  /** Additional keywords indexed by the `/` and `⌘F` settings search. */
  readonly searchTerms?: readonly string[];
  /** Panel body rendered when this section is active. */
  readonly render: () => ReactNode;
}

export interface EmbedSettingsHost {
  /** Render full-bleed inline without the floating AppRouteModal dialog backdrop/portal. */
  readonly inline: boolean;
  /** Called when the user clicks Close (×) or presses Escape. */
  readonly onClose?: () => void;
  /** Host-supplied settings sections (e.g. Account, Billing, Referrals, Integrations). */
  readonly extraSections: readonly DevSettingsExtraSection[];
  /** Active host-supplied section path, or null when showing a built-in Dev settings route. */
  readonly activeExtraPath: string | null;
  /** Select a host-supplied section path, or null when navigating to a built-in Dev route. */
  readonly onSelectExtraPath: (path: string | null) => void;
  /** Called whenever navigation selects a built-in or extra settings path. */
  readonly onPathChange?: (pathname: string) => void;
  /** Optional header rendered at the top of `/settings/usage` (e.g. cloud plan & weekly reset). */
  readonly usageHeader?: ReactNode;
  /** Optional content rendered in the bottom of the settings sidebar (e.g. feedback button). */
  readonly sidebarFooter?: ReactNode;
  /** Optional content rendered on `/settings/providers` when no machine environment is connected. */
  readonly providersFallback?: ReactNode;
}

export const EmbedSettingsHostContext = createContext<EmbedSettingsHost | null>(null);

export function useEmbedSettingsHost(): EmbedSettingsHost | null {
  return useContext(EmbedSettingsHostContext);
}
