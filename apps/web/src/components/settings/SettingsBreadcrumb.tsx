import { msg, useGT, useMessages } from "gt-react";

import {
  WorkspaceBreadcrumb,
  WorkspaceBreadcrumbItem,
  WorkspaceBreadcrumbSeparator,
} from "../WorkspaceBreadcrumb";
import { useEmbedSettingsHost } from "../../embed/settingsHost";
import { SETTINGS_SECTION_LABELS } from "./settingsSearch";

const SETTINGS_LABEL = msg("Settings");

const SETTINGS_BREADCRUMB_LABELS: Readonly<Record<string, string>> = {
  ...SETTINGS_SECTION_LABELS,
  "/settings/diagnostics": msg("Diagnostics"),
  "/settings/open-source-licenses": msg("Open source licenses"),
};

function settingsBreadcrumbLabel(pathname: string): string | null {
  const normalizedPathname = pathname.replace(/\/+$/, "") || "/";
  return SETTINGS_BREADCRUMB_LABELS[normalizedPathname] ?? null;
}

export function SettingsBreadcrumb({ pathname }: { pathname: string }) {
  const gt = useGT();
  const m = useMessages();
  const host = useEmbedSettingsHost();
  const normalizedPathname = pathname.replace(/\/+$/, "") || "/";
  const extraLabel =
    host?.extraSections.find((section) => section.path === normalizedPathname)?.label ?? null;
  const sectionLabel = settingsBreadcrumbLabel(normalizedPathname);
  const resolvedLabel = extraLabel ?? (sectionLabel ? m(sectionLabel) : null);

  return (
    <WorkspaceBreadcrumb ariaLabel={gt("Settings breadcrumb")}>
      {resolvedLabel ? (
        <>
          <WorkspaceBreadcrumbItem>{m(SETTINGS_LABEL)}</WorkspaceBreadcrumbItem>
          <WorkspaceBreadcrumbSeparator />
        </>
      ) : null}
      <WorkspaceBreadcrumbItem current className="truncate">
        {resolvedLabel ?? m(SETTINGS_LABEL)}
      </WorkspaceBreadcrumbItem>
    </WorkspaceBreadcrumb>
  );
}
