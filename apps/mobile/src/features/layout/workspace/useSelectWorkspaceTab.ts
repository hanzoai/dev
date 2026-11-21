import { Keyboard } from "react-native";

import { useAdaptiveWorkspaceLayout } from "../adaptive-workspace-context";
import { useWorkspaceTabs } from "./WorkspaceTabsContext";
import { openWorkspaceSection } from "./workspace-section-store";
import { sectionOfTab, type WorkspaceSection } from "./workspace-section.logic";
import type { WorkspaceTab } from "./workspace-tabs-state";

/**
 * Opens a workspace tab, Settings included, from a tab bar, rail, toolbar or command. On the
 * phone `section` says where the cloud chat landing's tab ("threads") belongs; Design, Library
 * and Tabs are sections with no tab of their own, so they ride on "threads".
 */
export function useSelectWorkspaceTab() {
  const dispatch = useWorkspaceTabs()?.dispatch;
  const { openWorkspaceTab } = useAdaptiveWorkspaceLayout();
  return (tab: WorkspaceTab | "settings", section: WorkspaceSection = "chat") => {
    Keyboard.dismiss();
    openWorkspaceTab(tab);
    if (tab === "settings") return;
    openWorkspaceSection(sectionOfTab(tab, section));
    dispatch?.({ type: "select", tab });
  };
}
