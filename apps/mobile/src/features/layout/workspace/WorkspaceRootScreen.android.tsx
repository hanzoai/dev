import { useIsFocused } from "@react-navigation/native";
import { useGT } from "gt-react-native";
import { WorkspaceNoTabs } from "./WorkspaceNoTabs";
import { useEnabledNavigationTabs } from "./navigation-tabs";
import { NAVIGATION_TAB_ORDER } from "./navigation-tab-order";
import { WorkspaceExtraTab } from "./WorkspaceExtraTab";
import { useEffect, type ReactNode } from "react";
import { View } from "react-native";
import { SafeAreaInsetsContext, useSafeAreaInsets } from "react-native-safe-area-context";
import { NativeStackScreenOptions } from "../../../native/StackHeader";
import { AppBar } from "../../../components/ui/AppBar";
import { WorkspaceEmptyDetail } from "../WorkspaceEmptyDetail";
import { ScreenContent } from "../../../components/ui/ScreenContent";
import { HomeRouteScreen } from "../../home/HomeRouteScreen";
import { PullRequestsScreen } from "../../pull-requests/PullRequestsScreen";
import { ListSearchToolbar } from "./ListSearchToolbar.android";
import { IssuesScreen } from "../../issues/IssuesScreen";
import { useAdaptiveWorkspaceLayout } from "../AdaptiveWorkspaceLayout";
import { WorkspaceSettledContentWidthContext } from "../workspace-content-width";
import { WorkspaceTabsContext, useWorkspaceTabs } from "./WorkspaceTabsContext";
import { WorkspaceTabPanes } from "./WorkspaceTabPanes";
import { WorkspaceAppBarScope } from "../../../components/ui/WorkspaceAppBar.android";
import { usePaneRole } from "../../../components/ui/Pane";
import { AndroidWorkspaceSidebar } from "./AndroidWorkspaceSidebar.android";
import type { WorkspaceTab } from "./workspace-tabs-state";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { useHardwareBack } from "../../../lib/backHandler";
import { PhoneChatLanding } from "../../chat/PhoneChatLanding";
import { PhoneFloatingHeader } from "./usePhoneHeaderButtons";
import { WorkspaceDrawer } from "./WorkspaceDrawer";
import { WorkspaceDrawerMenu } from "./WorkspaceDrawerMenu";
import { closeWorkspaceDrawer } from "./workspace-drawer-store";
import { DevMacsList } from "./DevMacsList";
import { DevProjectsList } from "./DevProjectsList";
import { WorkspaceWebSection } from "./WorkspaceWebSection";
import { useWorkspaceSection } from "./workspace-section-store";
import { isWebSection } from "./workspace-section.logic";
import { floatingHeaderTop } from "./workspace-drawer.logic";

const TABS: readonly WorkspaceTab[] = NAVIGATION_TAB_ORDER;

export function WorkspaceRootScreen() {
  return usePaneRole() === "sidebar" ? <AndroidWorkspaceSidebar /> : <WorkspaceRoot />;
}

function WorkspaceRoot() {
  const gt = useGT();
  const focused = useIsFocused();
  const { layout, workspacePageWidth } = useAdaptiveWorkspaceLayout();
  const workspace = useWorkspaceTabs()!;
  const enabled = useEnabledNavigationTabs();
  const insets = useSafeAreaInsets();
  const { section, projects, macs } = useWorkspaceSection();
  const drawer = !layout.usesSplitView;
  const context = {
    ...workspace,
    bottomClearance: layout.usesSplitView ? workspace.bottomClearance : Math.max(insets.bottom, 16),
  };
  const tab = workspace.state.selectedTab;
  const selectTab = useSelectWorkspaceTab();
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- shuts the drawer when another screen takes focus
  useEffect(() => {
    if (!focused) closeWorkspaceDrawer();
  }, [focused]);
  // Tabs are panes of Home, not routes, so Back has nothing to pop and would leave the app.
  // An open search keeps Back for itself.
  useHardwareBack(
    tab !== "threads" &&
      enabled.includes("threads") &&
      workspace.state.tabs[tab].searchMode === "resting",
    () => {
      selectTab("threads");
      return true;
    },
  );

  const content = (
    <SafeAreaInsetsContext
      value={drawer ? { ...insets, top: floatingHeaderTop(insets.top, "floating") } : insets}
    >
      {drawer && isWebSection(section) ? (
        <WorkspaceWebSection section={section} />
      ) : drawer && section === "chat" ? (
        <PhoneChatLanding />
      ) : drawer && section === "dev" && projects ? (
        <DevProjectsList />
      ) : drawer && section === "dev" && macs ? (
        <DevMacsList />
      ) : enabled.length === 0 ? (
        layout.usesSplitView ? (
          <View className="flex-1 bg-screen" />
        ) : (
          <WorkspaceNoTabs />
        )
      ) : (
        <WorkspaceAppBarScope enabled={!layout.usesSplitView}>
          <WorkspaceTabPanes
            tabs={TABS}
            selected={tab}
            renderTab={(tab) =>
              tab === "threads" ? (
                <HomeRouteScreen />
              ) : tab === "usage" ? (
                <WorkspacePageFrame width={workspacePageWidth ?? null}>
                  <WorkspaceExtraTab tab={tab} />
                </WorkspacePageFrame>
              ) : layout.usesSplitView ? (
                <>
                  <NativeStackScreenOptions options={{ headerShown: false }} />
                  <AppBar
                    embedded
                    title={
                      tab === "issues"
                        ? gt("Issues", { $context: "screen title, list of issues" })
                        : gt("Pull requests")
                    }
                  />
                  <WorkspaceEmptyDetail tab={tab} />
                </>
              ) : tab === "issues" ? (
                <IssuesScreen
                  renderFrame={(filters, list) => (
                    <>
                      <ListSearchToolbar tab="issues" filters={filters} />
                      <ScreenContent>{list}</ScreenContent>
                    </>
                  )}
                />
              ) : (
                <PullRequestsScreen
                  renderFrame={(filters, list) => (
                    <>
                      <ListSearchToolbar tab="pullRequests" filters={filters} />
                      <ScreenContent>{list}</ScreenContent>
                    </>
                  )}
                />
              )
            }
          />
        </WorkspaceAppBarScope>
      )}
    </SafeAreaInsetsContext>
  );

  return (
    <WorkspaceTabsContext value={context}>
      <View className={layout.usesSplitView ? "flex-1 bg-screen" : "flex-1 bg-header"}>
        {drawer ? <NativeStackScreenOptions options={{ headerShown: false }} /> : null}
        <WorkspaceDrawer
          enabled={drawer && focused}
          menu={<WorkspaceDrawerMenu />}
          floating={drawer ? <PhoneFloatingHeader top={insets.top + 4} /> : null}
        >
          {content}
        </WorkspaceDrawer>
      </View>
    </WorkspaceTabsContext>
  );
}

// Hidden, the page sits in the narrower pane beside the list; laying it out at the width it opens
// at keeps measured bars and breakpoints from catching up a frame after it appears.
function WorkspacePageFrame(props: {
  readonly width: number | null;
  readonly children: ReactNode;
}) {
  if (props.width === null) return props.children;
  return (
    <WorkspaceSettledContentWidthContext value={props.width}>
      <View style={{ flex: 1, width: props.width }}>{props.children}</View>
    </WorkspaceSettledContentWidthContext>
  );
}
