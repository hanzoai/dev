import { WorkspaceNoTabs } from "./WorkspaceNoTabs";
import { isSharedSlotTab } from "./navigation-tab-order";
import { SharedSlotTitle } from "./SharedSlotTitle";
import { WORKSPACE_TAB_LABELS, useEnabledNavigationTabs } from "./navigation-tabs";
import { WorkspaceExtraTab } from "./WorkspaceExtraTab";
import { WorkspaceSettingsList } from "../../settings/SettingsRouteScreen";
import { WorkspaceSectionToolbar } from "./WorkspaceSectionToolbar";
import { useIsFocused, useNavigation } from "@react-navigation/native";
import { useGT, useMessages } from "gt-react-native";
import { use, useEffect, type ReactNode } from "react";
import { View, useWindowDimensions } from "react-native";
import { useKeyboardState } from "react-native-keyboard-controller";
import { SafeAreaInsetsContext, useSafeAreaInsets } from "react-native-safe-area-context";

import { CompactBrandTitle } from "../../../components/CompactBrandTitle";
import { HeaderItemsWidthContext } from "../../../native/header-items-width";
import { NativeStackScreenOptions } from "../../../native/StackHeader";
import {
  NativePrimaryColumnContext,
  NativeWorkspaceTabActivityContext,
} from "../../../native/v5-workspace-context";
import { isDuoDevice } from "../../../lib/device-type";
import { NATIVE_LIQUID_GLASS_SUPPORTED } from "../../../native/native-glass";
import {
  useDeviceType,
  useNativeColumnLayoutMetrics,
  useNativeLayoutMetrics,
} from "../../../native/native-layout-metrics";
import { HomeRouteScreen } from "../../home/HomeRouteScreen";
import { WorkspaceConnectionTitle } from "../../home/WorkspaceConnectionTitle";
import { useAdaptiveWorkspaceLayout } from "../AdaptiveWorkspaceLayout";
import { PullRequestsScreen } from "../../pull-requests/PullRequestsScreen";
import { IssuesScreen } from "../../issues/IssuesScreen";
import {
  WorkspaceTabsContext,
  WorkspaceTabsProvider,
  useWorkspaceTabs,
} from "./WorkspaceTabsContext";
import { WorkspaceToolbar } from "./WorkspaceToolbar";
import { WORKSPACE_DOCK, workspaceDockBottomClearance } from "./workspace-tabs-state";
import { WorkspaceSidebarTabs } from "./WorkspaceSidebarTabs";
import {
  useDrawerWorkspace,
  useSidebarWorkspaceTabs,
  useSystemWorkspaceTabBar,
} from "./useSystemWorkspaceTabBar";
import { openSettings } from "../../settings/settings-navigation";
import { PhoneChatLanding } from "../../chat/PhoneChatLanding";
import { PhoneFloatingHeader } from "./usePhoneHeaderButtons";
import { WorkspaceDrawer } from "./WorkspaceDrawer";
import { WorkspaceDrawerMenu } from "./WorkspaceDrawerMenu";
import { closeWorkspaceDrawer } from "./workspace-drawer-store";
import { DevMacsList } from "./DevMacsList";
import { WorkspaceWebSection } from "./WorkspaceWebSection";
import { useWorkspaceSection } from "./workspace-section-store";
import { isWebSection } from "./workspace-section.logic";
import { floatingHeaderTop, phoneHeaderMode, workspaceHeaderShown } from "./workspace-drawer.logic";

const TAB_TITLES = WORKSPACE_TAB_LABELS;

function WorkspaceRootTitle(props: {
  readonly columnWidth: number;
  readonly onOpenEnvironments: () => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const { state, settingsOpen } = useWorkspaceTabs()!;
  const { section, macs } = useWorkspaceSection();
  const maxWidth = Math.max(0, props.columnWidth - (use(HeaderItemsWidthContext) ?? 160));
  const enabled = useEnabledNavigationTabs();
  const tabTitle = settingsOpen
    ? gt("Settings")
    : section === "dev" && macs
      ? gt("Macs")
      : enabled.length
        ? m(TAB_TITLES[state.selectedTab])
        : gt("Navigation", { $context: "screen title when every navigation tab is hidden" });
  return (
    <View style={{ width: maxWidth, minHeight: 32, justifyContent: "center" }}>
      <WorkspaceConnectionTitle
        brand={
          !settingsOpen && isSharedSlotTab(state.selectedTab) ? (
            <SharedSlotTitle tab={state.selectedTab} />
          ) : (
            <CompactBrandTitle
              accessibilityLabel={gt("{product}, {title}", {
                product: "Hanzo Dev",
                title: tabTitle,
              })}
              fit
            />
          )
        }
        maxWidth={maxWidth}
        onPress={props.onOpenEnvironments}
      />
    </View>
  );
}

/** On the phone the content slides aside for the drawer; elsewhere it is the content. */
function DrawerHost(props: {
  readonly drawer: boolean;
  readonly floating: boolean;
  readonly children: ReactNode;
}) {
  const focused = useIsFocused();
  const insets = useSafeAreaInsets();
  return props.drawer ? (
    <WorkspaceDrawer
      enabled={focused}
      menu={<WorkspaceDrawerMenu />}
      floating={props.floating ? <PhoneFloatingHeader top={insets.top + 4} /> : null}
    >
      {props.children}
    </WorkspaceDrawer>
  ) : (
    props.children
  );
}

function WorkspaceRootContent() {
  const gt = useGT();
  const m = useMessages();
  const navigation = useNavigation();
  const { width: windowWidth } = useWindowDimensions();
  const { layout, startNewTask } = useAdaptiveWorkspaceLayout();
  const primaryColumn = use(NativePrimaryColumnContext);
  const columnMetrics = useNativeColumnLayoutMetrics();
  const nativeMetrics = useNativeLayoutMetrics();
  const deviceType = useDeviceType();
  const systemTabBar = useSystemWorkspaceTabBar();
  const drawer = useDrawerWorkspace();
  const focused = useIsFocused();
  const enabled = useEnabledNavigationTabs();
  const sidebarTabs = useSidebarWorkspaceTabs();
  const sidebarPicker = sidebarTabs === "picker" || (deviceType === "pad" && layout.usesSplitView);
  const sidebarNavigation =
    primaryColumn !== null &&
    (layout.usesSplitView ||
      deviceType === "pad" ||
      (nativeMetrics !== null && nativeMetrics.verticalBarEdge !== "none"));
  const width = columnMetrics?.width ?? windowWidth;
  const insets = useSafeAreaInsets();
  const keyboardHeight = useKeyboardState((keyboard) => keyboard.height);
  const workspace = useWorkspaceTabs()!;
  const { state } = workspace;
  // Settings is a workspace tab too; its list takes the place of the selected tab's.
  const settingsOpen = workspace.settingsOpen;
  const headerOnly = use(NativeWorkspaceTabActivityContext)?.headerOnly === true;
  const { section, macs } = useWorkspaceSection();
  const headerMode = phoneHeaderMode({
    drawer: drawer && !headerOnly,
    settingsOpen,
    section,
  });
  const webSection = drawer && !settingsOpen && isWebSection(section);
  const headerShown = workspaceHeaderShown(headerMode);
  const renderHeaderTitle = () => (
    <WorkspaceRootTitle
      columnWidth={width}
      onOpenEnvironments={() => openSettings(navigation, "SettingsEnvironments")}
    />
  );
  const tab = state.tabs[settingsOpen ? "settings" : state.selectedTab];
  const bottomClearance =
    layout.variant === "medium" && state.selectedTab === "usage"
      ? workspaceDockBottomClearance({ bottomInset: insets.bottom, keyboardHeight })
      : isDuoDevice(deviceType)
        ? // The column's safe area holds the Duo's search toolbar, but not the sidebar's own tab bar.
          Math.max(insets.bottom + (sidebarPicker ? 64 : 0), keyboardHeight) + 16
        : drawer
          ? // The phone has no tab bar or dock; the drawer holds the destinations.
            Math.max(insets.bottom, keyboardHeight) + 16
          : deviceType === "pad"
            ? workspaceDockBottomClearance({ bottomInset: insets.bottom, keyboardHeight })
            : Math.max(insets.bottom, keyboardHeight) +
              (systemTabBar || sidebarTabs || tab.searchMode !== "resting"
                ? sidebarPicker
                  ? 80
                  : 16
                : WORKSPACE_DOCK.height + WORKSPACE_DOCK.bottomGap + 16);
  const context = { ...workspace, bottomClearance };
  // A thread, page or sheet opened from the drawer covers this screen; the drawer is shut by then.
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- shuts the drawer when another screen takes focus
  useEffect(() => {
    if (!focused) closeWorkspaceDrawer();
  }, [focused]);

  return (
    <WorkspaceTabsContext value={context}>
      <View className={sidebarPicker ? "flex-1 bg-drawer" : "flex-1 bg-screen"}>
        <NativeStackScreenOptions
          optionsVersion={[
            settingsOpen,
            state.selectedTab,
            width,
            sidebarNavigation,
            enabled.length,
            headerShown,
            section,
            macs,
          ]}
          options={{
            title: settingsOpen
              ? gt("Settings")
              : section === "dev" && macs
                ? gt("Macs")
                : enabled.length
                ? m(TAB_TITLES[state.selectedTab])
                : gt("Navigation", {
                    $context: "screen title when every navigation tab is hidden",
                  }),
            headerShown,
            headerTransparent: NATIVE_LIQUID_GLASS_SUPPORTED,
            headerStyle: NATIVE_LIQUID_GLASS_SUPPORTED
              ? { backgroundColor: "transparent" }
              : undefined,
            contentStyle: undefined,
            ...(enabled.length === 0
              ? { headerSearchBarOptions: undefined, unstable_headerRightItems: () => [] }
              : {}),
            unstable_headerLeftItems: () => [],
            // Beside a detail column the tab bar names the tab and the detail header the page,
            // so the sidebar keeps the brand.
            headerTitle:
              sidebarNavigation || (!settingsOpen && state.selectedTab !== "usage")
                ? renderHeaderTitle
                : undefined,
          }}
        />
        <DrawerHost drawer={drawer && !headerOnly} floating={headerMode === "floating"}>
          <SafeAreaInsetsContext
            value={{ ...insets, top: floatingHeaderTop(insets.top, headerMode) }}
          >
            {/* Usage draws its own header, and skips its body itself. */}
            {headerOnly && (settingsOpen || state.selectedTab !== "usage") ? null : settingsOpen ? (
              <WorkspaceSettingsList />
            ) : drawer && isWebSection(section) ? (
              <WorkspaceWebSection section={section} />
            ) : drawer && section === "chat" ? (
              <PhoneChatLanding />
            ) : drawer && section === "dev" && macs ? (
              <DevMacsList />
            ) : enabled.length === 0 ? (
              <WorkspaceNoTabs />
            ) : state.selectedTab === "threads" ? (
              <HomeRouteScreen />
            ) : state.selectedTab === "pullRequests" ? (
              <PullRequestsScreen />
            ) : state.selectedTab === "issues" ? (
              <IssuesScreen />
            ) : (
              <WorkspaceExtraTab tab={state.selectedTab} />
            )}
          </SafeAreaInsetsContext>
        </DrawerHost>
        {deviceType !== "pad" && deviceType !== "phone" && sidebarPicker ? (
          <WorkspaceSidebarTabs />
        ) : null}
        {webSection || (enabled.length === 0 && !settingsOpen) ? null : settingsOpen ||
          state.selectedTab !== "usage" ? (
          <WorkspaceToolbar sidebar={sidebarNavigation} onNewTask={startNewTask} />
        ) : (
          <WorkspaceSectionToolbar onNewTask={startNewTask} showCompose={false} />
        )}
      </View>
    </WorkspaceTabsContext>
  );
}

export function WorkspaceRootScreen() {
  const workspace = useWorkspaceTabs();
  return workspace ? (
    <WorkspaceRootContent />
  ) : (
    <WorkspaceTabsProvider>
      <WorkspaceRootContent />
    </WorkspaceTabsProvider>
  );
}
