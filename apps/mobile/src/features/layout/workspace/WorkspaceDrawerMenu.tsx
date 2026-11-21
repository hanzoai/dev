import { useGT, useMessages } from "gt-react-native";
import { Alert, Image, Pressable, TextInput, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { GlassSurface } from "../../../components/ui/GlassSurface";
import { SymbolView } from "../../../components/ui/AppSymbol";
import { AppText as Text } from "../../../components/ui/AppText";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { useHanzoMobileAccounts } from "../../cloud/hanzoMobileAuth";
import { HomeRouteScreen } from "../../home/HomeRouteScreen";
import { useWorkspaceState } from "../../../state/workspace";
import { WorkspaceDrawerThreads } from "./WorkspaceDrawerThreads";
import {
  NAVIGATION_TAB_ICONS,
  WORKSPACE_TAB_LABELS,
  useNavigationTabOrder,
} from "./navigation-tabs";
import {
  navigationCustomizationAvailable,
  resolveHiddenNavigationTabs,
} from "./navigation-tab-order";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { closeWorkspaceDrawer } from "./workspace-drawer-store";
import { startNewCloudChat } from "../../chat/cloud-chat";
import { useAdaptiveWorkspaceLayout } from "../adaptive-workspace-context";
import {
  drawerAccountName,
  drawerAccountInitial,
  drawerDestinations,
  type DrawerDestination,
} from "./workspace-drawer.logic";
import { useWorkspaceTabs } from "./WorkspaceTabsContext";
import { mobilePreferencesAtom } from "../../../state/preferences";
import { useAtomValue } from "@effect/atom-react";
import { AsyncResult } from "effect/reactivity";

function useDrawerDestinations() {
  const preferences = useAtomValue(mobilePreferencesAtom);
  const order = useNavigationTabOrder();
  const hidden = AsyncResult.isSuccess(preferences)
    ? resolveHiddenNavigationTabs(preferences.value.hiddenNavigationTabs)
    : [];
  return drawerDestinations(order, hidden, navigationCustomizationAvailable());
}

function DestinationRow(props: {
  readonly destination: DrawerDestination;
  readonly selected: boolean;
  readonly onPress: () => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const { colors } = useControlTheme();
  const settings = props.destination === "settings";
  const label = settings ? gt("Settings") : m(WORKSPACE_TAB_LABELS[props.destination]);
  const icon = settings ? "gearshape" : NAVIGATION_TAB_ICONS[props.destination];
  return (
    <Pressable
      accessibilityRole="button"
      accessibilityState={{ selected: props.selected }}
      accessibilityLabel={label}
      onPress={props.onPress}
      className={
        props.selected
          ? "h-11 flex-row items-center gap-3 rounded-xl bg-grouped-card px-3"
          : "h-11 flex-row items-center gap-3 rounded-xl px-3"
      }
    >
      <SymbolView name={icon} size={18} tintColor={colors["--color-foreground"]} />
      <Text className="text-base text-foreground">{label}</Text>
    </Pressable>
  );
}

function AccountFooter() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const insets = useSafeAreaInsets();
  const { activeAccount, accounts, switchAccount, signOutAll } = useHanzoMobileAccounts();
  const select = useDrawerSelect();
  const name = drawerAccountName(activeAccount, gt("Hanzo"));
  const email = activeAccount?.email && activeAccount.email !== name ? activeAccount.email : null;
  const openMenu = () => {
    const others = accounts.filter((account) => account.id !== activeAccount?.id);
    Alert.alert(name, email ?? undefined, [
      { text: gt("Usage"), onPress: () => select("usage") },
      { text: gt("Settings"), onPress: () => select("settings") },
      ...others.map((account) => ({
        text: gt("Switch to {name}", { name: drawerAccountName(account, gt("Hanzo")) }),
        onPress: () => void switchAccount(account.id),
      })),
      { text: gt("Sign out"), style: "destructive" as const, onPress: () => void signOutAll() },
      { text: gt("Cancel"), style: "cancel" as const },
    ]);
  };
  return (
    <View className="px-3 pt-2" style={{ paddingBottom: Math.max(insets.bottom, 12) }}>
      <GlassSurface chrome="none" style={{ borderRadius: 28 }}>
        <Pressable
          accessibilityRole="button"
          accessibilityLabel={gt("Account menu")}
          onPress={openMenu}
          className="h-16 flex-row items-center gap-3 px-3"
        >
          {activeAccount?.avatarUrl ? (
            <Image
              source={{ uri: activeAccount.avatarUrl }}
              accessibilityIgnoresInvertColors
              style={{ width: 40, height: 40, borderRadius: 20 }}
            />
          ) : (
            <View className="h-10 w-10 items-center justify-center rounded-full bg-grouped-card">
              <Text className="text-base font-hanzo-bold text-foreground">
                {drawerAccountInitial(name)}
              </Text>
            </View>
          )}
          <View className="flex-1">
            <Text className="text-base font-hanzo-bold text-foreground" numberOfLines={1}>
              {name}
            </Text>
            {email ? (
              <Text className="text-xs text-foreground-muted" numberOfLines={1}>
                {email}
              </Text>
            ) : null}
          </View>
          <SymbolView name="chevron.up" size={14} tintColor={colors["--color-foreground-muted"]} />
        </Pressable>
      </GlassSurface>
    </View>
  );
}

function useDrawerSelect() {
  const selectTab = useSelectWorkspaceTab();
  return (destination: DrawerDestination) => {
    closeWorkspaceDrawer();
    selectTab(destination);
  };
}

/** Hanzo and search, New chat, the sections, Recents, and the account. */
export function WorkspaceDrawerMenu() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const workspace = useWorkspaceTabs()!;
  const destinations = useDrawerDestinations();
  const select = useDrawerSelect();
  const { state: catalog } = useWorkspaceState();
  const { startNewTask } = useAdaptiveWorkspaceLayout();
  const sections = destinations.filter(
    (destination) => destination !== "usage" && destination !== "settings",
  );
  const selected = workspace.settingsOpen ? "settings" : workspace.state.selectedTab;
  const newChat = () => {
    closeWorkspaceDrawer();
    if (catalog.hasReadyEnvironment) startNewTask();
    else {
      startNewCloudChat();
      select("threads");
    }
  };
  return (
    <View className="flex-1 bg-drawer">
      <View className="gap-3 px-4 pb-2 pt-3">
        <Text className="text-2xl font-hanzo-bold text-foreground">{gt("Hanzo")}</Text>
        <GlassSurface chrome="none" style={{ borderRadius: 20 }}>
          <View className="h-10 flex-row items-center gap-2 px-3">
            <SymbolView
              name="magnifyingglass"
              size={15}
              tintColor={colors["--color-foreground-muted"]}
            />
            <TextInput
              value={workspace.state.tabs.threads.query}
              onChangeText={(query) => workspace.dispatch({ type: "query", query, tab: "threads" })}
              placeholder={gt("Search", { $context: "search field placeholder" })}
              accessibilityLabel={gt("Search threads")}
              placeholderTextColor={colors["--color-foreground-muted"]}
              autoCapitalize="none"
              autoCorrect={false}
              returnKeyType="search"
              clearButtonMode="while-editing"
              style={{ flex: 1, fontSize: 16, color: colors["--color-foreground"] }}
            />
          </View>
        </GlassSurface>
      </View>
      <View className="gap-0.5 px-2 pb-1">
        <Pressable
          accessibilityRole="button"
          accessibilityLabel={gt("New chat")}
          onPress={newChat}
          className="h-11 flex-row items-center gap-3 rounded-xl px-3"
        >
          <SymbolView name="square.and.pencil" size={18} tintColor={colors["--color-foreground"]} />
          <Text className="text-base text-foreground">{gt("New chat")}</Text>
        </Pressable>
        {sections.map((destination) => (
          <DestinationRow
            key={destination}
            destination={destination}
            selected={selected === destination}
            onPress={() => select(destination)}
          />
        ))}
      </View>
      <View className={catalog.hasReadyEnvironment ? "max-h-[40%]" : "flex-1"}>
        <WorkspaceDrawerThreads />
      </View>
      {catalog.hasReadyEnvironment ? (
        <View className="flex-1">
          <HomeRouteScreen />
        </View>
      ) : null}
      <AccountFooter />
    </View>
  );
}
