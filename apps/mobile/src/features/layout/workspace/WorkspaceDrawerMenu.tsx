import { useGT, useMessages } from "gt-react-native";
import { Image, Pressable, TextInput, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { SymbolView } from "../../../components/ui/AppSymbol";
import { AppText as Text } from "../../../components/ui/AppText";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { useHanzoMobileAccounts } from "../../cloud/hanzoMobileAuth";
import { HomeRouteScreen } from "../../home/HomeRouteScreen";
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
import {
  drawerAccountCaption,
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

function AccountFooter(props: { readonly destinations: readonly DrawerDestination[] }) {
  const gt = useGT();
  const insets = useSafeAreaInsets();
  const { activeAccount, billing } = useHanzoMobileAccounts();
  const select = useDrawerSelect();
  const workspace = useWorkspaceTabs();
  const name = activeAccount?.displayName ?? activeAccount?.email ?? gt("Hanzo");
  const caption = activeAccount
    ? drawerAccountCaption({ owner: activeAccount.owner, planName: billing?.planName ?? null })
    : null;
  const selected = workspace?.settingsOpen ? "settings" : workspace?.state.selectedTab;
  return (
    <View
      className="gap-2 border-t border-border px-3 pt-3"
      style={{ paddingBottom: Math.max(insets.bottom, 12) }}
    >
      <View className="flex-row items-center gap-3">
        {activeAccount?.avatarUrl ? (
          <Image
            source={{ uri: activeAccount.avatarUrl }}
            accessibilityIgnoresInvertColors
            style={{ width: 36, height: 36, borderRadius: 18 }}
          />
        ) : (
          <View className="h-9 w-9 items-center justify-center rounded-full bg-grouped-card">
            <Text className="text-base font-hanzo-bold text-foreground">
              {drawerAccountInitial(name)}
            </Text>
          </View>
        )}
        <View className="flex-1">
          <Text className="text-base font-hanzo-bold text-foreground" numberOfLines={1}>
            {name}
          </Text>
          {caption ? (
            <Text className="text-xs text-foreground-muted" numberOfLines={1}>
              {caption.kind === "superAdmin" ? gt("SuperAdmin") : (caption.plan ?? "")}
            </Text>
          ) : null}
        </View>
      </View>
      <View className="flex-row gap-2">
        {props.destinations.map((destination) => (
          <View key={destination} className="flex-1">
            <DestinationRow
              destination={destination}
              selected={selected === destination}
              onPress={() => select(destination)}
            />
          </View>
        ))}
      </View>
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

/** Search, the sections, the threads, and the account: everything the bottom bar reached. */
export function WorkspaceDrawerMenu() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const workspace = useWorkspaceTabs()!;
  const destinations = useDrawerDestinations();
  const select = useDrawerSelect();
  const sections = destinations.filter(
    (destination) => destination !== "usage" && destination !== "settings",
  );
  const account = destinations.filter(
    (destination) => destination === "usage" || destination === "settings",
  );
  const selected = workspace.settingsOpen ? "settings" : workspace.state.selectedTab;
  return (
    <View className="flex-1 bg-drawer">
      <View className="px-3 pb-2 pt-2">
        <View className="h-10 flex-row items-center gap-2 rounded-full bg-grouped-card px-3">
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
      </View>
      <View className="gap-0.5 px-2 pb-1">
        {sections.map((destination) => (
          <DestinationRow
            key={destination}
            destination={destination}
            selected={selected === destination}
            onPress={() => select(destination)}
          />
        ))}
      </View>
      <View className="flex-1">
        <HomeRouteScreen />
      </View>
      <AccountFooter destinations={account} />
    </View>
  );
}
