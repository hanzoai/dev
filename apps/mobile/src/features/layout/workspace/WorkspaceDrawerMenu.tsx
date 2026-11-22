import { useGT, useMessages } from "gt-react-native";
import { Alert, Image, Pressable, TextInput, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { GlassSurface } from "../../../components/ui/GlassSurface";
import { SymbolView, type AppSymbolName } from "../../../components/ui/AppSymbol";
import { AppText as Text } from "../../../components/ui/AppText";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { useHanzoMobileAccounts } from "../../cloud/hanzoMobileAuth";
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
import { showDevMacs, useWorkspaceSection } from "./workspace-section-store";
import {
  SECTION_ICONS,
  WORKSPACE_SECTIONS,
  devContents,
  type DevContent,
  type WorkspaceSection,
} from "./workspace-section.logic";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { closeWorkspaceDrawer } from "./workspace-drawer-store";
import { startNewCloudChat } from "../../chat/cloud-chat";
import { useAdaptiveWorkspaceLayout } from "../adaptive-workspace-context";
import { drawerAccountName, drawerAccountInitial } from "./workspace-drawer.logic";
import { useWorkspaceTabs } from "./WorkspaceTabsContext";
import { mobilePreferencesAtom } from "../../../state/preferences";
import { useAtomValue } from "@effect/atom-react";
import { AsyncResult } from "effect/reactivity";

function useDevContents() {
  const preferences = useAtomValue(mobilePreferencesAtom);
  const order = useNavigationTabOrder();
  const hidden = AsyncResult.isSuccess(preferences)
    ? resolveHiddenNavigationTabs(preferences.value.hiddenNavigationTabs)
    : [];
  return devContents(order, navigationCustomizationAvailable() ? hidden : []);
}

/** The section names are hanzo.app's own (@hanzo/app Rail): product names, not translated prose. */
function useSectionLabels(): Readonly<Record<WorkspaceSection, string>> {
  const gt = useGT();
  return {
    chat: gt("Chat"),
    dev: gt("Dev"),
    library: gt("Library"),
    design: gt("Design"),
    plugins: gt("Plugins"),
  };
}

function NavRow(props: {
  readonly label: string;
  readonly icon: AppSymbolName;
  readonly selected: boolean;
  readonly indent?: boolean;
  readonly onPress: () => void;
}) {
  const { colors } = useControlTheme();
  const tint = props.indent ? colors["--color-foreground-muted"] : colors["--color-foreground"];
  const base = props.indent
    ? "h-10 flex-row items-center gap-3 rounded-xl pl-9 pr-3"
    : "h-11 flex-row items-center gap-3 rounded-xl px-3";
  return (
    <Pressable
      accessibilityRole="button"
      accessibilityState={{ selected: props.selected }}
      accessibilityLabel={props.label}
      onPress={props.onPress}
      className={props.selected ? `${base} bg-grouped-card` : base}
    >
      <SymbolView name={props.icon} size={props.indent ? 15 : 18} tintColor={tint} />
      <Text className={props.indent ? "text-[15px] text-foreground" : "text-base text-foreground"}>
        {props.label}
      </Text>
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
      { text: gt("Usage"), onPress: () => select.other("usage") },
      { text: gt("Settings"), onPress: () => select.other("settings") },
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
  const workspace = useWorkspaceTabs()!;
  return {
    section: (section: WorkspaceSection) => {
      closeWorkspaceDrawer();
      // Dev reopens where it was left; the others ride on the threads tab.
      selectTab(section === "dev" ? workspace.state.selectedTab : "threads", section);
    },
    dev: (content: DevContent) => {
      closeWorkspaceDrawer();
      showDevMacs(content === "macs");
      selectTab(content === "macs" ? "threads" : content, "dev");
    },
    other: (destination: "usage" | "settings") => {
      closeWorkspaceDrawer();
      selectTab(destination);
    },
  };
}

/** Hanzo and search, New chat, hanzo.app's five sections, Recents, and the account. */
export function WorkspaceDrawerMenu() {
  const gt = useGT();
  const m = useMessages();
  const { colors } = useControlTheme();
  const workspace = useWorkspaceTabs()!;
  const select = useDrawerSelect();
  const labels = useSectionLabels();
  const devs = useDevContents();
  const { section, macs } = useWorkspaceSection();
  const { state: catalog } = useWorkspaceState();
  const { startNewTask } = useAdaptiveWorkspaceLayout();
  const active = workspace.settingsOpen ? null : section;
  const devSelected: DevContent = macs ? "macs" : workspace.state.selectedTab;
  const newChat = () => {
    closeWorkspaceDrawer();
    if (section === "dev" && catalog.hasReadyEnvironment) startNewTask();
    else {
      startNewCloudChat();
      select.section(section === "dev" ? "dev" : "chat");
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
        {WORKSPACE_SECTIONS.map((entry) => (
          <View key={entry} className="gap-0.5">
            <NavRow
              label={labels[entry]}
              icon={SECTION_ICONS[entry]}
              selected={active === entry}
              onPress={() => select.section(entry)}
            />
            {entry === "dev" && active === "dev"
              ? devs.map((content) => (
                  <NavRow
                    key={content}
                    indent
                    label={content === "macs" ? gt("Macs") : m(WORKSPACE_TAB_LABELS[content])}
                    icon={content === "macs" ? "laptopcomputer" : NAVIGATION_TAB_ICONS[content]}
                    selected={devSelected === content}
                    onPress={() => select.dev(content)}
                  />
                ))
              : null}
          </View>
        ))}
      </View>
      <View className="flex-1">
        <WorkspaceDrawerThreads />
      </View>
      <AccountFooter />
    </View>
  );
}
