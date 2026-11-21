import { useNavigation } from "@react-navigation/native";
import { useGT } from "gt-react-native";
import { Pressable, ScrollView, View } from "react-native";

import { SymbolView } from "../../../components/ui/AppSymbol";
import { AppText as Text } from "../../../components/ui/AppText";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { useCloudChat } from "../../chat/cloud-chat";
import { openSettings } from "../../settings/settings-navigation";
import { closeWorkspaceDrawer } from "./workspace-drawer-store";
import { drawerThreadRows } from "./workspace-drawer.logic";
import { useDrawerMacs } from "./useDrawerMacs";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { useWorkspaceTabs } from "./WorkspaceTabsContext";

/**
 * The drawer's body when no environment is connected: the account's linked Macs and its cloud
 * chat threads. Signing in is linking, so there is no add button and no empty state here.
 */
export function WorkspaceDrawerThreads() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const navigation = useNavigation();
  const selectTab = useSelectWorkspaceTab();
  const workspace = useWorkspaceTabs();
  const chat = useCloudChat();
  const macs = useDrawerMacs();
  const rows = drawerThreadRows(
    chat.state.threads,
    workspace?.state.tabs.threads.query ?? "",
    gt("New chat"),
  );
  const openThread = (key: string) => {
    closeWorkspaceDrawer();
    selectTab("threads");
    void chat.openThread(key);
  };
  return (
    <ScrollView className="flex-1" contentContainerClassName="gap-0.5 px-2 pb-2">
      {macs.length > 0 ? (
        <>
          <Text className="px-3 pb-1 pt-2 text-xs text-foreground-muted">{gt("Your Macs")}</Text>
          {macs.map((mac) => (
            <Pressable
              key={mac.id}
              accessibilityRole="button"
              accessibilityLabel={mac.name}
              onPress={() => {
                closeWorkspaceDrawer();
                openSettings(navigation, "SettingsEnvironments");
              }}
              className="h-11 flex-row items-center gap-3 rounded-xl px-3"
            >
              <SymbolView name="laptopcomputer" size={18} tintColor={colors["--color-foreground"]} />
              <Text className="flex-1 text-base text-foreground" numberOfLines={1}>
                {mac.name}
              </Text>
              <View
                className={mac.online ? "h-2 w-2 rounded-full bg-primary" : "h-2 w-2 rounded-full bg-border"}
              />
            </Pressable>
          ))}
        </>
      ) : null}
      {rows.length > 0 ? (
        <>
          <Text className="px-3 pb-1 pt-3 text-xs text-foreground-muted">{gt("Chats")}</Text>
          {rows.map((row) => (
            <Pressable
              key={row.key}
              accessibilityRole="button"
              accessibilityState={{ selected: chat.state.threadKey === row.key }}
              accessibilityLabel={row.title}
              onPress={() => openThread(row.key)}
              className={
                chat.state.threadKey === row.key
                  ? "h-11 justify-center rounded-xl bg-grouped-card px-3"
                  : "h-11 justify-center rounded-xl px-3"
              }
            >
              <Text className="text-base text-foreground" numberOfLines={1}>
                {row.title}
              </Text>
            </Pressable>
          ))}
        </>
      ) : null}
    </ScrollView>
  );
}
