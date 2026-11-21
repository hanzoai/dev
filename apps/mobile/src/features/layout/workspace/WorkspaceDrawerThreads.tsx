import { useGT } from "gt-react-native";
import * as Clipboard from "expo-clipboard";
import { Alert, Pressable, ScrollView, View } from "react-native";

import { SymbolView } from "../../../components/ui/AppSymbol";
import { AppText as Text } from "../../../components/ui/AppText";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { useCloudChat } from "../../chat/cloud-chat";
import { closeWorkspaceDrawer } from "./workspace-drawer-store";
import { drawerThreadRows } from "./workspace-drawer.logic";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { useWorkspaceTabs } from "./WorkspaceTabsContext";

/** The drawer's Recents: the account's cloud chat threads, synced with hanzo.app's chat. */
export function WorkspaceDrawerThreads() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const selectTab = useSelectWorkspaceTab();
  const workspace = useWorkspaceTabs();
  const chat = useCloudChat();
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
      {rows.length > 0 ? (
        <>
          <Text className="px-3 pb-1 pt-3 text-xs text-foreground-muted">{gt("Recents")}</Text>
          {rows.map((row) => (
            <View
              key={row.key}
              className={
                chat.state.threadKey === row.key
                  ? "h-11 flex-row items-center rounded-xl bg-grouped-card pl-3"
                  : "h-11 flex-row items-center rounded-xl pl-3"
              }
            >
              <Pressable
                accessibilityRole="button"
                accessibilityState={{ selected: chat.state.threadKey === row.key }}
                accessibilityLabel={row.title}
                onPress={() => openThread(row.key)}
                className="h-11 flex-1 flex-row items-center gap-2"
              >
                {row.pinned ? (
                  <SymbolView name="pin" size={12} tintColor={colors["--color-foreground-muted"]} />
                ) : null}
                <Text className="text-base text-foreground" numberOfLines={1}>
                  {row.title}
                </Text>
              </Pressable>
              <Pressable
                accessibilityRole="button"
                accessibilityLabel={gt("More")}
                hitSlop={6}
                onPress={() =>
                  Alert.alert(row.title, undefined, [
                    { text: gt("Open"), onPress: () => openThread(row.key) },
                    {
                      text: gt("Rename"),
                      onPress: () =>
                        Alert.prompt(
                          gt("Rename"),
                          undefined,
                          (title) => {
                            void chat.renameThread(row.key, title);
                          },
                          "plain-text",
                          row.title,
                        ),
                    },
                    {
                      text: row.pinned ? gt("Unpin") : gt("Pin"),
                      onPress: () => void chat.pinThread(row.key, !row.pinned),
                    },
                    {
                      text: gt("Copy title"),
                      onPress: () => void Clipboard.setStringAsync(row.title),
                    },
                    {
                      text: gt("Delete"),
                      style: "destructive",
                      onPress: () =>
                        Alert.alert(gt("Delete this chat?"), undefined, [
                          { text: gt("Cancel"), style: "cancel" },
                          {
                            text: gt("Delete"),
                            style: "destructive",
                            onPress: () => void chat.deleteThread(row.key),
                          },
                        ]),
                    },
                    { text: gt("Cancel"), style: "cancel" },
                  ])
                }
                className="h-11 w-11 items-center justify-center"
              >
                <SymbolView
                  name="ellipsis"
                  size={16}
                  tintColor={colors["--color-foreground-muted"]}
                />
              </Pressable>
            </View>
          ))}
        </>
      ) : null}
    </ScrollView>
  );
}
