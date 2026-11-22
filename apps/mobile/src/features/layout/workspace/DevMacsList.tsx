import { useNavigation } from "@react-navigation/native";
import { useGT } from "gt-react-native";
import { Pressable, ScrollView, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { SymbolView } from "../../../components/ui/AppSymbol";
import { AppText as Text } from "../../../components/ui/AppText";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { useCloudChat } from "../../chat/cloud-chat";
import { useAuth } from "../../cloud/hanzoMobileAuth";
import { useConnectionController } from "../../connection/useConnectionController";
import { openSettings } from "../../settings/settings-navigation";
import { resolveMacTap } from "./drawer-macs.logic";
import { useDrawerEnvironments } from "./useDrawerEnvironments";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { openWorkspaceSection, showDevMacs } from "./workspace-section-store";

/** Dev's linked Macs + Hanzo Cloud: tapping Cloud opens Cloud Chat or Sandbox; tapping a Mac connects it. */
export function DevMacsList() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const insets = useSafeAreaInsets();
  const navigation = useNavigation();
  const selectTab = useSelectWorkspaceTab();
  const { isSignedIn } = useAuth({ treatPendingAsSignedOut: false });
  const { macs: discoveredMacs, cloud } = useDrawerEnvironments();
  const chat = useCloudChat();
  const connection = useConnectionController();
  const connectedIds = new Set(
    connection.connectedEnvironments.map((environment) => String(environment.environmentId)),
  );
  const relayIds = new Set(
    connection.relayEnvironments.map((entry) => String(entry.environment.environmentId)),
  );

  // Merge discovered Macs, relay environments, and locally connected machines so nothing is missed.
  const mergedById = new Map<
    string,
    { readonly id: string; readonly name: string; readonly online: boolean }
  >();
  for (const env of connection.connectedEnvironments) {
    const id = String(env.environmentId);
    mergedById.set(id, {
      id,
      name: env.environmentLabel,
      online: env.connectionState === "connected",
    });
  }
  for (const entry of connection.relayEnvironments) {
    const id = String(entry.environment.environmentId);
    const existing = mergedById.get(id);
    const online = entry.availability === "online" || entry.environment.state === "online";
    mergedById.set(id, {
      id,
      name: existing?.name || entry.environment.label,
      online: existing ? existing.online || online : online,
    });
  }
  for (const mac of discoveredMacs) {
    const existing = mergedById.get(mac.id);
    mergedById.set(mac.id, {
      id: mac.id,
      name: existing?.name || mac.name,
      online: existing ? existing.online || mac.online : mac.online,
    });
  }
  const macs = [...mergedById.values()].sort(
    (a, b) => Number(b.online) - Number(a.online) || a.name.localeCompare(b.name),
  );

  const openThreads = () => {
    showDevMacs(false);
    selectTab("threads", "dev");
  };
  const openCloud = () => {
    showDevMacs(false);
    if (cloud?.available) {
      void chat.runInCloud();
    }
    openWorkspaceSection("chat");
  };

  return (
    <ScrollView
      className="flex-1"
      contentInsetAdjustmentBehavior="automatic"
      contentContainerClassName="gap-0.5 px-4 pt-3"
      contentContainerStyle={{ paddingBottom: insets.bottom + 16 }}
    >
      {isSignedIn ? (
        <Pressable
          accessibilityRole="button"
          accessibilityLabel={gt("Hanzo Cloud")}
          onPress={openCloud}
          className="h-12 flex-row items-center gap-3 rounded-xl px-3"
        >
          <SymbolView name="cloud" size={18} tintColor={colors["--color-foreground"]} />
          <View className="min-w-0 flex-1">
            <Text className="text-base text-foreground" numberOfLines={1}>
              {gt("Hanzo Cloud")}
            </Text>
          </View>
          <View className="flex-row items-center gap-1.5">
            <View className="h-2 w-2 rounded-full bg-emerald-500" />
            <Text className="text-xs text-foreground-muted">
              {cloud?.available ? gt("Cloud + Sandbox") : gt("Chat Ready")}
            </Text>
          </View>
        </Pressable>
      ) : null}
      {macs.length === 0 ? (
        <Pressable
          accessibilityRole="button"
          onPress={() => openSettings(navigation, "SettingsEnvironments")}
          className="px-3 py-6"
        >
          <Text className="text-center text-base text-foreground-muted">
            {gt(
              "No linked desktop machines yet. Sign in on Hanzo.app or Hanzo Dev on your Mac/PC, or tap to manage Environments.",
            )}
          </Text>
        </Pressable>
      ) : null}
      {macs.map((mac) => (
        <Pressable
          key={mac.id}
          accessibilityRole="button"
          accessibilityLabel={mac.name}
          onPress={() => {
            const tap = resolveMacTap({ macId: mac.id, connectedIds, relayIds });
            if (tap === "settings") {
              openSettings(navigation, "SettingsEnvironments");
              return;
            }
            if (tap === "connect") {
              const relay = connection.availableRelayEnvironments.find(
                (entry) => String(entry.environment.environmentId) === mac.id,
              );
              if (relay) void connection.connectRelayEnvironment(relay.environment);
            }
            openThreads();
          }}
          className="h-12 flex-row items-center gap-3 rounded-xl px-3"
        >
          <SymbolView name="laptopcomputer" size={18} tintColor={colors["--color-foreground"]} />
          <Text className="flex-1 text-base text-foreground" numberOfLines={1}>
            {mac.name}
          </Text>
          <View
            className={
              mac.online ? "h-2 w-2 rounded-full bg-emerald-500" : "h-2 w-2 rounded-full bg-border"
            }
          />
        </Pressable>
      ))}
    </ScrollView>
  );
}
