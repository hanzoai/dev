import { useNavigation } from "@react-navigation/native";
import { useGT } from "gt-react-native";
import { Pressable, ScrollView, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { SymbolView } from "../../../components/ui/AppSymbol";
import { AppText as Text } from "../../../components/ui/AppText";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { useConnectionController } from "../../connection/useConnectionController";
import { openSettings } from "../../settings/settings-navigation";
import { resolveMacTap } from "./drawer-macs.logic";
import { useDrawerMacs } from "./useDrawerMacs";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { showDevMacs } from "./workspace-section-store";

/** Dev's linked Macs from discovery: tapping one connects it and opens its threads. */
export function DevMacsList() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const insets = useSafeAreaInsets();
  const navigation = useNavigation();
  const selectTab = useSelectWorkspaceTab();
  const macs = useDrawerMacs();
  const connection = useConnectionController();
  const connectedIds = new Set(
    connection.connectedEnvironments.map((environment) => String(environment.environmentId)),
  );
  const relayIds = new Set(
    connection.relayEnvironments.map((entry) => String(entry.environment.environmentId)),
  );
  const openThreads = () => {
    showDevMacs(false);
    selectTab("threads", "dev");
  };
  return (
    <ScrollView
      className="flex-1"
      contentInsetAdjustmentBehavior="automatic"
      contentContainerClassName="gap-0.5 px-4 pt-3"
      contentContainerStyle={{ paddingBottom: insets.bottom + 16 }}
    >
      {macs.length === 0 ? (
        <Text className="px-3 py-6 text-center text-base text-foreground-muted">
          {gt("No linked Macs. Sign in on a Mac running Hanzo Dev to link it.")}
        </Text>
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
              mac.online ? "h-2 w-2 rounded-full bg-primary" : "h-2 w-2 rounded-full bg-border"
            }
          />
        </Pressable>
      ))}
    </ScrollView>
  );
}
