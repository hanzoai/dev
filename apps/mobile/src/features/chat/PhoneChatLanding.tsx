import { useGT } from "gt-react-native";
import { useNavigation } from "@react-navigation/native";
import { useState } from "react";
import { Alert, Pressable, TextInput, View } from "react-native";
import { KeyboardAvoidingView } from "react-native-keyboard-controller";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { SymbolView } from "../../components/ui/AppSymbol";
import { AppText as Text } from "../../components/ui/AppText";
import { useControlTheme } from "../../components/ui/useControlTheme";
import { useWorkspaceState } from "../../state/workspace";
import { useAdaptiveWorkspaceLayout } from "../layout/AdaptiveWorkspaceLayout";
import { openSettings } from "../settings/settings-navigation";
import { resolveSend, type RunTarget } from "./chat-landing.logic";
import { useCloudChat } from "./cloud-chat";

/** What a signed-in phone opens on: a new chat. An environment is asked for only to build. */
export function PhoneChatLanding() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const insets = useSafeAreaInsets();
  const navigation = useNavigation();
  const { startNewTask } = useAdaptiveWorkspaceLayout();
  const { state } = useWorkspaceState();
  const cloudChat = useCloudChat();
  const [prompt, setPrompt] = useState("");

  const run = (target: RunTarget) => {
    if (target === "mac") {
      // Linking a Mac is the environment flow; environment discovery plugs in there.
      openSettings(navigation, "SettingsEnvironmentNew");
      return;
    }
    Alert.alert(gt("Cloud machines are not available yet"), gt("Run on your Mac for now."));
  };

  const chooseTarget = () =>
    Alert.alert(gt("Where should this run?"), gt("Building needs a machine."), [
      { text: gt("Run on your Mac"), onPress: () => run("mac") },
      { text: gt("Run in the cloud"), onPress: () => run("cloud") },
      { text: gt("Cancel"), style: "cancel" },
    ]);

  const send = (build: boolean) => {
    const outcome = resolveSend({
      prompt,
      build,
      hasReadyEnvironment: state.hasReadyEnvironment,
      cloudChatAvailable: cloudChat.available,
    });
    if (outcome.kind === "machine") startNewTask();
    else if (outcome.kind === "choose-target") chooseTarget();
  };

  const model = cloudChat.models[0]?.label ?? gt("Auto");
  return (
    <KeyboardAvoidingView behavior="padding" style={{ flex: 1 }}>
      <View className="flex-1 items-center justify-center px-6">
        <Text className="text-center text-2xl font-hanzo-bold text-foreground">
          {gt("What are we building?")}
        </Text>
      </View>
      <View className="px-3" style={{ paddingBottom: Math.max(insets.bottom, 12) }}>
        <View className="gap-2 rounded-3xl bg-composer-surface p-3">
          <TextInput
            value={prompt}
            onChangeText={setPrompt}
            placeholder={gt("Ask anything")}
            accessibilityLabel={gt("Message")}
            placeholderTextColor={colors["--color-foreground-muted"]}
            multiline
            style={{
              maxHeight: 160,
              minHeight: 36,
              fontSize: 17,
              color: colors["--color-foreground"],
            }}
          />
          <View className="flex-row items-center gap-2">
            <Pressable
              accessibilityRole="button"
              accessibilityLabel={gt("Model")}
              disabled={cloudChat.models.length < 2}
              className="h-8 flex-row items-center rounded-full bg-grouped-card px-3"
            >
              <Text className="text-sm text-foreground">{model}</Text>
            </Pressable>
            <Pressable
              accessibilityRole="button"
              accessibilityLabel={gt("Build on a machine")}
              onPress={() => send(true)}
              className="h-8 flex-row items-center gap-1 rounded-full bg-grouped-card px-3"
            >
              <SymbolView name="hammer" size={13} tintColor={colors["--color-foreground"]} />
              <Text className="text-sm text-foreground">{gt("Build")}</Text>
            </Pressable>
            <View className="flex-1" />
            <Pressable
              accessibilityRole="button"
              accessibilityLabel={gt("Send")}
              onPress={() => send(false)}
              className="h-9 w-9 items-center justify-center rounded-full bg-primary"
            >
              <SymbolView
                name="arrow.up"
                size={16}
                tintColor={colors["--color-primary-foreground"]}
              />
            </Pressable>
          </View>
        </View>
      </View>
    </KeyboardAvoidingView>
  );
}
