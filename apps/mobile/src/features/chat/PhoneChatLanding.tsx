import { useGT } from "gt-react-native";
import { useNavigation } from "@react-navigation/native";
import { useState } from "react";
import { Alert, Pressable, ScrollView, TextInput, View } from "react-native";
import { KeyboardAvoidingView } from "react-native-keyboard-controller";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { GlassSurface } from "../../components/ui/GlassSurface";
import { SymbolView } from "../../components/ui/AppSymbol";
import { AppText as Text } from "../../components/ui/AppText";
import { useControlTheme } from "../../components/ui/useControlTheme";
import { useWorkspaceState } from "../../state/workspace";
import { useAdaptiveWorkspaceLayout } from "../layout/AdaptiveWorkspaceLayout";
import { openSettings } from "../settings/settings-navigation";
import { resolveSend, sandboxNotice, type RunTarget } from "./chat-landing.logic";
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
    void cloudChat.runInCloud().then((result) => {
      const notice = sandboxNotice(result, {
        ready: gt("Cloud sandbox ready"),
        readyDetail: gt("Its browser opens from Environments."),
        unavailable: gt("Cloud sandboxes aren't available yet"),
        unavailableDetail: gt("Keep chatting, or run on your Mac."),
        failed: gt("Couldn't start a cloud sandbox"),
      });
      Alert.alert(
        notice.title,
        notice.detail,
        notice.opens
          ? [
              { text: gt("Open"), onPress: () => openSettings(navigation, "SettingsEnvironments") },
              { text: gt("Close"), style: "cancel" },
            ]
          : undefined,
      );
    });
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
    else if (outcome.kind === "chat") {
      void cloudChat.send(prompt);
      setPrompt("");
    }
  };

  const { messages, error, sending, sync } = cloudChat.state;
  const errorText =
    error === "signed-out" || error === "unauthorized"
      ? gt("Sign in again to keep chatting.")
      : error === "unavailable"
        ? gt("Can't reach Hanzo. Check your connection.")
        : error === "failed"
          ? gt("That reply failed. Try again.")
          : null;
  const model =
    cloudChat.models.find((m) => m.id === cloudChat.state.model)?.label ?? cloudChat.state.model;
  const nextModel = () => {
    const list = cloudChat.models;
    const at = list.findIndex((m) => m.id === cloudChat.state.model);
    const next = list[(at + 1) % list.length];
    if (next) cloudChat.selectModel(next.id);
  };
  const composerClearance = Math.max(insets.bottom, 12) + 120;
  return (
    <KeyboardAvoidingView behavior="padding" style={{ flex: 1 }}>
      <View style={{ flex: 1 }}>
        {messages.length === 0 ? (
          <View className="flex-1 items-center justify-center px-6">
            <Text className="text-center text-2xl font-hanzo-bold text-foreground">
              {gt("What are we building?")}
            </Text>
          </View>
        ) : (
          <ScrollView
            className="flex-1 px-4"
            contentContainerStyle={{ gap: 12, paddingVertical: 12, paddingBottom: composerClearance }}
          >
            {messages.map((message, index) => (
              <Text
                key={index}
                selectable
                className={message.role === "user" ? "self-end text-foreground" : "text-foreground"}
              >
                {message.content === "" && sending ? "…" : message.content}
              </Text>
            ))}
            {errorText ? <Text className="text-foreground-muted">{errorText}</Text> : null}
            {sync === "local" && !sending && !error ? (
              <Text className="text-xs text-foreground-muted">
                {gt("Saved on this phone only for now.")}
              </Text>
            ) : null}
          </ScrollView>
        )}
        <View
          className="absolute inset-x-0 bottom-0 px-3"
          style={{ paddingBottom: Math.max(insets.bottom, 12) }}
        >
          <GlassSurface chrome="none" style={{ borderRadius: 28 }}>
            <View className="gap-2 p-3">
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
                <GlassSurface chrome="none" style={{ borderRadius: 16 }}>
                  <Pressable
                    accessibilityRole="button"
                    accessibilityLabel={gt("Model")}
                    disabled={cloudChat.models.length < 2}
                    onPress={nextModel}
                    className="h-8 flex-row items-center px-3"
                  >
                    <Text className="text-sm text-foreground">{model}</Text>
                  </Pressable>
                </GlassSurface>
                <GlassSurface chrome="none" style={{ borderRadius: 16 }}>
                  <Pressable
                    accessibilityRole="button"
                    accessibilityLabel={gt("Build on a machine")}
                    onPress={() => send(true)}
                    className="h-8 flex-row items-center gap-1 px-3"
                  >
                    <SymbolView name="hammer" size={13} tintColor={colors["--color-foreground"]} />
                    <Text className="text-sm text-foreground">{gt("Build")}</Text>
                  </Pressable>
                </GlassSurface>
                <View className="flex-1" />
                <GlassSurface chrome="none" style={{ borderRadius: 18 }}>
                  <Pressable
                    accessibilityRole="button"
                    accessibilityLabel={gt("Send")}
                    onPress={() => send(false)}
                    className="h-9 w-9 items-center justify-center"
                  >
                    <SymbolView name="arrow.up" size={16} tintColor={colors["--color-foreground"]} />
                  </Pressable>
                </GlassSurface>
              </View>
            </View>
          </GlassSurface>
        </View>
      </View>
    </KeyboardAvoidingView>
  );
}
