import type { CloudChatAttachment } from "@hanzodev/client-runtime/cloud-chat";
import { useGT } from "gt-react-native";
import { useNavigation } from "@react-navigation/native";
import { useState } from "react";
import { Alert, ScrollView, View } from "react-native";
import { KeyboardAvoidingView } from "react-native-keyboard-controller";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { AppText as Text } from "../../components/ui/AppText";
import { useWorkspaceState } from "../../state/workspace";
import { useAdaptiveWorkspaceLayout } from "../layout/AdaptiveWorkspaceLayout";
import { openSettings } from "../settings/settings-navigation";
import {
  resolveSend,
  sandboxNotice,
  type RunTarget,
} from "./chat-landing.logic";
import { ChatLandingComposer } from "./ChatLandingComposer";
import { useCloudChat } from "./cloud-chat";

/** What a signed-in phone opens on: a new chat. Where to run (a Mac or the cloud) is picked in the Dev section. */
export function PhoneChatLanding() {
  const gt = useGT();
  const insets = useSafeAreaInsets();
  const navigation = useNavigation();
  const { startNewTask } = useAdaptiveWorkspaceLayout();
  const { state } = useWorkspaceState();
  const cloudChat = useCloudChat();

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

  const send = (
    prompt: string,
    build: boolean,
    attachments: ReadonlyArray<CloudChatAttachment> = [],
  ): boolean => {
    const outcome = resolveSend({
      prompt,
      build,
      hasReadyEnvironment: state.hasReadyEnvironment,
      cloudChatAvailable: cloudChat.available,
    });
    if (outcome.kind === "machine") startNewTask();
    else if (outcome.kind === "choose-target") chooseTarget();
    else if (outcome.kind === "chat") {
      void cloudChat.send(prompt, attachments);
      return true;
    }
    return false;
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
  const [composerHeight, setComposerHeight] = useState(0);
  const composerClearance = composerHeight + 12;
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
            contentContainerStyle={{
              gap: 12,
              paddingVertical: 12,
              paddingBottom: composerClearance,
            }}
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
        <View className="absolute inset-x-0 bottom-0" pointerEvents="box-none">
          <ChatLandingComposer
            chat={cloudChat}
            bottomInset={insets.bottom}
            onSend={(prompt, attachments) => send(prompt, false, attachments)}
            onHeightChange={setComposerHeight}
          />
        </View>
      </View>
    </KeyboardAvoidingView>
  );
}
