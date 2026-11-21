import type { CloudChatAttachment } from "@hanzodev/client-runtime/cloud-chat";
import { useGT } from "gt-react-native";
import { useState } from "react";
import { ScrollView, View } from "react-native";
import { KeyboardAvoidingView } from "react-native-keyboard-controller";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { AppText as Text } from "../../components/ui/AppText";
import { ChatLandingComposer } from "./ChatLandingComposer";
import { useCloudChat } from "./cloud-chat";

/** What a signed-in phone opens on: a new chat. Where to run (a Mac or the cloud) is picked in the Dev section. */
export function PhoneChatLanding() {
  const gt = useGT();
  const insets = useSafeAreaInsets();
  const cloudChat = useCloudChat();

  /** Plain chat always goes to the cloud chat; where to run work is chosen in the Dev section. */
  const send = (prompt: string, attachments: ReadonlyArray<CloudChatAttachment> = []): boolean => {
    if (prompt.trim() === "") return false;
    void cloudChat.send(prompt, attachments);
    return true;
  };

  const { messages, error, sending, sync } = cloudChat.state;
  const errorText =
    error === "signed-out" || error === "unauthorized"
      ? gt("Sign in again to keep chatting.")
      : error === "unavailable"
        ? gt("Can't reach Hanzo. Check your connection.")
        : error === "offline"
          ? gt("Your Mac is offline. Pick a cloud model, or reconnect your Mac.")
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
            onSend={send}
            onHeightChange={setComposerHeight}
          />
        </View>
      </View>
    </KeyboardAvoidingView>
  );
}
