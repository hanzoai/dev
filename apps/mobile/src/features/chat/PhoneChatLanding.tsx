import type { CloudChatAttachment } from "@hanzodev/client-runtime/cloud-chat";
import { useGT } from "gt-react-native";
import { useState } from "react";
import { Pressable, ScrollView, View } from "react-native";
import { KeyboardAvoidingView } from "react-native-keyboard-controller";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { AppText as Text } from "../../components/ui/AppText";
import { useHanzoMobileAccounts } from "../cloud/hanzoMobileAuth";
import { ChatLandingComposer } from "./ChatLandingComposer";
import { useCloudChat } from "./cloud-chat";

const RECOMMENDATIONS = [
  { label: "Brainstorm ideas", prompt: "Help me brainstorm practical architecture options for " },
  { label: "Analyze code", prompt: "Review this code for bugs, edge cases, and performance improvements: " },
  { label: "Draft & refine", prompt: "Write a clear, concise draft for " },
  { label: "Research a topic", prompt: "Give me a crisp technical breakdown of " },
] as const;

function firstNameOf(name: string | null | undefined): string | null {
  if (!name) return null;
  const first = name.trim().split(/\s+/)[0];
  return first || null;
}

/** What a signed-in phone opens on: a new chat. Where to run (a Mac or the cloud) is picked in the Dev section. */
export function PhoneChatLanding() {
  const gt = useGT();
  const insets = useSafeAreaInsets();
  const cloudChat = useCloudChat();
  const { activeAccount } = useHanzoMobileAccounts();

  /** Plain chat always goes to the cloud chat; where to run work is chosen in the Dev section. */
  const send = (prompt: string, attachments: ReadonlyArray<CloudChatAttachment> = []): boolean => {
    if (prompt.trim() === "") return false;
    void cloudChat.send(prompt, attachments);
    return true;
  };

  const firstName = firstNameOf(activeAccount?.displayName ?? activeAccount?.name);
  const hour = new Date().getHours();
  const partOfDay =
    hour < 12 ? gt("Good morning") : hour < 18 ? gt("Good afternoon") : gt("Good evening");
  const greeting = firstName ? `${partOfDay}, ${firstName}.` : gt("What's next?");

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
          <View className="flex-1 items-center justify-center gap-5 px-6 pb-24">
            <Text className="text-center text-2xl font-hanzo-bold text-foreground">
              {greeting}
            </Text>
            <View className="flex-row flex-wrap items-center justify-center gap-2">
              {RECOMMENDATIONS.map((rec) => (
                <Pressable
                  key={rec.label}
                  accessibilityRole="button"
                  onPress={() => send(rec.prompt)}
                  className="rounded-full border border-border bg-grouped-card/50 px-3.5 py-2"
                >
                  <Text className="text-xs text-foreground-muted">{rec.label}</Text>
                </Pressable>
              ))}
            </View>
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
