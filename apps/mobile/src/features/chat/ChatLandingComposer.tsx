import { cloudChatSelection } from "@hanzodev/client-runtime/cloud-chat";
import { EnvironmentId, MessageId, ProjectId, ThreadId } from "@hanzodev/contracts";
import { useGT } from "gt-react-native";
import { View } from "react-native";
import { useSharedValue } from "react-native-reanimated";

import * as Dialog from "../../components/ui/Dialog";
import { ComposerInlineControl } from "../../components/ComposerToolbar";
import { useComposerAttachmentActions } from "../../state/use-composer-attachment-actions";
import {
  getComposerDraftSnapshot,
  removeComposerDraftAttachment,
  setComposerDraftText,
  useComposerDraft,
} from "../../state/use-composer-drafts";
import { ComposerPopoverHost } from "../threads/ComposerPopoverHost";
import { ThreadComposer } from "../threads/ThreadComposer";
import { draftThreadShell } from "../threads/draft-thread-shell";
import { CHAT_LANDING_DRAFT_KEY } from "./chat-landing.logic";
import type { CloudChat } from "./cloud-chat";

// Stand-ins for the thread the composer expects. No environment owns them, so every
// environment-keyed lookup the composer makes finds nothing and falls back.
const CLOUD_ENVIRONMENT = EnvironmentId.make("cloud-chat");
const CLOUD_PROJECT = ProjectId.make("cloud-chat");
const CLOUD_THREAD = ThreadId.make("cloud-chat");
const SENT = MessageId.make("cloud-chat");
const CREATED_AT = "2026-01-01T00:00:00.000Z";

/**
 * The thread composer, mounted with no environment: the same editor, attachments, voice and
 * model picker, over the `/v1/models` catalog. Sending streams through cloud chat.
 */
export function ChatLandingComposer(props: {
  readonly chat: CloudChat;
  readonly bottomInset: number;
  readonly onBuild: () => void;
  readonly onHeightChange: (height: number) => void;
  /** Sends the draft as a chat; false when it cannot be sent as one. */
  readonly onSend: (prompt: string) => boolean;
}) {
  const gt = useGT();
  const popoverHeight = useSharedValue(0);
  const draft = useComposerDraft(CHAT_LANDING_DRAFT_KEY);
  const { chat } = props;
  const attachments = useComposerAttachmentActions({
    draftKey: CHAT_LANDING_DRAFT_KEY,
    serverConfig: chat.serverConfig,
  });
  const thread = draftThreadShell({
    environmentId: CLOUD_ENVIRONMENT,
    threadId: CLOUD_THREAD,
    projectId: CLOUD_PROJECT,
    modelSelection: cloudChatSelection(chat.state.model, chat.state.effort),
    runtimeMode: "full-access",
    interactionMode: "default",
    branch: null,
    worktreePath: null,
    createdAt: CREATED_AT,
  });
  const send = async () => {
    const current = getComposerDraftSnapshot(CHAT_LANDING_DRAFT_KEY);
    if (current.attachments.length > 0) {
      // A chat reply sees text only; files and photos travel with a task on a machine.
      Dialog.alert(
        gt("Attachments need a machine"),
        gt("Remove them to keep chatting, or tap Build to run this on a machine."),
      );
      return null;
    }
    if (!props.onSend(current.text)) return null;
    setComposerDraftText(CHAT_LANDING_DRAFT_KEY, "");
    return SENT;
  };
  return (
    <ComposerPopoverHost hidden={false} inFlow popoverHeight={popoverHeight}>
      <ThreadComposer
        isDraft
        modelPickerRoute="ChatLandingModelSettings"
        canOperateThread
        canSwitchProvider
        placeholder={gt("Ask anything")}
        draftControls={
          <View className="flex-row gap-2 pb-2">
            <ComposerInlineControl
              icon="hammer"
              label={gt("Build")}
              showChevron={false}
              accessibilityLabel={gt("Build on a machine")}
              onPress={props.onBuild}
            />
          </View>
        }
        bottomInset={props.bottomInset}
        connectionState="connected"
        environmentLabel={null}
        selectedThread={thread}
        hasCompactableConversation={false}
        serverConfig={chat.serverConfig}
        queueCount={0}
        activeThreadBusy={false}
        redirectsApproval={false}
        canStopThread={false}
        environmentId={CLOUD_ENVIRONMENT}
        projectCwd={null}
        draftKey={CHAT_LANDING_DRAFT_KEY}
        followUpBehavior="queue"
        canSteerActiveTurn={false}
        hidden={false}
        interactionLocked={chat.state.sending}
        onChangeDraftMessage={(value) => setComposerDraftText(CHAT_LANDING_DRAFT_KEY, value)}
        onPickDraftMedia={attachments.onPickDraftMedia}
        onPickDraftFiles={attachments.onPickDraftFiles}
        onNativePasteImages={attachments.onNativePasteImages}
        onNativePasteText={attachments.onNativePasteText}
        onRemoveDraftImage={(id) => removeComposerDraftAttachment(CHAT_LANDING_DRAFT_KEY, id)}
        onStopThread={() => undefined}
        onResumeQueue={null}
        onSendMessage={send}
        onShowUsageLimits={() => undefined}
        onHeightChange={props.onHeightChange}
      />
    </ComposerPopoverHost>
  );
}
