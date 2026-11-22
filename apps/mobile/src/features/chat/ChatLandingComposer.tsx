import { CLOUD_ENVIRONMENT } from "./cloud-environment";
import {
  cloudChatAcceptsInput,
  cloudChatSelection,
  type CloudChatAttachment,
} from "@hanzodev/client-runtime/cloud-chat";
import { MessageId, ProjectId, ThreadId } from "@hanzodev/contracts";
import * as Clipboard from "expo-clipboard";
import { useGT } from "gt-react-native";
import { useEffect, useEffectEvent } from "react";
import { useSharedValue } from "react-native-reanimated";

import * as Dialog from "../../components/ui/Dialog";
import { useGlobalVoiceInput } from "../voice-input/VoiceInputProvider";
import { createVoiceInputTarget } from "../voice-input/voiceInputSession";
import {
  consumePendingWidgetQuickAction,
  subscribeWidgetQuickAction,
  type WidgetQuickAction,
} from "./widgetQuickAction";
import { useComposerAttachmentActions } from "../../state/use-composer-attachment-actions";
import {
  clearComposerDraftContent,
  getComposerDraftSnapshot,
  removeComposerDraftAttachment,
  setComposerDraftText,
} from "../../state/use-composer-drafts";
import { ComposerPopoverHost } from "../threads/ComposerPopoverHost";
import { ThreadComposer } from "../threads/ThreadComposer";
import { draftThreadShell } from "../threads/draft-thread-shell";
import { CHAT_LANDING_DRAFT_KEY } from "./chat-landing.logic";
import {
  CLOUD_CHAT_FILE_BYTES,
  cloudAttachmentKind,
  readCloudAttachments,
} from "./cloud-attachments";
import type { CloudChat } from "./cloud-chat";

// Stand-ins for the thread the composer expects. No environment owns them, so every
// environment-keyed lookup the composer makes finds nothing and falls back.
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
  readonly onHeightChange: (height: number) => void;
  /** Sends the draft as a chat; false when it cannot be sent as one. */
  readonly onSend: (prompt: string, attachments: ReadonlyArray<CloudChatAttachment>) => boolean;
}) {
  const gt = useGT();
  const popoverHeight = useSharedValue(0);
  const { chat } = props;
  const attachments = useComposerAttachmentActions({
    draftKey: CHAT_LANDING_DRAFT_KEY,
    serverConfig: chat.serverConfig,
    maxFileBytes: CLOUD_CHAT_FILE_BYTES,
  });
  const globalVoice = useGlobalVoiceInput();
  const handleWidgetAction = useEffectEvent((act: WidgetQuickAction) => {
    if (act.threadId) {
      const targetId = act.threadId;
      const matched = chat.state.threads.find(
        (thread) => thread.key === targetId || thread.remoteId === targetId,
      );
      if (matched) {
        void chat.openThread(matched.key);
      } else {
        void chat.openRemoteThread(targetId);
      }
    } else if (act.action === "new") {
      chat.newChat();
    }
    if (act.prompt) {
      if (act.autoSend) {
        props.onSend(act.prompt, []);
      } else {
        setComposerDraftText(CHAT_LANDING_DRAFT_KEY, act.prompt);
      }
    }
    if (act.action === "voice") {
      const currentText = getComposerDraftSnapshot(CHAT_LANDING_DRAFT_KEY).text;
      void globalVoice.session.start({
        ...createVoiceInputTarget(
          CHAT_LANDING_DRAFT_KEY,
          () => getComposerDraftSnapshot(CHAT_LANDING_DRAFT_KEY).text,
          (text) => setComposerDraftText(CHAT_LANDING_DRAFT_KEY, text),
          { start: currentText.length, end: currentText.length },
        ),
        label: gt("Hanzo Chat"),
      });
    } else if (act.action === "camera") {
      void attachments.onPickDraftMedia();
    } else if (act.action === "attach") {
      void attachments.onPickDraftFiles();
    } else if (act.action === "clipboard") {
      void Clipboard.getStringAsync()
        .then((clip) => {
          const text = clip.trim();
          if (text) {
            setComposerDraftText(CHAT_LANDING_DRAFT_KEY, text);
          }
        })
        .catch(() => {});
    }
  });
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- listens for Home Screen widget quick actions (deep links) while the landing composer is mounted
  useEffect(() => {
    const initial = consumePendingWidgetQuickAction();
    if (initial) {
      handleWidgetAction(initial);
    }
    return subscribeWidgetQuickAction((act) => {
      consumePendingWidgetQuickAction();
      handleWidgetAction(act);
    });
  }, []);
  const thread = draftThreadShell({
    environmentId: CLOUD_ENVIRONMENT,
    threadId: CLOUD_THREAD,
    projectId: CLOUD_PROJECT,
    modelSelection: cloudChatSelection(chat.models, chat.state.model, chat.state.effort),
    runtimeMode: "full-access",
    interactionMode: "default",
    branch: null,
    worktreePath: null,
    createdAt: CREATED_AT,
  });
  const send = async () => {
    const current = getComposerDraftSnapshot(CHAT_LANDING_DRAFT_KEY);
    const model = chat.models.find((candidate) => candidate.id === chat.state.model);
    const kinds = new Set(current.attachments.map(cloudAttachmentKind));
    // Say plainly what this model cannot read, instead of sending and getting a refusal.
    if (kinds.has("other")) {
      Dialog.alert(
        gt("Can't send that file in a chat"),
        gt("Chat reads images, PDFs and text files. Remove it, or tap Build to run on a machine."),
      );
      return null;
    }
    if (kinds.has("image") && !cloudChatAcceptsInput(model, "image")) {
      Dialog.alert(
        gt("This model can't read images"),
        gt("Choose another model, or remove the image."),
      );
      return null;
    }
    if (kinds.has("pdf") && !cloudChatAcceptsInput(model, "file")) {
      Dialog.alert(
        gt("This model can't read PDFs"),
        gt("Choose another model, or remove the file."),
      );
      return null;
    }
    const files = await readCloudAttachments(current.attachments).catch(() => null);
    if (files === null) {
      Dialog.alert(gt("Couldn't read an attachment"), gt("Remove it and try again."));
      return null;
    }
    const text =
      current.text.trim() === "" && files.length > 0 ? gt("See attached.") : current.text;
    if (!props.onSend(text, files)) return null;
    clearComposerDraftContent(CHAT_LANDING_DRAFT_KEY);
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
