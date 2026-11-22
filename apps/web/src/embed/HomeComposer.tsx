import {
  cloudChatProviders,
  cloudChatSelection,
  type CloudChatAttachment,
  type CloudChatModel,
  type LocalChatModel,
} from "@hanzodev/client-runtime/cloud-chat";
import { EnvironmentId, ProjectId, ThreadId, type ModelSelection } from "@hanzodev/contracts";
import { scopeThreadRef } from "@hanzodev/client-runtime/environment";
import { useAtomValue } from "@effect/atom-react";
import { useRef, type ReactNode } from "react";

import { ChatComposer, type ChatComposerHandle } from "../components/chat/ChatComposer";
import { ComposerSurface } from "../components/chat/ComposerSurface";
import { useEnvironmentSettings } from "../hooks/useSettings";
import { useStableCallback } from "../hooks/useStableCallback";
import { useTheme } from "../hooks/useTheme";
import { DraftId, useComposerDraftStore } from "../composerDraftStore";
import { readAttachments } from "./homeAttachments";
import { primaryServerKeybindingsAtom } from "../state/server";
import type { ComposerImageAttachment, ComposerFileAttachment } from "../composerDraftStore";
import type { TerminalContextDraft } from "../lib/terminalContext";

const CLOUD_ENVIRONMENT = EnvironmentId.make("cloud");
const HOME_DRAFT = DraftId.make("dev-home");
const HOME_THREAD = ThreadId.make("dev-home");
const NONE: readonly never[] = [];
const noop = () => {};

/**
 * Dev's own composer (ChatComposer: the model picker with the Enso ring, the "+" menu, voice,
 * attachments, @ $ / ) over no environment: the providers are the cloud catalog (text-output
 * models only) plus the linked Mac's local models, and a send goes to the cloud chat controller.
 */
export function HomeComposer(props: {
  models: ReadonlyArray<CloudChatModel>;
  localModels: ReadonlyArray<LocalChatModel>;
  model: string;
  effort: string | null;
  sending: boolean;
  disabledReason: string | null;
  onSelect: (model: string, effort: string | null) => void;
  onSend: (prompt: string, attachments: ReadonlyArray<CloudChatAttachment>) => void;
  /** Optional voice conversation control placed inside the composer footer beside the mic. */
  voiceControl?: ReactNode;
  /** Triggered when `/private` is chosen from the slash menu. */
  onTogglePrivate?: () => void;
  /** Triggered when `/persona` is chosen from the slash menu. */
  onOpenPersona?: () => void;
  /** Said under the composer while a send is being prepared (files uploading and being read). */
  preparing?: string | null | undefined;
}) {
  const settings = useEnvironmentSettings(CLOUD_ENVIRONMENT);
  const keybindings = useAtomValue(primaryServerKeybindingsAtom);
  const { resolvedTheme } = useTheme();
  const composerRef = useRef<ChatComposerHandle | null>(null);
  const promptRef = useRef("");
  const imagesRef = useRef<ComposerImageAttachment[]>([]);
  const filesRef = useRef<ComposerFileAttachment[]>([]);
  const terminalRef = useRef<TerminalContextDraft[]>([]);
  const providers = cloudChatProviders(
    props.models,
    props.localModels.length > 0 ? props.localModels : undefined,
  );
  const selection: ModelSelection = cloudChatSelection(props.models, props.model, props.effort);
  const threadRef = scopeThreadRef(CLOUD_ENVIRONMENT, HOME_THREAD);
  const send = useStableCallback(() => {
    const prompt = promptRef.current.trim();
    if (prompt === "" && imagesRef.current.length === 0 && filesRef.current.length === 0) return;
    const images = imagesRef.current;
    const files = filesRef.current;
    void readAttachments(images, files).then((attachments) => props.onSend(prompt, attachments));
    useComposerDraftStore.getState().clearComposerContent?.(HOME_DRAFT);
    promptRef.current = "";
  });
  const select = useStableCallback((_instance: unknown, model: string) =>
    props.onSelect(model, null),
  );
  const openProviderSetup = useStableCallback((instanceId: unknown) => {
    if (typeof window !== "undefined") {
      window.dispatchEvent(new CustomEvent("hanzo:open-providers", { detail: { instanceId } }));
    }
  });
  return (
    <>
      <ComposerSurface.Shell>
        <ComposerSurface.Host>
          <ChatComposer
            composerRef={composerRef}
            composerDraftTarget={HOME_DRAFT}
            environmentId={CLOUD_ENVIRONMENT}
            canOperateThread
            environmentless
            attachmentUploadsCapabilityKnown
            supportsAttachmentUploads={false}
            supportsQuestionAttachments={false}
            maxFileAttachmentBytes={null}
            routeKind="draft"
            routeThreadRef={threadRef}
            draftId={HOME_DRAFT}
            multipleModelSelections={null}
            supportsMultipleModels={false}
            onMultipleModelSelectionsChange={noop}
            activeThreadId={null}
            activeThread={undefined}
            activeThreadRuns={NONE}
            promptHistoryEntries={NONE}
            forceExpandedOnMobile={false}
            projectSelectionRequired={false}
            phase={props.sending ? "running" : "disconnected"}
            canSteer={false}
            canInterrupt={false}
            isSendBusy={props.sending}
            canResume={false}
            sendDisabledReason={props.disabledReason}
            isPreparingWorktree={false}
            bannerItems={NONE}
            resumeCompactionTokens={null}
            keepFullHistory={false}
            onToggleKeepFullHistory={noop}
            environmentUnavailable={null}
            activePendingApproval={null}
            pendingApprovals={[]}
            onHeldRequestIdsChange={noop}
            pendingUserInputs={[]}
            activePendingIsResponding={false}
            respondingRequestIds={[]}
            runtimeMode={"approval-required" as never}
            lockedProvider={null}
            providerStatuses={providers as never}
            providerCatalogKnown
            activeProjectDefaultModelSelection={selection}
            activeThreadModelSelection={selection}
            activeProjectId={null as ProjectId | null}
            compactThreadUnavailable
            compactDisabled
            resolvedTheme={resolvedTheme}
            settings={settings}
            keybindings={keybindings}
            getShortcutContext={() => ({
              terminalFocus: false,
              terminalOpen: false,
              previewFocus: false,
              previewOpen: false,
              isWeb: true,
              isDesktop: false,
              editableFocus: true,
              modelPickerOpen: composerRef.current?.isModelPickerOpen() ?? false,
            })}
            terminalOpen={false}
            gitCwd={null}
            pullRequestProjectId={null}
            pullRequestRepository={null}
            threadPullRequests={undefined}
            onComposerOverlayHeightChange={noop}
            permissionsShownInStrip
            promptRef={promptRef}
            composerImagesRef={imagesRef}
            composerFilesRef={filesRef}
            composerTerminalContextsRef={terminalRef}
            onPageScrollKeyDown={noop}
            onPageScrollKeyUp={noop}
            onPageScrollRelease={noop}
            editingQueuedAttachments={null}
            onRemoveEditingQueuedAttachment={noop}
            onCompactContext={noop}
            onSend={send}
            onResume={noop}
            onInterrupt={noop}
            onSelectActivePendingUserInputOption={noop}
            onAdvanceActivePendingUserInput={noop}
            onDismissActivePendingUserInput={noop}
            onPreviousActivePendingUserInputQuestion={noop}
            onChangeActivePendingUserInputCustomAnswer={noop}
            onProviderModelSelect={select as never}
            onOpenProviderSetup={openProviderSetup as never}
            getModelDisabledReason={() => null}
            handleRuntimeModeChange={noop}
            focusComposer={() => composerRef.current?.focusAtEnd()}
            scheduleComposerFocus={() => composerRef.current?.focusAtEnd()}
            setThreadError={noop}
            onExpandImage={noop}
            onFileOpen={noop}
            extraPrimaryActions={props.voiceControl}
            {...(props.onTogglePrivate ? { onTogglePrivate: props.onTogglePrivate } : {})}
            {...(props.onOpenPersona ? { onOpenPersona: props.onOpenPersona } : {})}
          />
        </ComposerSurface.Host>
      </ComposerSurface.Shell>
      {props.preparing ? (
        <div
          className="flex items-center gap-2 px-1 pt-1 text-xs text-muted-foreground"
          data-testid="composer-extras"
        >
          <span role="status">{props.preparing}</span>
        </div>
      ) : null}
    </>
  );
}
