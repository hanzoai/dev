import { useCloudChat } from "../cloud/cloudChat";
import { useAuth } from "../cloud/hanzoWebAuth";
import type { EnvironmentId } from "@hanzodev/contracts";
import { Link, useRouter } from "@tanstack/react-router";
import { T, useGT } from "gt-react";
import { ArrowUpIcon, ChevronDownIcon, HammerIcon } from "lucide-react";
import { useState } from "react";

import { APP_DISPLAY_NAME } from "~/branding";
import { hasCloudPublicConfig } from "~/cloud/publicConfig";
import { useEnvironmentDiscovery } from "~/cloud/environmentDiscovery";
import { isElectron } from "../env";
import { isLocalEnvironmentDisabled } from "../localEnvironment";
import { useRelayEnvironmentDiscovery } from "../state/environments";
import { useHanzoConnectAuthPrompt } from "./account/useHanzoConnectAuthPrompt";
import { CloudEnvironmentConnectRows } from "./cloud/CloudEnvironmentConnectList";
import { cloudRunNote, hostedLanding, modelPillLabel, resolveSend } from "./chatLanding.logic";
import { Button } from "./ui/button";
import { Empty, EmptyDescription, EmptyHeader, EmptyTitle } from "./ui/empty";
import { Popover, PopoverPopup, PopoverTrigger } from "./ui/popover";
import { SidebarInset } from "./ui/sidebar";
import { WorkspacePageHeader } from "./WorkspacePageHeader";

/** Home screen for the hosted app while it has no environment: sign in, or chat. */
export function HostedEmptyState() {
  const localEnvironmentOff = isLocalEnvironmentDisabled();
  const cloudEnabled = hasCloudPublicConfig();

  return (
    <SidebarInset className="h-dvh min-h-0 overflow-hidden overscroll-y-none">
      <div className="flex min-h-0 min-w-0 flex-1 flex-col overflow-x-hidden bg-background">
        <WorkspacePageHeader
          electron={isElectron}
          className="max-md:invisible md:border-b md:border-border"
        >
          <div className="flex items-center gap-2">
            <span className="text-sm font-medium text-foreground md:text-muted-foreground/60">
              {APP_DISPLAY_NAME}
            </span>
          </div>
        </WorkspacePageHeader>

        {localEnvironmentOff ? (
          <Empty className="flex-1">
            <EmptyHeader>
              <EmptyTitle>
                <T>This computer is off</T>
              </EmptyTitle>
              <EmptyDescription>
                <T>Turn it back on, or connect a different computer.</T>
              </EmptyDescription>
            </EmptyHeader>
            <Button render={<Link to="/settings/connections" />} size="sm">
              <T>Open Environments</T>
            </Button>
          </Empty>
        ) : cloudEnabled ? (
          <HostedCloudLanding />
        ) : (
          <Empty className="flex-1">
            <EmptyHeader>
              <EmptyTitle>
                <T>No computer connected</T>
              </EmptyTitle>
              <EmptyDescription>
                <T>Pair a computer from Settings.</T>
              </EmptyDescription>
            </EmptyHeader>
            <Button render={<Link to="/settings/connections" />} size="sm">
              <T>Open Environments</T>
            </Button>
          </Empty>
        )}
      </div>
    </SidebarInset>
  );
}

function HostedCloudLanding() {
  const { isLoaded, isSignedIn } = useAuth({ treatPendingAsSignedOut: false });
  const landing = hostedLanding({ isLoaded, isSignedIn });
  if (landing === "loading") return null;
  return landing === "sign-in" ? <SignInLanding /> : <ChatLanding />;
}

function SignInLanding() {
  const { openAuthPrompt } = useHanzoConnectAuthPrompt();
  return (
    <Empty className="flex-1">
      <Button size="lg" onClick={openAuthPrompt}>
        <T>Sign in to Hanzo</T>
      </Button>
    </Empty>
  );
}

/**
 * Signed-in landing: a composer and the model, no environment gate. A machine is asked for only
 * to build. Signing in links the account's computers, so any that appear are added unseen.
 */
function ChatLanding() {
  const gt = useGT();
  const router = useRouter();
  const discovery = useEnvironmentDiscovery();
  const [prompt, setPrompt] = useState("");
  const [choosing, setChoosing] = useState(false);
  const [cloudResult, setCloudResult] = useState<Awaited<
    ReturnType<ReturnType<typeof useCloudChat>["runInCloud"]>
  > | null>(null);
  const [modelsOpen, setModelsOpen] = useState(false);
  const chat = useCloudChat();
  const { messages, models, model, sending, error } = chat.state;
  const cloudNote = cloudRunNote(cloudResult);

  const send = (build: boolean) => {
    const outcome = resolveSend({
      prompt,
      build,
      hasReadyEnvironment: false,
      cloudChatAvailable: true,
    });
    setCloudResult(null);
    setChoosing(outcome.kind === "choose-target");
    if (outcome.kind === "chat") {
      setPrompt("");
      void chat.send(prompt);
    }
  };

  const linkedMac = discovery?.environments.find((environment) => environment.type === "linked");

  return (
    <>
      <LinkAccountComputers />
      <div
        className={`scrollbar-gutter-both ${
          messages.length > 0
            ? "flex min-h-0 flex-1 flex-col gap-3 overflow-y-auto px-4 py-4"
            : "flex min-h-0 flex-1 flex-col items-center justify-center px-6"
        }`}
        data-testid="chat-thread"
      >
        {messages.length === 0 ? (
          <h1 className="text-center text-2xl font-semibold text-foreground">
            <T>What are we building?</T>
          </h1>
        ) : (
          messages.map((message, index) => (
            <div
              className={
                message.role === "user"
                  ? "ml-auto max-w-[85%] whitespace-pre-wrap rounded-2xl bg-muted px-3 py-2 text-sm"
                  : "mr-auto max-w-[95%] whitespace-pre-wrap px-1 py-1 text-sm"
              }
              data-role={message.role}
              // oxlint-disable-next-line react/no-array-index-key -- chat messages carry no id, and these text rows hold no state
              key={index}
            >
              {message.content === "" && sending ? "…" : message.content}
            </div>
          ))
        )}
        {error !== null ? (
          <p className="text-center text-xs text-destructive" role="alert">
            {error === "signed-out" || error === "unauthorized" ? (
              <T>Sign in again to keep chatting.</T>
            ) : (
              <T>Could not reach Hanzo. Try again.</T>
            )}
          </p>
        ) : null}
        {choosing ? (
          <div
            className="mt-6 flex w-full max-w-sm flex-col gap-2 rounded-2xl border border-border p-4"
            data-testid="run-target"
          >
            <p className="text-sm font-medium">
              <T>Where should this run?</T>
            </p>
            <p className="text-xs text-muted-foreground">
              <T>Building needs a machine.</T>
            </p>
            <Button
              disabled={linkedMac !== undefined}
              onClick={() => void router.navigate({ to: "/settings/connections" })}
              size="sm"
            >
              {linkedMac ? linkedMac.label : <T>Run on your Mac</T>}
            </Button>
            <Button
              disabled={discovery?.cloud.status === "unavailable"}
              onClick={() => void chat.runInCloud().then(setCloudResult)}
              size="sm"
              variant="outline"
            >
              <T>Cloud</T>
            </Button>
            {cloudNote === "unavailable" || discovery?.cloud.status === "unavailable" ? (
              <p className="text-xs text-muted-foreground">
                <T>Not available yet</T>
              </p>
            ) : cloudNote === "failed" ? (
              <p className="text-xs text-muted-foreground">
                <T>Could not start a cloud machine. Try again.</T>
              </p>
            ) : cloudNote === "started" ? (
              <p className="text-xs text-muted-foreground">
                <T>Cloud machine started.</T>
              </p>
            ) : null}
          </div>
        ) : null}
      </div>
      <div className="px-3 pb-safe">
        <div className="mx-auto flex w-full max-w-3xl flex-col gap-2 rounded-3xl border border-border bg-muted/30 p-3">
          <textarea
            aria-label={gt("Message")}
            className="max-h-40 min-h-9 resize-none bg-transparent px-2 text-base outline-none placeholder:text-muted-foreground"
            onChange={(event) => setPrompt(event.currentTarget.value)}
            placeholder={gt("Ask anything")}
            rows={1}
            value={prompt}
          />
          <div className="flex items-center gap-2">
            <Popover onOpenChange={setModelsOpen} open={modelsOpen}>
              <PopoverTrigger
                render={<Button aria-label={gt("Model")} size="sm" variant="outline" />}
              >
                {modelPillLabel(models, model)}
                <ChevronDownIcon />
              </PopoverTrigger>
              <PopoverPopup align="start" side="top" width="sm">
                <ul className="flex flex-col gap-1" data-testid="model-list">
                  {models.map((entry) => (
                    <li key={entry.id}>
                      <button
                        aria-pressed={entry.id === model}
                        className="w-full rounded-md px-2 py-1.5 text-left text-sm hover:bg-accent aria-pressed:font-medium"
                        onClick={() => {
                          chat.selectModel(entry.id);
                          setModelsOpen(false);
                        }}
                        type="button"
                      >
                        {entry.label}
                      </button>
                    </li>
                  ))}
                </ul>
              </PopoverPopup>
            </Popover>
            <Button
              aria-label={gt("Build on a machine")}
              onClick={() => send(true)}
              size="sm"
              variant="outline"
            >
              <HammerIcon />
              <T>Build</T>
            </Button>
            <span className="flex-1" />
            <Button
              aria-label={gt("Send")}
              disabled={sending}
              onClick={() => send(false)}
              size="icon"
              variant="default"
            >
              <ArrowUpIcon />
            </Button>
          </div>
        </div>
      </div>
    </>
  );
}

/** Adds the computers the account has linked, without showing a list. */
function LinkAccountComputers() {
  // Adding the first computer unmounts the landing. Wait for all health checks
  // so every compatible registration is queued before that happens.
  const [autoAddedComputers] = useState(() => new Set<EnvironmentId>());
  const [selectedIds, setSelectedIds] = useState<ReadonlySet<EnvironmentId>>(() => new Set());
  const discovered = [...useRelayEnvironmentDiscovery().environments.values()];
  const discoverySettled =
    discovered.length > 0 && discovered.every(({ availability }) => availability !== "checking");
  return (
    <div hidden>
      <CloudEnvironmentConnectRows
        primaryEnvironmentId={null}
        savedEnvironments={[]}
        refreshWhileEmpty
        selection={{
          ...(discoverySettled ? { autoSelectedComputers: autoAddedComputers } : {}),
          selectedIds,
          onChange: (environmentId, selected) =>
            setSelectedIds((current) => {
              const next = new Set(current);
              if (selected) next.add(environmentId);
              else next.delete(environmentId);
              return next;
            }),
        }}
      />
    </div>
  );
}
