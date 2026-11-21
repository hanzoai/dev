import { CheckIcon, CloudIcon, LaptopIcon } from "lucide-react";
import { useEffect, useMemo, useState, type ReactNode } from "react";

import ChatMarkdown from "../components/ChatMarkdown";
import { Menu, MenuItem, MenuPopup, MenuTrigger } from "../components/ui/menu";
import { I18nProvider } from "../i18n/I18nProvider";
import { startI18n, whenLocaleReady } from "../i18n/runtime";
import { AppAtomRegistryProvider } from "../rpc/atomRegistry";
import { HomeComposer } from "./HomeComposer";
import { useHomeChat } from "./homeChat";
import type { DevCloud, DevEnvironment } from "./runtime";

let ready: Promise<unknown> | null = null;
const ensure = () => (ready ??= Promise.resolve(startI18n()).then(whenLocaleReady));

export interface DevHomeProps {
  cloud: DevCloud;
  mode: "chat" | "dev";
  environments: readonly DevEnvironment[];
  environment: string | null;
  onEnvironment: (id: string) => void;
  onHandoff?: (environment: DevEnvironment, prompt: string) => void;
  conversation?: string | null;
  onConversation?: (id: string | null) => void;
  className?: string;
}

function EnvironmentChip(props: {
  environments: readonly DevEnvironment[];
  environment: string | null;
  onEnvironment: (id: string) => void;
}) {
  const chosen = props.environments.find((e) => e.id === props.environment) ?? props.environments[0];
  if (!chosen) return null;
  return (
    <Menu>
      <MenuTrigger
        data-testid="environment-chip"
        className="inline-flex items-center gap-1.5 rounded-md px-2 py-1 text-xs text-muted-foreground hover:bg-accent"
      >
        {chosen.kind === "cloud" ? <CloudIcon className="size-3.5" /> : <LaptopIcon className="size-3.5" />}
        {chosen.label}
        {!chosen.available && chosen.note ? <span className="opacity-60">· {chosen.note}</span> : null}
      </MenuTrigger>
      <MenuPopup align="start" side="top">
        {props.environments.map((e) => (
          <MenuItem key={e.id} onClick={() => props.onEnvironment(e.id)}>
            {e.kind === "cloud" ? <CloudIcon className="size-4" /> : <LaptopIcon className="size-4" />}
            <span>{e.label}</span>
            {!e.available && e.note ? <span className="ml-auto text-xs opacity-60">{e.note}</span> : null}
            {e.id === chosen.id ? <CheckIcon className="ml-2 size-4" /> : null}
          </MenuItem>
        ))}
      </MenuPopup>
    </Menu>
  );
}

function Home(props: DevHomeProps): ReactNode {
  const machine = props.environments.find((e) => e.id === props.environment && e.kind === "machine") ?? null;
  const chat = useHomeChat(props.cloud, machine);
  const { state, controller } = chat;
  const { onConversation, conversation } = props;
  useEffect(() => {
    if (conversation && conversation !== state.threadId) void controller.open(conversation);
    if (!conversation && state.threadId === null && state.messages.length > 0 && !state.sending) controller.reset();
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- follows the host's address only.
  }, [conversation]);
  useEffect(() => {
    if (state.threadId && state.threadId !== conversation) onConversation?.(state.threadId);
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- reports a new thread once.
  }, [state.threadId]);
  const error = state.error
    ? state.error === "offline"
      ? "The linked Mac is not reachable."
      : state.error === "signed-out" || state.error === "unauthorized"
        ? "Sign in to chat."
        : "That did not go through. Try again."
    : null;
  const empty = state.messages.length === 0;
  return (
    <div className={`flex min-h-0 min-w-0 flex-1 flex-col ${props.className ?? ""}`} data-dev-home="">
      <div className="min-h-0 flex-1 overflow-y-auto">
        {empty ? (
          <div className="flex h-full items-end justify-center pb-6">
            <h1 className="text-2xl font-medium text-foreground">Build what&apos;s next.</h1>
          </div>
        ) : (
          <div className="mx-auto flex w-full max-w-3xl flex-col gap-4 px-4 py-6">
            {state.messages.map((m, i) => (
              <div key={i} className={m.role === "user" ? "self-end rounded-2xl bg-accent px-4 py-2" : ""}>
                {m.role === "user" ? m.content : <ChatMarkdown text={m.content} cwd={undefined} />}
              </div>
            ))}
          </div>
        )}
      </div>
      <div className="mx-auto w-full max-w-3xl px-4 pb-4">
        {error ? <p role="alert" className="pb-2 text-sm text-destructive">{error}</p> : null}
        <HomeComposer
          models={state.models}
          localModels={state.localModels}
          model={state.model}
          effort={state.effort}
          sending={state.sending}
          disabledReason={null}
          onSelect={(model, effort) => controller.selectModel(model, effort)}
          onSend={(prompt, attachments) => {
            if (machine && props.onHandoff && props.mode === "dev" && !chat.localSelected) {
              props.onHandoff(machine, prompt);
              return;
            }
            void controller.send(prompt, attachments);
          }}
        />
        {props.mode === "dev" ? (
          <div className="flex items-center gap-2 pt-1">
            <EnvironmentChip
              environments={props.environments}
              environment={props.environment}
              onEnvironment={props.onEnvironment}
            />
          </div>
        ) : null}
      </div>
    </div>
  );
}

/**
 * The composer-first surface for /dev and /chat. It runs with no environment: a turn in Cloud is a
 * chat completion on the person's account, saved to their cloud conversations; a turn on a linked
 * Mac's local model goes to that Mac; a Dev turn on a Mac is handed to the host.
 */
export function DevHome(props: DevHomeProps): ReactNode {
  const [loaded, setLoaded] = useState(false);
  useEffect(() => {
    void ensure().then(() => setLoaded(true));
  }, []);
  const stable = useMemo(() => props, [props]);
  if (!loaded) return null;
  return (
    <I18nProvider>
      <AppAtomRegistryProvider>
        <Home {...stable} />
      </AppAtomRegistryProvider>
    </I18nProvider>
  );
}
