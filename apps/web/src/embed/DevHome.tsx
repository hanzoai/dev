import "./embedded";
import { said } from "@hanzodev/client-runtime/cloud-chat";
import { msg, T, useGT, useMessages, Var } from "gt-react";
import { CheckIcon, CloudIcon, LaptopIcon, LockIcon, UserRoundIcon, XIcon } from "lucide-react";
import { useEffect, useState, type ReactNode } from "react";

import ChatMarkdown from "../components/ChatMarkdown";
import { Menu, MenuItem, MenuPopup, MenuTrigger } from "../components/ui/menu";
import { I18nProvider } from "../i18n/I18nProvider";
import { m, type MessageResolver } from "../i18n/messages";
import { startI18n, whenLocaleReady } from "../i18n/runtime";
import { AppAtomRegistryProvider } from "../rpc/atomRegistry";
import { HomeComposer } from "./HomeComposer";
import { threadFor } from "./chatOrder";
import { TalkMode } from "./TalkMode";
import { useHomeChat } from "./homeChat";
import { prepareTurn } from "./homeSend";
import type { DevCloud, DevEnvironment } from "./runtime";

let ready: Promise<unknown> | null = null;
const ensure = () => (ready ??= Promise.resolve(startI18n()).then(whenLocaleReady));

const CHAT_RECOMMENDATIONS: ReadonlyArray<{ readonly label: string; readonly prompt: string }> = [
  {
    label: msg("Brainstorm ideas"),
    prompt: "Help me brainstorm practical architecture options for ",
  },
  {
    label: msg("Analyze code"),
    prompt: "Review this code for bugs, edge cases, and performance improvements: ",
  },
  { label: msg("Draft & refine"), prompt: "Write a clear, concise draft for " },
  { label: msg("Research a topic"), prompt: "Give me a crisp technical breakdown of " },
];

const DEV_RECOMMENDATIONS: ReadonlyArray<{ readonly label: string; readonly prompt: string }> = [
  {
    label: msg("Review git diff"),
    prompt: "Review my latest changes and flag any risks or regressions.",
  },
  {
    label: msg("Fix failing tests"),
    prompt: "Run the test suite, diagnose any failures, and fix them.",
  },
  { label: msg("Build a feature"), prompt: "Design and implement a clean solution for " },
  {
    label: msg("Refactor module"),
    prompt: "Simplify and refactor this module without changing behavior.",
  },
];

function readSignedInFirstName(): string | null {
  try {
    const winName = (window as unknown as { __hanzoUserName?: string }).__hanzoUserName;
    if (typeof winName === "string" && winName.trim()) {
      const first = winName.trim().split(/\s+/)[0];
      if (first) return first;
    }
    const rawUser = window.localStorage.getItem("hanzo_iam_user");
    if (rawUser) {
      const parsed = JSON.parse(rawUser) as Record<string, unknown>;
      const full =
        (typeof parsed.displayName === "string" && parsed.displayName) ||
        (typeof parsed.given_name === "string" && parsed.given_name) ||
        (typeof parsed.name === "string" && parsed.name) ||
        "";
      const first = full.trim().split(/\s+/)[0];
      if (first) return first;
    }
    const token = window.localStorage.getItem("hanzo_iam_access_token");
    if (token) {
      const part = token.split(".")[1];
      if (part) {
        const b64 = part.replace(/-/g, "+").replace(/_/g, "/");
        const padded = b64 + "=".repeat((4 - (b64.length % 4)) % 4);
        const claims = JSON.parse(atob(padded)) as Record<string, unknown>;
        const full =
          (typeof claims.given_name === "string" && claims.given_name) ||
          (typeof claims.name === "string" && claims.name) ||
          (typeof claims.displayName === "string" && claims.displayName) ||
          "";
        const first = full.trim().split(/\s+/)[0];
        if (first) return first;
      }
    }
  } catch {
    // ignore storage or JWT parse errors
  }
  return null;
}

function dynamicGreeting(
  mode: "chat" | "dev",
  firstName: string | null,
  resolve: MessageResolver = m,
): string {
  const hour = new Date().getHours();
  if (mode === "dev") {
    if (!firstName) return resolve(msg("Build what's next."));
    return resolve(
      hour < 12
        ? msg("Good morning, {name}. What are we building?", { name: firstName })
        : hour < 18
          ? msg("Good afternoon, {name}. What are we building?", { name: firstName })
          : msg("Good evening, {name}. What are we building?", { name: firstName }),
    );
  }
  if (!firstName) return resolve(msg("What's next?"));
  return resolve(
    hour < 12
      ? msg("Good morning, {name}.", { name: firstName })
      : hour < 18
        ? msg("Good afternoon, {name}.", { name: firstName })
        : msg("Good evening, {name}.", { name: firstName }),
  );
}

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
  const available = props.environments.filter((e) => e.available);
  if (available.length === 0) return null;
  const chosen = available.find((e) => e.id === props.environment) ?? available[0];
  if (!chosen) return null;
  return (
    <Menu>
      <MenuTrigger
        data-testid="environment-chip"
        // oxlint-disable-next-line shadcn/no-restyle -- MenuTrigger draws a bare button here; this borderless text chip is the embed composer's own look, not a menu-trigger variant
        className="inline-flex items-center gap-1.5 rounded-md px-2 py-1 text-xs text-muted-foreground hover:bg-accent"
      >
        {chosen.kind === "cloud" ? (
          <CloudIcon className="size-3.5" />
        ) : (
          <LaptopIcon className="size-3.5" />
        )}
        {chosen.label}
      </MenuTrigger>
      <MenuPopup align="start" side="top">
        {available.map((e) => (
          <MenuItem key={e.id} onClick={() => props.onEnvironment(e.id)}>
            {e.kind === "cloud" ? (
              <CloudIcon className="size-4" />
            ) : (
              <LaptopIcon className="size-4" />
            )}
            <span>{e.label}</span>
            {e.id === chosen.id ? <CheckIcon className="ml-2 size-4" /> : null}
          </MenuItem>
        ))}
      </MenuPopup>
    </Menu>
  );
}

function Home(props: DevHomeProps): ReactNode {
  const [preparing, setPreparing] = useState<string | null>(null);
  const [firstName, setFirstName] = useState<string | null>(() => readSignedInFirstName());
  const [personaMenuOpen, setPersonaMenuOpen] = useState(false);
  const gt = useGT();
  const resolve = useMessages();
  const machine =
    props.environments.find((e) => e.id === props.environment && e.kind === "machine") ?? null;
  const chat = useHomeChat(props.cloud, machine);
  const { state, controller } = chat;
  const { onConversation, conversation } = props;

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- listens for the host's hanzo:user-updated event and re-reads the signed-in name from storage
  useEffect(() => {
    const syncName = () => setFirstName(readSignedInFirstName());
    syncName();
    window.addEventListener("hanzo:user-updated", syncName);
    return () => window.removeEventListener("hanzo:user-updated", syncName);
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- a new cloud session from the host (a sign-in) is when the stored name changes.
  }, [props.cloud]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- listens for the host's hanzo:new-chat event to close the picker and start a fresh conversation
  useEffect(() => {
    const onNewChat = () => {
      setPersonaMenuOpen(false);
      controller.reset();
    };
    window.addEventListener("hanzo:new-chat", onNewChat);
    return () => window.removeEventListener("hanzo:new-chat", onNewChat);
  }, [controller]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- holds a window keydown listener that closes the persona picker on Escape while it is open
  useEffect(() => {
    if (!personaMenuOpen) return;
    const onKey = (e: KeyboardEvent) => {
      if (e.key === "Escape") setPersonaMenuOpen(false);
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [personaMenuOpen]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- drives the shared cloud-chat controller to the conversation the host's address names
  useEffect(() => {
    const known = threadFor(state.threads, conversation ?? null);
    if (conversation && conversation !== state.threadId && conversation !== state.threadKey) {
      if (known !== null) void controller.openThread(known.key);
      else void controller.open(conversation);
    }
    if (!conversation && state.threadId === null && state.messages.length > 0 && !state.sending)
      controller.reset();
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- follows the host's address only.
  }, [conversation]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- tells the host's router the controller's new thread so the address follows the conversation
  useEffect(() => {
    const ref = state.threadId ?? state.threadKey;
    if (ref && ref !== conversation && state.messages.length > 0) onConversation?.(ref);
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- reports a new thread once.
  }, [state.threadId, state.threadKey]);

  const error = state.error
    ? state.error === "offline"
      ? "The linked Mac is not reachable."
      : state.error === "signed-out" || state.error === "unauthorized"
        ? "Sign in to chat."
        : "That did not go through. Try again."
    : null;

  const empty = state.messages.length === 0;
  const greeting = dynamicGreeting(props.mode, firstName, resolve);
  const recommendations = props.mode === "dev" ? DEV_RECOMMENDATIONS : CHAT_RECOMMENDATIONS;

  const handleSend = (
    rawPrompt: string,
    attachments: Parameters<typeof controller.send>[1] = [],
  ) => {
    const trimmed = rawPrompt.trim();
    if (/^\/private(?:\s|$)/i.test(trimmed)) {
      const rest = trimmed.replace(/^\/private\s*/i, "").trim();
      if (!rest) {
        controller.setGhost(!state.ghost);
        return;
      }
      controller.setGhost(true);
      void sendPrompt(rest, attachments);
      return;
    }
    if (/^\/persona(?:\s|$)/i.test(trimmed)) {
      const arg = trimmed.replace(/^\/persona\s*/i, "").trim();
      if (!arg) {
        setPersonaMenuOpen((open) => !open);
        return;
      }
      if (/^(off|none|clear|reset)$/i.test(arg)) {
        chat.selectPersona(null);
        setPersonaMenuOpen(false);
        return;
      }
      const lower = arg.toLowerCase();
      const matched =
        chat.personas.find((p) => p.name.toLowerCase() === lower) ??
        chat.personas.find((p) => p.name.toLowerCase().startsWith(lower));
      if (matched) {
        chat.selectPersona(matched.name);
        setPersonaMenuOpen(false);
      } else {
        setPersonaMenuOpen(true);
      }
      return;
    }
    void sendPrompt(rawPrompt, attachments);
  };

  const sendPrompt = async (
    prompt: string,
    attachments: Parameters<typeof controller.send>[1] = [],
  ) => {
    if (machine && props.onHandoff && props.mode === "dev" && !chat.localSelected) {
      props.onHandoff(machine, prompt);
      return;
    }
    const auth = { accessToken: await props.cloud.token(), orgId: props.cloud.org ?? null };
    const prepared = chat.localSelected
      ? { prompt, attachments, turn: {} }
      : await prepareTurn({
          auth,
          options: { baseUrl: props.cloud.api },
          prompt,
          attachments,
          history: state.messages,
          persona: await chat.personaRecord(),
          say: setPreparing,
        });
    await controller.send(prepared.prompt, prepared.attachments, prepared.turn);
  };

  const composerBlock = (
    <div className="relative mx-auto w-full max-w-3xl px-4">
      {personaMenuOpen ? (
        <div
          data-testid="persona-command-popover"
          className="mb-2 max-h-64 overflow-y-auto rounded-xl border border-border/60 bg-popover p-1.5 shadow-xl"
        >
          <div className="flex items-center justify-between px-2 py-1 text-2xs font-medium text-muted-foreground">
            <span>
              <T>
                Select persona (<Var>/persona</Var>)
              </T>
            </span>
            <button
              type="button"
              aria-label={gt("Close persona picker")}
              onClick={() => setPersonaMenuOpen(false)}
              className="inline-flex size-5 cursor-pointer items-center justify-center rounded-md hover:bg-accent hover:text-foreground"
            >
              <XIcon className="size-3.5" />
            </button>
          </div>
          <button
            type="button"
            onClick={() => {
              chat.selectPersona(null);
              setPersonaMenuOpen(false);
            }}
            className="flex w-full cursor-pointer items-center gap-2 rounded-lg px-2 py-1.5 text-left text-xs text-foreground hover:bg-accent"
          >
            <span>
              <T>No persona</T>
            </span>
            {chat.persona === null ? <CheckIcon className="ml-auto size-3.5" /> : null}
          </button>
          {chat.personas.map((p) => (
            <button
              key={p.name}
              type="button"
              onClick={() => {
                chat.selectPersona(p.name);
                setPersonaMenuOpen(false);
              }}
              className="flex w-full cursor-pointer items-center gap-2 rounded-lg px-2 py-1.5 text-left text-xs text-foreground hover:bg-accent"
            >
              <span className="shrink-0 font-medium">
                {p.emoji ? `${p.emoji} ` : ""}
                {p.name}
              </span>
              {p.description ? (
                <span className="truncate text-2xs text-muted-foreground">{p.description}</span>
              ) : null}
              {p.name === chat.persona ? <CheckIcon className="ml-auto size-3.5 shrink-0" /> : null}
            </button>
          ))}
          {chat.personas.length === 0 ? (
            <div className="px-2 py-2 text-xs text-muted-foreground">
              <T>Sign in to load your personas.</T>
            </div>
          ) : null}
        </div>
      ) : null}
      {state.ghost || chat.persona ? (
        <div className="mb-2 flex flex-wrap items-center gap-1.5">
          {state.ghost ? (
            <div
              data-testid="private-mode-banner"
              className="inline-flex items-center gap-1.5 rounded-full border border-border/70 bg-muted/40 px-2.5 py-1 text-xs text-muted-foreground"
            >
              <LockIcon className="size-3" />
              <span>
                <T>Private mode · nothing saved or recorded</T>
              </span>
              <button
                type="button"
                aria-label={gt("Exit private mode")}
                onClick={() => controller.setGhost(false)}
                className="ml-0.5 inline-flex size-4 cursor-pointer items-center justify-center rounded-full hover:bg-accent hover:text-foreground"
              >
                <XIcon className="size-3" />
              </button>
            </div>
          ) : null}
          {chat.persona ? (
            <div
              data-testid="active-persona-banner"
              className="inline-flex items-center gap-1.5 rounded-full border border-border/70 bg-muted/40 px-2.5 py-1 text-xs text-muted-foreground"
            >
              <UserRoundIcon className="size-3" />
              <span>{chat.persona}</span>
              <button
                type="button"
                aria-label={gt("Clear persona")}
                onClick={() => chat.selectPersona(null)}
                className="ml-0.5 inline-flex size-4 cursor-pointer items-center justify-center rounded-full hover:bg-accent hover:text-foreground"
              >
                <XIcon className="size-3" />
              </button>
            </div>
          ) : null}
        </div>
      ) : null}
      {error ? (
        <p role="alert" className="pb-2 text-sm text-destructive">
          {error}
        </p>
      ) : null}
      <HomeComposer
        models={state.models}
        localModels={state.localModels}
        model={state.model}
        effort={state.effort}
        sending={state.sending}
        disabledReason={null}
        onSelect={(model, effort) => controller.selectModel(model, effort)}
        preparing={preparing}
        voiceControl={<TalkMode cloud={props.cloud} />}
        onTogglePrivate={() => controller.setGhost(!state.ghost)}
        onOpenPersona={() => setPersonaMenuOpen((open) => !open)}
        onSend={handleSend}
      />
      {props.mode === "dev" && props.environments.some((e) => e.available) ? (
        <div className="flex items-center gap-2 pt-1.5">
          <EnvironmentChip
            environments={props.environments}
            environment={props.environment}
            onEnvironment={props.onEnvironment}
          />
        </div>
      ) : null}
    </div>
  );

  if (empty) {
    return (
      <div
        className={`flex min-h-0 min-w-0 flex-1 flex-col items-center justify-center pb-12 ${props.className ?? ""}`}
        data-dev-home=""
      >
        <h1 className="mb-5 text-center text-2xl font-medium tracking-tight text-foreground">
          {greeting}
        </h1>
        {composerBlock}
        <div
          data-testid="chat-recommendations"
          className="mx-auto mt-4 flex w-full max-w-3xl flex-wrap items-center justify-center gap-2 px-4"
        >
          {recommendations.map((rec) => (
            <button
              key={rec.label}
              type="button"
              onClick={() => handleSend(rec.prompt)}
              className="inline-flex h-8 cursor-pointer items-center rounded-full border border-border/70 bg-transparent px-3.5 text-xs text-muted-foreground transition-colors hover:border-foreground/25 hover:bg-accent/50 hover:text-foreground"
            >
              {resolve(rec.label)}
            </button>
          ))}
        </div>
      </div>
    );
  }

  return (
    <div
      className={`flex min-h-0 min-w-0 flex-1 flex-col ${props.className ?? ""}`}
      data-dev-home=""
    >
      <div className="min-h-0 flex-1 overflow-y-auto scrollbar-gutter-both">
        <div className="mx-auto flex w-full max-w-3xl flex-col gap-4 px-4 py-6">
          {state.messages.map((m, i) => (
            <div
              // oxlint-disable-next-line react/no-array-index-key -- turns carry no id and a conversation only appends them, so a turn keeps its position
              key={i}
              className={m.role === "user" ? "self-end rounded-2xl bg-accent px-4 py-2" : ""}
            >
              {m.role === "user" ? (
                said(m.content).text ||
                said(m.content)
                  .carried?.attached.map((f) => f.name)
                  .join(", ")
              ) : (
                <ChatMarkdown text={m.content} cwd={undefined} />
              )}
            </div>
          ))}
        </div>
      </div>
      <div className="pb-4">{composerBlock}</div>
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
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- starts GT and waits for the locale catalog before drawing Home
  useEffect(() => {
    void ensure().then(() => setLoaded(true));
  }, []);
  if (!loaded) return null;
  return (
    <I18nProvider>
      <AppAtomRegistryProvider>
        <Home {...props} />
      </AppAtomRegistryProvider>
    </I18nProvider>
  );
}
