import type { ReactNode } from "react";

export interface DevRuntime {
  readonly kind: "machine" | "cell";
  readonly id: string;
  readonly label: string;
  readonly zap: string;
  readonly token: () => Promise<string>;
  readonly sandbox?: string;
}

export interface DevRuntimeSource {
  resolve(signal: AbortSignal): Promise<DevRuntime | null>;
  list(signal: AbortSignal): Promise<readonly DevRuntime[]>;
}

export function DevProvider(props: {
  runtime: DevRuntime;
  /** The runtime would not pair: the host may try another. */
  onFail?: (runtime: DevRuntime) => void;
  children?: ReactNode;
}): ReactNode;

export function DevThreadList(props: {
  active?: string | null;
  onOpen?: (ref: string | null) => void;
  /** Show the code-only sidebar icons (source control, issues): /dev yes, /chat no. */
  tools?: boolean;
  className?: string;
}): ReactNode;

export function DevWorkspace(props: {
  thread?: string | null;
  onNavigate?: (ref: string | null) => void;
  dock?: (context: { runtime: DevRuntime; thread: string | null }) => ReactNode;
  className?: string;
}): ReactNode;

export function DevStart(props: {
  onCloud: () => Promise<boolean | void> | void;
  onLink: () => void;
  className?: string;
}): ReactNode;

export function useDevOpen(): (input: {
  /** `github.com/<owner>/<name>` or any clone address. */
  repo: string;
  /** The branch to start on, or "" for the default. */
  ref: string;
}) => Promise<string>;

/** Focuses the composer; false when none is drawn. */
export function focusComposer(): boolean;

/** Focuses the open thread's row (else the first); false when no list is drawn. */
export function focusThreadList(): boolean;

/** Where a turn runs. Cloud is plain chat on the person's account; a machine is a linked Mac. */
export interface DevEnvironment {
  readonly id: string;
  readonly kind: "cloud" | "machine";
  readonly label: string;
  /** False draws the chip disabled with `note` ("Not available yet"); plain chat still works in Cloud. */
  readonly available: boolean;
  readonly note?: string;
  /** A machine's runtime (ZAP endpoint); absent for Cloud. */
  readonly runtime?: DevRuntime;
}

/** The person's cloud account: plain chat needs no environment. */
export interface DevCloud {
  /** `https://api.hanzo.ai` */
  readonly api: string;
  /** The person's Hanzo IAM access token, fresh per call. */
  readonly token: () => Promise<string>;
  /** The person's organization, sent as `X-Org-Id` when set. */
  readonly org?: string;
}

/**
 * The composer-first surface for /dev and /chat: "Build what's next." with Dev's composer (model
 * picker, "+" menu, voice, environment chip), runnable with NO environment. A turn in Cloud is a
 * chat completion on the person's account, persisted to their cloud conversations; a turn aimed at a
 * machine is handed to the host (`onHandoff`), which mounts that runtime's `DevProvider` and opens
 * the thread with the prompt. Dev-only controls (environment chip, code tools) show when
 * `mode === "dev"` or once the thread has a workspace; `mode="chat"` hides them.
 */
export function DevHome(props: {
  cloud: DevCloud;
  mode: "chat" | "dev";
  /** Cloud first, then the person's linked Macs (from `GET /v1/sources`). */
  environments: readonly DevEnvironment[];
  /** The chosen environment id; the host keeps it. */
  environment: string | null;
  onEnvironment: (id: string) => void;
  /** A turn for a machine: the host mounts `environment.runtime` and opens a thread with `prompt`. */
  onHandoff?: (environment: DevEnvironment, prompt: string) => void;
  /** The open cloud conversation (the shell's /chat/<id>), null for a new one. */
  conversation?: string | null;
  /** The person started or switched a cloud conversation; the host moves its address. */
  onConversation?: (id: string | null) => void;
  className?: string;
}): ReactNode;

/** Every model that writes replies (text outputs only; decision models are never listed). */
export function useReplyModels(cloud: DevCloud): {
  readonly models: ReadonlyArray<{ readonly id: string; readonly label: string; readonly family?: string }>;
  readonly error: string | null;
};
