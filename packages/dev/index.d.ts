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

export function DevProvider(props: { runtime: DevRuntime; children?: ReactNode }): ReactNode;

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
  onCloud: () => void;
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
