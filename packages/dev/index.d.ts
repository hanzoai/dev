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
  className?: string;
}): ReactNode;

export function DevWorkspace(props: {
  thread?: string | null;
  onNavigate?: (ref: string | null) => void;
  dock?: (context: { runtime: DevRuntime; thread: string | null }) => ReactNode;
  className?: string;
}): ReactNode;
