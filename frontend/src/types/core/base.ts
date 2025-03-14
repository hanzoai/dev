export type HanzoEventType =
  | "message"
  | "agent_state_changed"
  | "run"
  | "read"
  | "write"
  | "edit"
  | "run_ipython"
  | "delegate"
  | "browse"
  | "browse_interactive"
  | "reject"
  | "think"
  | "finish"
  | "error";

interface HanzoBaseEvent {
  id: number;
  source: "agent" | "user";
  message: string;
  timestamp: string; // ISO 8601
}

export interface HanzoActionEvent<T extends HanzoEventType>
  extends HanzoBaseEvent {
  action: T;
  args: Record<string, unknown>;
}

export interface HanzoObservationEvent<T extends HanzoEventType>
  extends HanzoBaseEvent {
  cause: number;
  observation: T;
  content: string;
  extras: Record<string, unknown>;
}
