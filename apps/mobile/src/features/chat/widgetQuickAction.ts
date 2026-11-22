export type WidgetQuickActionKind = "chat" | "camera" | "voice" | "attach" | "clipboard";

export interface WidgetQuickAction {
  readonly id: number;
  readonly action: WidgetQuickActionKind;
  readonly prompt?: string;
}

let seq = 0;
let pending: WidgetQuickAction | null = null;
const listeners = new Set<(action: WidgetQuickAction) => void>();

const VALID_ACTIONS = new Set<WidgetQuickActionKind>([
  "chat",
  "camera",
  "voice",
  "attach",
  "clipboard",
]);

export function parseAskWidgetPath(path: string): {
  readonly action: WidgetQuickActionKind;
  readonly prompt?: string;
} | null {
  const clean = path.replace(/^\/+/, "");
  if (!/^ask(?:$|[/?#])/i.test(clean)) return null;
  const queryStart = clean.indexOf("?");
  const params = queryStart >= 0 ? new URLSearchParams(clean.slice(queryStart + 1)) : new URLSearchParams();
  const rawAction = (params.get("action") ?? "chat").toLowerCase() as WidgetQuickActionKind;
  const action: WidgetQuickActionKind = VALID_ACTIONS.has(rawAction) ? rawAction : "chat";
  const prompt = params.get("prompt")?.trim() || undefined;
  return { action, prompt };
}

export function dispatchWidgetQuickAction(input: {
  readonly action: WidgetQuickActionKind;
  readonly prompt?: string;
}): void {
  const next: WidgetQuickAction = {
    id: ++seq,
    action: input.action,
    prompt: input.prompt,
  };
  pending = next;
  for (const listener of listeners) {
    listener(next);
  }
}

export function consumePendingWidgetQuickAction(): WidgetQuickAction | null {
  const current = pending;
  pending = null;
  return current;
}

export function subscribeWidgetQuickAction(listener: (action: WidgetQuickAction) => void): () => void {
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}
