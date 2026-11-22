export type WidgetQuickActionKind = "chat" | "new" | "camera" | "voice" | "attach" | "clipboard";

export interface WidgetQuickAction {
  readonly id: number;
  readonly action: WidgetQuickActionKind;
  readonly prompt?: string;
  readonly threadId?: string;
  readonly autoSend?: boolean;
}

export interface ParsedWidgetQuickAction {
  readonly action: WidgetQuickActionKind;
  readonly prompt?: string;
  readonly threadId?: string;
  readonly autoSend?: boolean;
}

let seq = 0;
let pending: WidgetQuickAction | null = null;
const listeners = new Set<(action: WidgetQuickAction) => void>();

const VALID_ACTIONS = new Set<WidgetQuickActionKind>([
  "chat",
  "new",
  "camera",
  "voice",
  "attach",
  "clipboard",
]);

function decodeSegment(segment: string): string {
  try {
    return decodeURIComponent(segment);
  } catch {
    return segment;
  }
}

export function parseAskWidgetPath(path: string): ParsedWidgetQuickAction | null {
  const clean = path.replace(/^\/+/, "");
  const queryStart = clean.indexOf("?");
  const hashStart = clean.indexOf("#");
  const pathEnd =
    queryStart >= 0 && hashStart >= 0
      ? Math.min(queryStart, hashStart)
      : queryStart >= 0
        ? queryStart
        : hashStart >= 0
          ? hashStart
          : clean.length;
  const routePath = clean.slice(0, pathEnd).replace(/\/+$/, "");
  const querySlice =
    queryStart >= 0
      ? clean.slice(
          queryStart + 1,
          hashStart >= 0 && hashStart > queryStart ? hashStart : undefined,
        )
      : "";
  const params = new URLSearchParams(querySlice);
  const prompt = params.get("prompt")?.trim() || undefined;
  const rawSend = params.get("send")?.toLowerCase();
  const autoSend = rawSend === "1" || rawSend === "true" ? true : undefined;

  if (/^ask$/i.test(routePath)) {
    const rawAction = (params.get("action") ?? "chat").toLowerCase() as WidgetQuickActionKind;
    const action: WidgetQuickActionKind = VALID_ACTIONS.has(rawAction) ? rawAction : "chat";
    const threadId = params.get("thread")?.trim() || undefined;
    return {
      action,
      ...(prompt ? { prompt } : {}),
      ...(threadId ? { threadId } : {}),
      ...(autoSend ? { autoSend } : {}),
    };
  }

  // Handle hanzo://chat, hanzo://chat/new, hanzo://chat/voice, hanzo://chat/thread/<id>,
  // hanzo://chat?prompt=... — while preserving hanzo://chat/settings for the model picker route.
  if (/^chat(?:\/|$)/i.test(routePath)) {
    const segments = routePath.split("/");
    const sub = segments[1]?.toLowerCase();
    if (sub === "settings") return null;

    if (sub === "thread" && segments[2]) {
      const threadId = decodeSegment(segments.slice(2).join("/")).trim();
      if (!threadId) return null;
      return {
        action: "chat",
        threadId,
        ...(prompt ? { prompt } : {}),
        ...(autoSend ? { autoSend } : {}),
      };
    }

    if (sub === "new") {
      return {
        action: "new",
        ...(prompt ? { prompt } : {}),
        ...(autoSend ? { autoSend } : {}),
      };
    }

    if (sub === "voice" || sub === "camera" || sub === "attach" || sub === "clipboard") {
      return {
        action: sub,
        ...(prompt ? { prompt } : {}),
        ...(autoSend ? { autoSend } : {}),
      };
    }

    if (!sub) {
      const rawAction = (params.get("action") ?? "chat").toLowerCase() as WidgetQuickActionKind;
      const action: WidgetQuickActionKind = VALID_ACTIONS.has(rawAction) ? rawAction : "chat";
      const threadId = params.get("thread")?.trim() || undefined;
      return {
        action,
        ...(prompt ? { prompt } : {}),
        ...(threadId ? { threadId } : {}),
        ...(autoSend ? { autoSend } : {}),
      };
    }
  }

  return null;
}

export function dispatchWidgetQuickAction(input: ParsedWidgetQuickAction): void {
  const next: WidgetQuickAction = {
    id: ++seq,
    action: input.action,
    ...(input.prompt ? { prompt: input.prompt } : {}),
    ...(input.threadId ? { threadId: input.threadId } : {}),
    ...(input.autoSend ? { autoSend: input.autoSend } : {}),
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

export function subscribeWidgetQuickAction(
  listener: (action: WidgetQuickAction) => void,
): () => void {
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}
