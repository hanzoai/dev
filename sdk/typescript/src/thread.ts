import { CodexOptions } from "./codexOptions";
import { ThreadEvent } from "./events";
import { CodexExec } from "./exec";
import { ThreadItem } from "./items";
import { TurnOptions } from "./turnOptions";

export type RunResult = {
  items: ThreadItem[];
  finalResponse: string;
};

export type RunStreamedResult = {
  events: AsyncGenerator<ThreadEvent>;
};

export type Input = string;

export class Thread {
  private exec: CodexExec;
  private options: CodexOptions;
  public id: string | null;

  constructor(exec: CodexExec, options: CodexOptions, id: string | null = null) {
    this.exec = exec;
    this.options = options;
    this.id = id;
  }

  async runStreamed(input: string, options?: TurnOptions): Promise<RunStreamedResult> {
    return { events: this.runStreamedInternal(input, options) };
  }

  private async *runStreamedInternal(
    input: string,
    options?: TurnOptions,
  ): AsyncGenerator<ThreadEvent> {
    const generator = this.exec.run({
      input,
      baseUrl: this.options.baseUrl,
      apiKey: this.options.apiKey,
      threadId: this.id,
      model: options?.model,
      sandboxMode: options?.sandboxMode,
    });

    for await (const item of generator) {
      let parsed: unknown;
      try {
        parsed = JSON.parse(item);
      } catch {
        continue;
      }

      const threadEvents = mapCliEventToThreadEvents(parsed, this.id);
      if (threadEvents.length === 0) {
        continue;
      }

      for (const event of threadEvents) {
        if (event.type === "thread.started") {
          this.id = event.thread_id;
        }
        yield event;
      }
    }
  }

  async run(input: string, options?: TurnOptions): Promise<RunResult> {
    const generator = this.runStreamedInternal(input, options);
    const items: ThreadItem[] = [];
    let finalResponse: string = "";
    for await (const event of generator) {
      if (event.type === "item.completed") {
        if (event.item.item_type === "assistant_message") {
          finalResponse = event.item.text;
        }
        items.push(event.item);
      }
    }
    return { items, finalResponse };
  }
}

function mapCliEventToThreadEvents(
  raw: unknown,
  currentThreadId: string | null,
): ThreadEvent[] {
  if (typeof raw !== "object" || raw === null) {
    return [];
  }

  const event = raw as Record<string, unknown>;
  const msg = event.msg as Record<string, unknown> | undefined;
  const msgType = typeof msg?.type === "string" ? (msg.type as string) : undefined;

  if (!msgType || !msg) {
    return [];
  }

  switch (msgType) {
    case "session_configured": {
      const threadId = deriveThreadId(event, msg, currentThreadId);
      return [
        { type: "thread.started", thread_id: threadId },
      ];
    }
    case "task_started": {
      const events: ThreadEvent[] = [];
      if (!currentThreadId) {
        const derivedThreadId = deriveThreadId(event, msg, currentThreadId);
        events.push({ type: "thread.started", thread_id: derivedThreadId });
      }
      events.push({ type: "turn.started" });
      return events;
    }
    case "agent_reasoning": {
      const text = typeof msg?.text === "string" ? (msg.text as string) : "";
      const reasoningItem: ThreadItem = {
        id: extractEventId(event),
        item_type: "reasoning",
        text,
      };
      return [
        {
          type: "item.completed",
          item: reasoningItem,
        },
      ];
    }
    case "agent_message": {
      const message = typeof msg?.message === "string" ? (msg.message as string) : "";
      const assistantItem: ThreadItem = {
        id: extractEventId(event),
        item_type: "assistant_message",
        text: message,
      };
      return [
        {
          type: "item.completed",
          item: assistantItem,
        },
      ];
    }
    case "token_count": {
      const info = msg?.info as { total_token_usage?: Record<string, unknown> } | undefined;
      const usageInfo = info?.total_token_usage ?? {};
      const usage = normalizeUsage(usageInfo);
      return [
        {
          type: "turn.completed",
          usage,
        },
      ];
    }
    case "error": {
      const message = typeof msg?.message === "string" ? (msg.message as string) : "Unknown error";
      return [
        {
          type: "error",
          message,
        },
      ];
    }
    default:
      return [];
  }
}

function deriveThreadId(
  event: Record<string, unknown>,
  msg: Record<string, unknown>,
  currentThreadId: string | null,
): string {
  if (typeof msg.session_id === "string" && msg.session_id.length > 0) {
    return msg.session_id;
  }

  const explicitId = event.thread_id;
  if (typeof explicitId === "string" && explicitId.length > 0) {
    return explicitId;
  }

  if (currentThreadId) {
    return currentThreadId;
  }

  const eventId = event.id;
  if (typeof eventId === "string" && eventId.length > 0) {
    return eventId;
  }

  return `thread-${Date.now()}`;
}

function extractEventId(event: Record<string, unknown>): string {
  const identifier = event.id;
  if (typeof identifier === "string" && identifier.length > 0) {
    return identifier;
  }
  const orderSequence = typeof event.event_seq === "number" ? event.event_seq : Date.now();
  return `event-${orderSequence}`;
}

function normalizeUsage(raw: Record<string, unknown>): {
  input_tokens: number;
  cached_input_tokens: number;
  output_tokens: number;
} {
  const toNumber = (value: unknown): number => {
    if (typeof value === "number" && Number.isFinite(value)) {
      return value;
    }
    if (typeof value === "string") {
      const parsed = Number(value);
      return Number.isFinite(parsed) ? parsed : 0;
    }
    return 0;
  };

  return {
    input_tokens: toNumber(raw.input_tokens),
    cached_input_tokens: toNumber(raw.cached_input_tokens),
    output_tokens: toNumber(raw.output_tokens),
  };
}
