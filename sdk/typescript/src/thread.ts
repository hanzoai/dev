import { CodexOptions } from "./codexOptions";
import { ThreadEvent, ThreadError, Usage } from "./events";
import { CodexExec } from "./exec";
import { ThreadItem } from "./items";
import { ThreadOptions } from "./threadOptions";

/** Completed turn. */
export type Turn = {
  items: ThreadItem[];
  finalResponse: string;
  usage: Usage | null;
};

/** Alias for `Turn` to describe the result of `run()`. */
export type RunResult = Turn;

/** The result of the `runStreamed` method. */
export type StreamedTurn = {
  events: AsyncGenerator<ThreadEvent>;
};

/** Alias for `StreamedTurn` to describe the result of `runStreamed()`. */
export type RunStreamedResult = StreamedTurn;

/** An input to send to the agent. */
export type Input = string;

/** Represent a thread of conversation with the agent. One thread can have multiple consecutive turns. */
export class Thread {
  private _exec: CodexExec;
  private _options: CodexOptions;
  private _id: string | null;
  private _threadOptions: ThreadOptions;

  /** Returns the ID of the thread. Populated after the first turn starts. */
  public get id(): string | null {
    return this._id;
  }

  /* @internal */
  constructor(
    exec: CodexExec,
    options: CodexOptions,
    threadOptions: ThreadOptions,
    id: string | null = null,
  ) {
    this._exec = exec;
    this._options = options;
    this._id = id;
    this._threadOptions = threadOptions;
  }

  /** Provides the input to the agent and streams events as they are produced during the turn. */
  async runStreamed(input: string, options?: ThreadOptions): Promise<StreamedTurn> {
    return { events: this.runStreamedInternal(input, options) };
  }

  private async *runStreamedInternal(
    input: string,
    options?: ThreadOptions,
  ): AsyncGenerator<ThreadEvent> {
    const mergedOptions = {
      ...this._threadOptions,
      ...options,
    };
    if (options) {
      this._threadOptions = { ...mergedOptions };
    }
    const generator = this._exec.run({
      input,
      baseUrl: this._options.baseUrl,
      apiKey: this._options.apiKey,
      threadId: this._id,
      model: mergedOptions?.model,
      sandboxMode: mergedOptions?.sandboxMode,
      workingDirectory: mergedOptions?.workingDirectory,
      skipGitRepoCheck: mergedOptions?.skipGitRepoCheck,
    });

    for await (const item of generator) {
      let parsed: unknown;
      try {
        parsed = JSON.parse(item);
      } catch {
        continue;
      }

      const event = parseThreadEvent(parsed);
      if (!event) {
        continue;
      }

      if (event.type === "thread.started") {
        this._id = event.thread_id;
      }
      yield event;
    }
  }

  /** Provides the input to the agent and returns the completed turn. */
  async run(input: string, options?: ThreadOptions): Promise<Turn> {
    const generator = this.runStreamedInternal(input, options);
    const items: ThreadItem[] = [];
    let finalResponse: string = "";
    let usage: Usage | null = null;
    let turnFailure: ThreadError | null = null;
    for await (const event of generator) {
      if (event.type === "item.completed") {
        if (event.item.type === "agent_message") {
          finalResponse = event.item.text;
        }
        items.push(event.item);
      } else if (event.type === "turn.completed") {
        usage = event.usage;
      } else if (event.type === "turn.failed") {
        turnFailure = event.error;
        break;
      }
    }
    if (turnFailure) {
      throw new Error(turnFailure.message);
    }
    return { items, finalResponse, usage };
  }
}

/**
 * `dev exec --json` emits the `thread.*` / `item.*` schema directly — the same
 * shape declared in ./events.ts, which is generated from
 * code-rs/exec/src/exec_events.rs. This used to reconstruct that schema from
 * raw core events on this side, which meant the contract only ever existed in
 * TypeScript and nothing verified the two halves agreed. They are one wire
 * format now, so this only has to recognise it.
 */
const THREAD_EVENT_TYPES = new Set([
  "thread.started",
  "turn.started",
  "turn.completed",
  "turn.failed",
  "item.started",
  "item.updated",
  "item.completed",
  "error",
]);

function parseThreadEvent(raw: unknown): ThreadEvent | null {
  if (typeof raw !== "object" || raw === null) {
    return null;
  }
  const type = (raw as Record<string, unknown>).type;
  if (typeof type !== "string" || !THREAD_EVENT_TYPES.has(type)) {
    return null;
  }
  return raw as ThreadEvent;
}
