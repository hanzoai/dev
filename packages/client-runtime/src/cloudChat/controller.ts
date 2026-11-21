import {
  CloudChatError,
  DEFAULT_MODEL_ID,
  FALLBACK_MODELS,
  listChatModels,
  pickDefaultModel,
  recordTurns,
  streamChatReply,
  type CloudChatAuth,
  type CloudChatErrorKind,
  type CloudChatMessage,
  type CloudChatModel,
  type CloudChatOptions,
} from "./api.ts";
import { loadChatThread } from "./threads.ts";

export type ThreadSync = "local" | "synced";

export interface CloudChatState {
  readonly messages: ReadonlyArray<CloudChatMessage>;
  readonly models: ReadonlyArray<CloudChatModel>;
  readonly model: string;
  readonly threadId: string | null;
  /** `local` until the hanzo.app thread store accepts the turns; chat works either way. */
  readonly sync: ThreadSync;
  readonly sending: boolean;
  readonly error: CloudChatErrorKind | null;
}

export interface CloudChatController {
  getState(): CloudChatState;
  subscribe(listener: () => void): () => void;
  loadModels(): Promise<void>;
  selectModel(id: string): void;
  send(prompt: string): Promise<void>;
  /** Replaces the conversation with a saved thread; later turns append to it. */
  open(threadId: string): Promise<void>;
  reset(): void;
}

/**
 * One conversation. The thread lives in memory first; each completed turn is mirrored to the
 * hanzo.app thread store. A sync failure leaves `sync: "local"` and the next turn retries.
 */
export function createCloudChatController(input: {
  readonly getAuth: () => Promise<CloudChatAuth>;
  readonly options?: CloudChatOptions;
}): CloudChatController {
  const initial: CloudChatState = {
    messages: [],
    models: FALLBACK_MODELS,
    model: DEFAULT_MODEL_ID,
    threadId: null,
    sync: "local",
    sending: false,
    error: null,
  };
  let state = initial;
  let modelChosen = false;
  const listeners = new Set<() => void>();
  const set = (patch: Partial<CloudChatState>) => {
    state = { ...state, ...patch };
    for (const listener of listeners) listener();
  };
  const kindOf = (cause: unknown): CloudChatErrorKind =>
    cause instanceof CloudChatError ? cause.kind : "failed";

  return {
    getState: () => state,
    subscribe(listener) {
      listeners.add(listener);
      return () => void listeners.delete(listener);
    },
    async loadModels() {
      try {
        const models = await listChatModels(await input.getAuth(), input.options);
        set({ models, ...(modelChosen ? {} : { model: pickDefaultModel(models) }) });
      } catch {
        // The known live set stays; sending reports sign-in problems itself.
      }
    },
    selectModel(id) {
      modelChosen = true;
      set({ model: id });
    },
    async send(prompt) {
      const text = prompt.trim();
      if (text === "" || state.sending) return;
      const user: CloudChatMessage = { role: "user", content: text };
      const history = [...state.messages, user];
      set({ messages: [...history, { role: "assistant", content: "" }], sending: true, error: null });
      let reply = "";
      try {
        const auth = await input.getAuth();
        reply = await streamChatReply({
          auth,
          model: state.model,
          messages: history,
          options: input.options,
          onDelta: (chunk) => {
            reply += chunk;
            set({ messages: [...history, { role: "assistant", content: reply }] });
          },
        });
      } catch (cause) {
        set({ messages: reply === "" ? history : [...history, { role: "assistant", content: reply }], sending: false, error: kindOf(cause) });
        return;
      }
      const answer: CloudChatMessage = { role: "assistant", content: reply };
      set({ messages: [...history, answer], sending: false });
      const auth = await input.getAuth().catch(() => ({ accessToken: null }));
      const threadId = await recordTurns({
        auth,
        threadId: state.threadId,
        messages: [user, answer],
        options: input.options,
      });
      if (threadId !== null) set({ threadId, sync: "synced" });
      else set({ sync: "local" });
    },
    async open(threadId) {
      if (state.sending) return;
      try {
        const messages = await loadChatThread(await input.getAuth(), threadId, input.options);
        set({ messages, threadId, sync: "synced", error: null });
      } catch (cause) {
        set({ error: kindOf(cause) });
      }
    },
    reset() {
      set({ messages: [], threadId: null, sync: "local", error: null });
    },
  };
}
