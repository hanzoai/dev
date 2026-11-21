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
import { loadChatThread } from "./chatThreads.ts";
import {
  deleteRemoteThread,
  listRemoteThreads,
  mergeThreads,
  readRemoteThread,
  summaryOf,
  threadTitle,
  type StoredThread,
  type ThreadStore,
  type ThreadSummary,
} from "./threads.ts";

export type ThreadSync = "local" | "synced";

export interface CloudChatState {
  readonly messages: ReadonlyArray<CloudChatMessage>;
  readonly models: ReadonlyArray<CloudChatModel>;
  readonly model: string;
  /** Reasoning effort chosen for the model; null leaves it to the router. */
  readonly effort: string | null;
  /** The hanzo.app thread id, once the remote store has accepted the thread. */
  readonly threadId: string | null;
  /** Device key of the open thread; null before the first message. */
  readonly threadKey: string | null;
  /** Every known thread, newest first: device copies merged with the remote list. */
  readonly threads: ReadonlyArray<ThreadSummary>;
  /** `local` until the hanzo.app thread store accepts the turns; chat works either way. */
  readonly sync: ThreadSync;
  readonly sending: boolean;
  readonly error: CloudChatErrorKind | null;
}

export interface CloudChatController {
  getState(): CloudChatState;
  subscribe(listener: () => void): () => void;
  loadModels(): Promise<void>;
  selectModel(id: string, effort?: string | null): void;
  send(prompt: string): Promise<void>;
  /** Reads the device copies, then the remote list, into `state.threads`. */
  loadThreads(): Promise<void>;
  openThread(key: string): Promise<void>;
  /** Opens a saved hanzo.app thread by its backend id. */
  open(threadId: string): Promise<void>;
  /** Rename, pin and delete keep a device copy; delete also asks the remote store, best effort. */
  renameThread(key: string, title: string): Promise<void>;
  pinThread(key: string, pinned: boolean): Promise<void>;
  deleteThread(key: string): Promise<void>;
  /** A new, empty conversation. */
  reset(): void;
}

/**
 * One conversation. The thread lives in memory first; each completed turn is mirrored to the
 * hanzo.app thread store. A sync failure leaves `sync: "local"` and the next turn retries.
 */
export function createCloudChatController(input: {
  readonly getAuth: () => Promise<CloudChatAuth>;
  readonly options?: CloudChatOptions;
  /** Device copy of threads: a thread survives restart when sync fails. */
  readonly store?: ThreadStore;
  readonly createKey?: () => string;
  readonly now?: () => number;
}): CloudChatController {
  const createKey =
    input.createKey ??
    (() => `t-${Date.now().toString(36)}${Math.random().toString(36).slice(2, 10)}`);
  const now = input.now ?? Date.now;
  const initial: CloudChatState = {
    messages: [],
    models: FALLBACK_MODELS,
    model: DEFAULT_MODEL_ID,
    effort: null,
    threadId: null,
    threadKey: null,
    threads: [],
    sync: "local",
    sending: false,
    error: null,
  };
  let state = initial;
  let modelChosen = false;
  /** Turns the remote store already holds for the open thread. */
  let synced = 0;
  let writes: Promise<void> = Promise.resolve();
  /** Remote ids the user deleted: hidden even if the remote store still lists them. */
  const deleted = new Set<string>();
  const listeners = new Set<() => void>();
  const set = (patch: Partial<CloudChatState>) => {
    state = { ...state, ...patch };
    for (const listener of listeners) listener();
  };
  const ownerOf = (auth: CloudChatAuth) => auth.userId ?? "anonymous";
  /** Saves the open thread to the device and the list; failures never reach the chat. */
  const persist = (auth: CloudChatAuth) => {
    const key = state.threadKey;
    if (key === null || state.messages.length === 0) return;
    const last = state.messages.at(-1);
    const settled =
      last?.role === "assistant" && last.content === ""
        ? state.messages.slice(0, -1)
        : state.messages;
    const current = state.threads.find((entry) => entry.key === key);
    const thread: StoredThread = {
      key,
      remoteId: state.threadId,
      title: threadTitle(state.messages),
      updatedAt: now(),
      messages: settled,
      synced,
      pinned: current?.pinned === true,
      renamed: current?.renamed === true,
      ...(current?.renamed === true ? { title: current.title } : {}),
    };
    const others = state.threads.filter((entry) => entry.key !== key);
    set({ threads: [summaryOf(thread), ...others] });
    const { store } = input;
    if (store === undefined) return;
    writes = writes.then(() => store.save(ownerOf(auth), thread)).catch(() => undefined);
  };
  /** Writes a thread's flags onto its device copy, creating a message-less stub for a remote one. */
  const save = async (
    auth: CloudChatAuth,
    key: string,
    entry: ThreadSummary,
    flags: Partial<StoredThread>,
  ) => {
    const { store } = input;
    if (store === undefined) return;
    const existing = (await store.load(ownerOf(auth)).catch(() => [])).find((t) => t.key === key);
    const base: StoredThread = existing ?? {
      key,
      remoteId: entry.remoteId,
      title: entry.title,
      updatedAt: entry.updatedAt,
      messages: [],
      synced: 0,
    };
    writes = writes
      .then(() => store.save(ownerOf(auth), { ...base, ...flags }))
      .catch(() => undefined);
    await writes;
  };
  const patch = async (
    key: string,
    flags: { title?: string; pinned?: boolean; renamed?: boolean },
  ) => {
    const entry = state.threads.find((thread) => thread.key === key);
    if (entry === undefined) return;
    const next = { ...entry, ...flags };
    set({
      threads: state.threads
        .map((thread) => (thread.key === key ? next : thread))
        .toSorted((a, b) => Number(b.pinned) - Number(a.pinned) || b.updatedAt - a.updatedAt),
    });
    const auth = await input.getAuth().catch(() => ({ accessToken: null }) as CloudChatAuth);
    await save(auth, key, entry, flags);
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
    selectModel(id, effort = null) {
      modelChosen = true;
      set({ model: id, effort });
    },
    async send(prompt) {
      const text = prompt.trim();
      if (text === "" || state.sending) return;
      const user: CloudChatMessage = { role: "user", content: text };
      const history = [...state.messages, user];
      set({
        messages: [...history, { role: "assistant", content: "" }],
        sending: true,
        error: null,
        ...(state.threadKey === null ? { threadKey: createKey() } : {}),
      });
      let reply = "";
      let auth: CloudChatAuth = { accessToken: null };
      try {
        auth = await input.getAuth();
        persist(auth);
        reply = await streamChatReply({
          auth,
          model: state.model,
          effort: state.effort,
          messages: history,
          options: input.options,
          onDelta: (chunk) => {
            reply += chunk;
            set({ messages: [...history, { role: "assistant", content: reply }] });
          },
        });
      } catch (cause) {
        set({
          messages: reply === "" ? history : [...history, { role: "assistant", content: reply }],
          sending: false,
          error: kindOf(cause),
        });
        persist(auth);
        return;
      }
      const answer: CloudChatMessage = { role: "assistant", content: reply };
      const all = [...history, answer];
      set({ messages: all, sending: false });
      persist(auth);
      const threadId = await recordTurns({
        auth: await input.getAuth().catch(() => ({ accessToken: null })),
        threadId: state.threadId,
        messages: all.slice(synced),
        options: input.options,
      });
      if (threadId !== null) {
        synced = all.length;
        set({ threadId, sync: "synced" });
      } else set({ sync: "local" });
      persist(auth);
    },
    async loadThreads() {
      const auth = await input.getAuth().catch(() => ({ accessToken: null }) as CloudChatAuth);
      const stored = (await input.store?.load(ownerOf(auth)).catch(() => [])) ?? [];
      for (const thread of stored)
        if (thread.deleted === true && thread.remoteId !== null) deleted.add(thread.remoteId);
      const local = stored.filter((thread) => thread.deleted !== true).map(summaryOf);
      set({
        threads: mergeThreads(
          local,
          state.threads.filter((entry) => entry.local),
        ),
      });
      const remote = (await listRemoteThreads(auth, input.options)).filter(
        (entry) => entry.remoteId === null || !deleted.has(entry.remoteId),
      );
      set({ threads: mergeThreads(state.threads, remote) });
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
    async openThread(key) {
      if (state.sending) return;
      const auth = await input.getAuth().catch(() => ({ accessToken: null }) as CloudChatAuth);
      const entry = state.threads.find((thread) => thread.key === key);
      if (entry === undefined) return;
      const stored = entry.local
        ? ((await input.store?.load(ownerOf(auth)).catch(() => []))?.find((t) => t.key === key) ??
          null)
        : null;
      let messages = stored !== null && stored.messages.length > 0 ? stored.messages : null;
      if (messages === null && entry.remoteId !== null)
        messages = await readRemoteThread(auth, entry.remoteId, input.options);
      if (messages === null) {
        set({ error: "unavailable" });
        return;
      }
      synced = stored?.synced ?? messages.length;
      set({
        messages,
        threadKey: key,
        threadId: entry.remoteId,
        sync: entry.remoteId ? "synced" : "local",
        error: null,
      });
    },
    async renameThread(key, title) {
      const text = title.trim();
      if (text !== "") await patch(key, { title: text, renamed: true });
    },
    async pinThread(key, pinned) {
      await patch(key, { pinned });
    },
    async deleteThread(key) {
      const entry = state.threads.find((thread) => thread.key === key);
      if (entry === undefined) return;
      if (entry.remoteId !== null) deleted.add(entry.remoteId);
      set({ threads: state.threads.filter((thread) => thread.key !== key) });
      if (state.threadKey === key) {
        synced = 0;
        set({ messages: [], threadId: null, threadKey: null, sync: "local", error: null });
      }
      const auth = await input.getAuth().catch(() => ({ accessToken: null }) as CloudChatAuth);
      if (entry.remoteId !== null) void deleteRemoteThread(auth, entry.remoteId, input.options);
      await save(auth, key, entry, { deleted: true });
    },
    reset() {
      synced = 0;
      set({ messages: [], threadId: null, threadKey: null, sync: "local", error: null });
    },
  };
}
