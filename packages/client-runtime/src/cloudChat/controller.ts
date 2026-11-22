// @effect-diagnostics globalDate:off globalRandom:off -- Cloud chat is a plain promise client shared with React Native and the browser; it has no Effect runtime.
import {
  CloudChatError,
  DEFAULT_MODEL_ID,
  FALLBACK_MODELS,
  listChatModels,
  pickDefaultModel,
  recordTurns,
  streamChatReply,
  type CloudChatAttachment,
  type CloudChatAuth,
  type CloudChatErrorKind,
  type CloudChatMessage,
  type CloudChatModel,
  type CloudChatOptions,
} from "./api.ts";
import { loadChatThread } from "./chatThreads.ts";
import {
  isLocalChatSlug,
  localChatModelId,
  type LocalChatHost,
  type LocalChatModel,
} from "./local.ts";
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
  /** The linked Mac's models, as last listed; empty until it has answered. */
  readonly localModels: ReadonlyArray<LocalChatModel>;
  /** Whether the linked Mac is connected: local models can only be chosen and used while it is. */
  readonly macOnline: boolean;
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
  /** Private chat: the open conversation is kept in memory only, never saved, listed or recorded. */
  readonly ghost: boolean;
}

/** What shapes one turn beyond its words: a persona's instructions and the model it is pinned to. */
export interface CloudChatTurn {
  readonly system?: string | undefined;
  readonly model?: string | undefined;
}

export interface CloudChatController {
  getState(): CloudChatState;
  subscribe(listener: () => void): () => void;
  loadModels(): Promise<void>;
  /** Asks the linked Mac for its models; with no Mac connected, local models are offline. */
  loadLocalModels(): Promise<void>;
  selectModel(id: string, effort?: string | null): void;
  /** Attachments ride this request and later ones of the open thread; threads store text only. */
  send(prompt: string, attachments?: ReadonlyArray<CloudChatAttachment>, turn?: CloudChatTurn): Promise<void>;
  /** Reads the device copies, then the remote list, into `state.threads`. */
  loadThreads(): Promise<void>;
  openThread(key: string): Promise<void>;
  /** Opens a saved hanzo.app thread by its backend id. */
  open(threadId: string): Promise<void>;
  /** Rename, pin and delete keep a device copy; delete also asks the remote store, best effort. */
  renameThread(key: string, title: string): Promise<void>;
  pinThread(key: string, pinned: boolean): Promise<void>;
  deleteThread(key: string): Promise<void>;
  /** Private chat on or off for the open conversation; a ghost turn is never saved to the device or the cloud. */
  setGhost(on: boolean): void;
  /** A new, empty conversation (a normal one: ghost is off). */
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
  /** The linked Mac, or null while it is not connected. Local models are served only by it. */
  readonly local?: () => LocalChatHost | null;
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
    localModels: [],
    macOnline: false,
    model: DEFAULT_MODEL_ID,
    effort: null,
    threadId: null,
    threadKey: null,
    threads: [],
    sync: "local",
    sending: false,
    error: null,
    ghost: false,
  };
  let state = initial;
  let modelChosen = false;
  /** Attachments by message index, for the open conversation. */
  let attached = new Map<number, ReadonlyArray<CloudChatAttachment>>();
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
    if (state.ghost || key === null || state.messages.length === 0) return;
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
        .sort((a, b) => Number(b.pinned) - Number(a.pinned) || b.updatedAt - a.updatedAt),
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
    async loadLocalModels() {
      const host = input.local?.() ?? null;
      if (host === null) {
        set({ macOnline: false });
        return;
      }
      try {
        set({ localModels: await host.models(), macOnline: true });
      } catch {
        set({ macOnline: false });
      }
    },
    selectModel(id, effort = null) {
      // A local model is chosen only while the Mac is there; the choice never falls back to cloud.
      if (isLocalChatSlug(id) && input.local?.() == null) return;
      modelChosen = true;
      set({ model: id, effort });
    },
    async send(prompt, attachments = [], turn = {}) {
      const text = prompt.trim();
      if (text === "" || state.sending) return;
      const user: CloudChatMessage = { role: "user", content: text };
      const history = [...state.messages, user];
      if (attachments.length > 0) attached.set(state.messages.length, attachments);
      const request = history.map((message, index) => {
        const files = attached.get(index);
        return files === undefined ? message : { ...message, attachments: files };
      });
      set({
        messages: [...history, { role: "assistant", content: "" }],
        sending: true,
        error: null,
        ...(state.threadKey === null ? { threadKey: createKey() } : {}),
      });
      let reply = "";
      let auth: CloudChatAuth = { accessToken: null };
      const onLocal = isLocalChatSlug(state.model);
      const onDelta = (chunk: string) => {
        reply += chunk;
        set({ messages: [...history, { role: "assistant", content: reply }] });
      };
      try {
        auth = await input.getAuth();
        persist(auth);
        if (onLocal) {
          const host = input.local?.() ?? null;
          if (host === null) {
            set({ macOnline: false });
            throw new CloudChatError("offline", "The Mac is not connected");
          }
          reply = await host.stream({
            model: localChatModelId(state.model),
            messages: request,
            onDelta,
          });
        } else
          reply = await streamChatReply({
            auth,
            model: turn.model ?? state.model,
            effort: state.effort,
            system: turn.system,
            messages: request,
            options: input.options,
            onDelta,
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
      // A local conversation stays on the phone and the Mac, and a private one stays in memory:
      // nothing is recorded at hanzo.app.
      if (onLocal || state.ghost) return;
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
        attached = new Map();
        set({ messages, threadId, sync: "synced", error: null, ghost: false });
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
      attached = new Map();
      set({
        messages,
        threadKey: key,
        threadId: entry.remoteId,
        sync: entry.remoteId ? "synced" : "local",
        error: null,
        ghost: false,
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
    setGhost(on) {
      if (state.ghost === on) return;
      // Turning it on drops the conversation from the list: it was not private until now.
      const key = state.threadKey;
      set({
        ghost: on,
        ...(on && key !== null ? { threads: state.threads.filter((entry) => entry.key !== key) } : {}),
      });
    },
    reset() {
      synced = 0;
      attached = new Map();
      set({
        messages: [],
        threadId: null,
        threadKey: null,
        sync: "local",
        error: null,
        ghost: false,
      });
    },
  };
}
