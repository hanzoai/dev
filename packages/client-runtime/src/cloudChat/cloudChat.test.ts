// @effect-diagnostics globalDate:off -- Test fixtures build timestamps outside an Effect runtime.
import { describe, expect, it } from "vite-plus/test";

import {
  CloudChatError,
  createCloudChatController,
  createSseParser,
  listChatModels,
  withAuto,
  parseChatModels,
  runInCloud,
  streamChatReply,
} from "./index.ts";

const json = (status: number, body: unknown) =>
  new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json" } });

const sse = (chunks: ReadonlyArray<string>, status = 200) => {
  const encoder = new TextEncoder();
  return new Response(
    new ReadableStream({
      start(controller) {
        for (const chunk of chunks) controller.enqueue(encoder.encode(chunk));
        controller.close();
      },
    }),
    { status },
  );
};
const frame = (text: string) =>
  `data: ${JSON.stringify({ choices: [{ delta: { content: text } }] })}\n\n`;
const auth = { accessToken: "tok", orgId: "acme" };

describe("sse parser", () => {
  it("joins partial chunks and stops at [DONE]", () => {
    const parser = createSseParser();
    const whole = frame("Hel") + frame("lo") + "data: [DONE]\n\n" + frame("ignored");
    const events = [];
    for (let i = 0; i < whole.length; i += 7) events.push(...parser.push(whole.slice(i, i + 7)));
    events.push(...parser.flush());
    expect(events).toEqual([
      { kind: "delta", text: "Hel" },
      { kind: "delta", text: "lo" },
      { kind: "done" },
    ]);
  });
  it("flushes an unterminated last line and skips junk", () => {
    const parser = createSseParser();
    expect(
      parser.push(": ping\r\ndata: {bad\n" + 'data: {"choices":[{"delta":{"content":"x"}}]}'),
    ).toEqual([]);
    expect(parser.flush()).toEqual([{ kind: "delta", text: "x" }]);
  });
});

describe("models", () => {
  it("keeps chat models and drops decision models", () => {
    const models = parseChatModels({
      data: [
        { id: "enso-flash", outputs: ["text"] },
        { id: "kai-1", outputs: ["decision"], endpoint: "/v1/decisions" },
        { id: "zen6-flash" },
        { id: "", outputs: ["text"] },
      ],
    });
    expect(models.map((m) => m.id)).toEqual(["enso-flash", "zen6-flash"]);
  });
  it("sends bearer and org, and falls back to the known set when empty", async () => {
    let seen: Record<string, string> = {};
    const models = await listChatModels(auth, {
      fetch: async (_url, init) => {
        seen = init?.headers as Record<string, string>;
        return json(200, { data: [] });
      },
    });
    expect(seen.Authorization).toBe("Bearer tok");
    expect(seen["X-Org-Id"]).toBe("acme");
    expect(models.map((m) => m.id)).toContain("enso");
  });
  it("always offers Auto first, once, and sends its id so the gateway routes", async () => {
    const listed = await listChatModels(auth, {
      fetch: async () => json(200, { data: [{ id: "enso-auto", outputs: ["text"] }, { id: "enso-flash", outputs: ["text"] }] }),
    });
    expect(listed.map((m) => m.id)).toEqual(["enso-auto", "enso-flash"]);
    expect(listed[0]?.label).toBe("Auto");
    expect(withAuto([{ id: "zen6-flash", label: "zen6-flash" }]).map((m) => m.id)).toEqual(["enso-auto", "zen6-flash"]);
  });
});

describe("streamChatReply", () => {
  it("streams deltas across split chunks", async () => {
    const got: string[] = [];
    const reply = await streamChatReply({
      auth,
      model: "enso-flash",
      messages: [{ role: "user", content: "hi" }],
      onDelta: (t) => got.push(t),
      options: {
        fetch: async () =>
          sse([
            frame("Hi").slice(0, 10),
            frame("Hi").slice(10),
            frame(" there"),
            "data: [DONE]\n\n",
          ]),
      },
    });
    expect(reply).toBe("Hi there");
    expect(got).toEqual(["Hi", " there"]);
  });
  it("refuses without a token and never calls the network", async () => {
    let called = false;
    const error = await streamChatReply({
      auth: { accessToken: null },
      model: "m",
      messages: [],
      onDelta: () => undefined,
      options: { fetch: async () => ((called = true), json(200, {})) },
    }).catch((e: unknown) => e);
    expect(error).toBeInstanceOf(CloudChatError);
    expect((error as CloudChatError).kind).toBe("signed-out");
    expect(called).toBe(false);
  });
  it("maps 401 to unauthorized", async () => {
    const error = await streamChatReply({
      auth,
      model: "m",
      messages: [],
      onDelta: () => undefined,
      options: { fetch: async () => json(401, {}) },
    }).catch((e: unknown) => e);
    expect((error as CloudChatError).kind).toBe("unauthorized");
  });
});

describe("controller", () => {
  const route = (threads: () => Response | Promise<Response>) => async (url: string) =>
    url.endsWith("/v1/chat/completions")
      ? sse([frame("Pong"), "data: [DONE]\n\n"])
      : url.endsWith("/v1/agents/chat/conversations")
        ? threads()
        : json(404, {});

  it("syncs the thread and continues it by id", async () => {
    const bodies: unknown[] = [];
    const chat = createCloudChatController({
      getAuth: async () => auth,
      options: {
        fetch: async (url, init) => {
          if (url.endsWith("/conversations")) bodies.push(JSON.parse(String(init?.body)));
          return route(() => json(200, { conversationId: "c1" }))(url);
        },
      },
    });
    await chat.send("Ping");
    await chat.send("Again");
    expect(chat.getState().sync).toBe("synced");
    expect(chat.getState().threadId).toBe("c1");
    expect(bodies[0]).toEqual({
      messages: [
        { role: "user", content: "Ping" },
        { role: "assistant", content: "Pong" },
      ],
    });
    expect(bodies[1]).toMatchObject({ conversationId: "c1" });
  });

  it.each([404, 500])("degrades to a local thread when sync answers %i", async (status: number) => {
    const chat = createCloudChatController({
      getAuth: async () => auth,
      options: { fetch: route(() => json(status, {})) },
    });
    await chat.send("Ping");
    const state = chat.getState();
    expect(state.messages.map((m) => m.content)).toEqual(["Ping", "Pong"]);
    expect(state.sync).toBe("local");
    expect(state.error).toBeNull();
    expect(state.sending).toBe(false);
  });

  it("degrades when sync throws", async () => {
    const chat = createCloudChatController({
      getAuth: async () => auth,
      options: { fetch: route(() => Promise.reject(new Error("offline"))) },
    });
    await chat.send("Ping");
    expect(chat.getState().messages).toHaveLength(2);
    expect(chat.getState().sync).toBe("local");
  });

  it("reports signed-out and unauthorized without losing the prompt", async () => {
    const out = createCloudChatController({ getAuth: async () => ({ accessToken: null }) });
    await out.send("Ping");
    expect(out.getState().error).toBe("signed-out");
    expect(out.getState().messages).toEqual([{ role: "user", content: "Ping" }]);

    const denied = createCloudChatController({
      getAuth: async () => auth,
      options: { fetch: async () => json(401, {}) },
    });
    await denied.send("Ping");
    expect(denied.getState().error).toBe("unauthorized");
  });
});

describe("runInCloud", () => {
  it.each([404, 501])("says unavailable on %i", async (status: number) => {
    expect(await runInCloud(auth, { fetch: async () => json(status, {}) })).toEqual({
      kind: "unavailable",
    });
  });
  it("starts a session", async () => {
    const result = await runInCloud(auth, {
      fetch: async () => json(200, { id: "s1", zap: "zap://x" }),
    });
    expect(result).toEqual({ kind: "started", sessionId: "s1" });
  });
});

const memoryStore = () => {
  const rows = new Map<string, import("./index.ts").StoredThread>();
  return {
    rows,
    load: async (_owner: string) => [...rows.values()],
    save: async (_owner: string, thread: import("./index.ts").StoredThread) =>
      void rows.set(thread.key, thread),
  };
};

describe("stream assembly", () => {
  it("grows the assistant message token by token, then settles", async () => {
    const seen: string[] = [];
    const chat = createCloudChatController({
      getAuth: async () => auth,
      options: {
        fetch: async (url) =>
          url.endsWith("/v1/chat/completions")
            ? sse([
                frame("Hel").slice(0, 12),
                frame("Hel").slice(12),
                frame("lo"),
                frame(", wor"),
                frame("ld"),
                "data: [DONE]\n\n",
              ])
            : json(404, {}),
      },
    });
    chat.subscribe(() => {
      const last = chat.getState().messages.at(-1);
      if (last?.role === "assistant") seen.push(last.content);
    });
    await chat.send("hi");
    const distinct = seen.filter((value, i) => value !== seen[i - 1]);
    expect(distinct).toEqual(["", "Hel", "Hello", "Hello, wor", "Hello, world"]);
    expect(chat.getState().sending).toBe(false);
  });
});

describe("thread persistence", () => {
  const completions = (url: string) =>
    url.endsWith("/v1/chat/completions") ? sse([frame("Pong"), "data: [DONE]\n\n"]) : null;

  it("keeps the thread on the device when sync fails, and reopens it", async () => {
    const store = memoryStore();
    const chat = createCloudChatController({
      getAuth: async () => auth,
      store,
      createKey: () => "k1",
      options: { fetch: async (url) => completions(url) ?? json(500, {}) },
    });
    await chat.send("Ping");
    expect(store.rows.get("k1")).toMatchObject({
      remoteId: null,
      title: "Ping",
      synced: 0,
      messages: [
        { role: "user", content: "Ping" },
        { role: "assistant", content: "Pong" },
      ],
    });
    const restarted = createCloudChatController({
      getAuth: async () => auth,
      store,
      options: { fetch: async () => json(404, {}) },
    });
    await restarted.loadThreads();
    expect(restarted.getState().threads.map((t) => t.title)).toEqual(["Ping"]);
    await restarted.openThread("k1");
    expect(restarted.getState().messages.map((m) => m.content)).toEqual(["Ping", "Pong"]);
  });

  it("resends turns the remote store missed once sync recovers", async () => {
    const store = memoryStore();
    let healthy = false;
    const bodies: { messages: unknown[] }[] = [];
    const chat = createCloudChatController({
      getAuth: async () => auth,
      store,
      createKey: () => "k1",
      options: {
        fetch: async (url, init) => {
          if (url.endsWith("/conversations")) {
            if (!healthy) return json(503, {});
            bodies.push(JSON.parse(String(init?.body)));
            return json(200, { conversationId: "c9" });
          }
          return completions(url) ?? json(404, {});
        },
      },
    });
    await chat.send("one");
    healthy = true;
    await chat.send("two");
    expect(bodies[0]?.messages).toHaveLength(4);
    expect(store.rows.get("k1")).toMatchObject({ remoteId: "c9", synced: 4 });
  });

  it("merges remote threads with device copies and reads a remote-only one", async () => {
    const store = memoryStore();
    await store.save("u", {
      key: "k1",
      remoteId: "c1",
      title: "Mine",
      updatedAt: 1000,
      synced: 2,
      messages: [{ role: "user", content: "a" }],
    });
    const chat = createCloudChatController({
      getAuth: async () => auth,
      store,
      options: {
        fetch: async (url) =>
          url.endsWith("/conversations")
            ? json(200, {
                conversations: [
                  { id: "c1", title: "Mine renamed", updatedAt: new Date(5000).toISOString() },
                  { id: "c2", title: "Other device", updatedAt: new Date(3000).toISOString() },
                ],
              })
            : url.endsWith("/conversations/c2")
              ? json(200, {
                  id: "c2",
                  messages: [
                    { id: "1", role: "user", content: "hey" },
                    { id: "2", role: "assistant", content: "yo" },
                  ],
                })
              : json(404, {}),
      },
    });
    await chat.loadThreads();
    expect(chat.getState().threads.map((t) => [t.key, t.title])).toEqual([
      ["k1", "Mine renamed"],
      ["c2", "Other device"],
    ]);
    await chat.openThread("c2");
    expect(chat.getState().messages.map((m) => m.content)).toEqual(["hey", "yo"]);
    expect(chat.getState().threadId).toBe("c2");
  });

  it("lists nothing, quietly, when the thread endpoint is missing", async () => {
    const chat = createCloudChatController({
      getAuth: async () => auth,
      options: { fetch: async () => json(501, {}) },
    });
    await chat.loadThreads();
    expect(chat.getState().threads).toEqual([]);
    expect(chat.getState().error).toBeNull();
  });
});

describe("thread actions", () => {
  const seeded = async () => {
    const store = memoryStore();
    await store.save("u", {
      key: "k1",
      remoteId: "c1",
      title: "Mine",
      updatedAt: 1000,
      synced: 2,
      messages: [{ role: "user", content: "a" }],
    });
    const calls: string[] = [];
    const chat = createCloudChatController({
      getAuth: async () => auth,
      store,
      options: {
        fetch: async (url, init) => {
          calls.push(`${init?.method ?? "GET"} ${url.replace("https://api.hanzo.ai", "")}`);
          return url.endsWith("/conversations")
            ? json(200, {
                conversations: [
                  { id: "c1", title: "Server title", updatedAt: new Date(5000).toISOString() },
                  { id: "c2", title: "Other", updatedAt: new Date(3000).toISOString() },
                ],
              })
            : json(200, {});
        },
      },
    });
    await chat.loadThreads();
    return { store, chat, calls };
  };

  it("renames locally and the remote title no longer replaces it", async () => {
    const { store, chat } = await seeded();
    await chat.renameThread("k1", "  Launch plan ");
    expect(chat.getState().threads.find((t) => t.key === "k1")?.title).toBe("Launch plan");
    expect(store.rows.get("k1")).toMatchObject({ title: "Launch plan", renamed: true });
    await chat.loadThreads();
    expect(chat.getState().threads.find((t) => t.key === "k1")?.title).toBe("Launch plan");
  });

  it("pins a remote-only thread to the top and keeps it across reloads", async () => {
    const { store, chat } = await seeded();
    await chat.pinThread("c2", true);
    expect(chat.getState().threads[0]?.key).toBe("c2");
    expect(store.rows.get("c2")).toMatchObject({ pinned: true, remoteId: "c2", messages: [] });
    await chat.loadThreads();
    expect(chat.getState().threads[0]?.key).toBe("c2");
  });

  it("deletes: hides it, asks the backend, and stays hidden when the backend still lists it", async () => {
    const { store, chat, calls } = await seeded();
    await chat.deleteThread("k1");
    expect(chat.getState().threads.map((t) => t.key)).toEqual(["c2"]);
    expect(calls).toContain("DELETE /v1/agents/chat/conversations/c1");
    expect(store.rows.get("k1")).toMatchObject({ deleted: true });
    await chat.loadThreads();
    expect(chat.getState().threads.map((t) => t.key)).toEqual(["c2"]);
  });
});

describe("private chat", () => {
  const reply = () =>
    new Response(
      "data: " + JSON.stringify({ choices: [{ delta: { content: "ok" } }] }) + "\n\ndata: [DONE]\n\n",
      { status: 200, headers: { "content-type": "text/event-stream" } },
    );
  it("is never listed, saved or recorded, and a new conversation is normal again", async () => {
    const urls: string[] = [];
    const saved: unknown[] = [];
    const controller = createCloudChatController({
      getAuth: async () => auth,
      options: {
        fetch: async (url: string) => {
          urls.push(String(url));
          return String(url).includes("/chat/completions") ? reply() : json(200, { conversationId: "c-1" });
        },
      },
      store: { load: async () => [], save: async (_o, thread) => void saved.push(thread) },
    });
    controller.setGhost(true);
    await controller.send("secret");
    expect(controller.getState().messages.at(-1)?.content).toBe("ok");
    expect(controller.getState().threads).toEqual([]);
    expect(saved).toEqual([]);
    expect(urls.some((u) => u.includes("/agents/chat/conversations"))).toBe(false);
    controller.reset();
    expect(controller.getState().ghost).toBe(false);
    await controller.send("kept");
    expect(controller.getState().threads).toHaveLength(1);
    expect(urls.some((u) => u.includes("/agents/chat/conversations"))).toBe(true);
  });
});
