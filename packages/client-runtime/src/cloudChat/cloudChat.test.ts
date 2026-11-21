import { describe, expect, it } from "vitest";

import {
  CloudChatError,
  createCloudChatController,
  createSseParser,
  listChatModels,
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
const frame = (text: string) => `data: ${JSON.stringify({ choices: [{ delta: { content: text } }] })}\n\n`;
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
    expect(parser.push(": ping\r\ndata: {bad\n" + 'data: {"choices":[{"delta":{"content":"x"}}]}')).toEqual([]);
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
});

describe("streamChatReply", () => {
  it("streams deltas across split chunks", async () => {
    const got: string[] = [];
    const reply = await streamChatReply({
      auth,
      model: "enso-flash",
      messages: [{ role: "user", content: "hi" }],
      onDelta: (t) => got.push(t),
      options: { fetch: async () => sse([frame("Hi").slice(0, 10), frame("Hi").slice(10), frame(" there"), "data: [DONE]\n\n"]) },
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
    expect(bodies[0]).toEqual({ messages: [{ role: "user", content: "Ping" }, { role: "assistant", content: "Pong" }] });
    expect(bodies[1]).toMatchObject({ conversationId: "c1" });
  });

  it.each([404, 500])("degrades to a local thread when sync answers %i", async (status) => {
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
  it.each([404, 501])("says unavailable on %i", async (status) => {
    expect(await runInCloud(auth, { fetch: async () => json(status, {}) })).toEqual({ kind: "unavailable" });
  });
  it("starts a session", async () => {
    const result = await runInCloud(auth, {
      fetch: async () => json(200, { id: "s1", zap: "zap://x" }),
    });
    expect(result).toEqual({ kind: "started", sessionId: "s1" });
  });
});
