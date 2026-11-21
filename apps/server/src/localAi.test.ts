import { describe, expect, it } from "vitest";
import type { LocalEngineStatus } from "@hanzodev/shared/localAi";

import { engineFor, modelsBody, parseChatBody } from "./localAi.ts";

const status = (name: string, baseUrl: string, ids: ReadonlyArray<string>): LocalEngineStatus => ({
  engine: { name, kind: "openai", baseUrl },
  online: true,
  baseUrl,
  latencyMs: 1,
  models: ids.map((id) => ({ id, zen: false })),
});

const engines = [
  status("Hanzo Engine", "http://127.0.0.1:3690/v1/engine", ["zen-eco-3b-agent"]),
  status("Ollama", "http://127.0.0.1:11434/v1", ["zen-eco-3b-agent", "llama"]),
];

describe("local chat", () => {
  it("lists each model once, owned by the first engine that serves it", () => {
    expect(modelsBody(engines)).toEqual({
      data: [
        { id: "zen-eco-3b-agent", engine: "Hanzo Engine" },
        { id: "llama", engine: "Ollama" },
      ],
    });
  });

  it("routes a model to the first engine that serves it, and to none when none does", () => {
    expect(engineFor(engines, "zen-eco-3b-agent")).toBe("http://127.0.0.1:3690/v1/engine");
    expect(engineFor(engines, "llama")).toBe("http://127.0.0.1:11434/v1");
    expect(engineFor(engines, "gpt-4o")).toBeNull();
  });

  it("accepts only a model with messages", () => {
    expect(parseChatBody({ model: "m", messages: [{ role: "user", content: "hi" }] })).toEqual({
      model: "m",
      messages: [{ role: "user", content: "hi" }],
    });
    expect(parseChatBody({ model: "", messages: [{}] })).toBeNull();
    expect(parseChatBody({ model: "m", messages: [] })).toBeNull();
    expect(parseChatBody(null)).toBeNull();
  });
});
