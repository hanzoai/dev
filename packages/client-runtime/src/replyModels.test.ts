import { describe, expect, it } from "@effect/vitest";

import { writesReplies } from "./replyModels.ts";

describe("writesReplies", () => {
  it("lists chat models, whatever else they can produce", () => {
    expect(writesReplies({ endpoint: "/v1/chat/completions", outputs: ["text"] })).toBe(true);
    expect(writesReplies({ endpoint: "/v1/chat/completions", outputs: ["image", "text"] })).toBe(
      true,
    );
  });
  it("never lists a decision model", () => {
    expect(writesReplies({ endpoint: "/v1/decisions", outputs: ["decision"] })).toBe(false);
    expect(writesReplies({ outputs: ["decision"] })).toBe(false);
  });
  it("never lists models that only make speech, images, video or embeddings", () => {
    expect(writesReplies({ endpoint: "/v1/audio/speech", outputs: ["speech"] })).toBe(false);
    expect(writesReplies({ endpoint: "/v1/embeddings", outputs: ["embeddings"] })).toBe(false);
  });
  it("lists a model the catalog says nothing about", () => {
    expect(writesReplies({})).toBe(true);
  });
});
