import { describe, expect, it } from "vite-plus/test";

import { parseProposedVoiceAction } from "./VoiceActionReviewCard";

describe("parseProposedVoiceAction", () => {
  it("parses voice assignment commands with matched target, owner, and summary", () => {
    const parsed = parseProposedVoiceAction("assign this deal to Sal");
    expect(parsed).toEqual({
      kind: "assign",
      target: "deal",
      owner: "Sal",
      summary: 'Assign "deal" -> Sal',
      rawText: "assign this deal to Sal",
    });
  });

  it("parses status transitions and organization switches while ignoring regular chat prompts", () => {
    expect(parseProposedVoiceAction("move issue #42 to in progress")).toMatchObject({
      kind: "status",
      target: "issue #42",
      summary: 'Update "issue #42" status -> in progress',
    });

    expect(parseProposedVoiceAction("switch organization to lux")).toMatchObject({
      kind: "switch_org",
      owner: "lux",
    });

    expect(parseProposedVoiceAction("explain how useEffect works in React")).toBeNull();
  });
});
