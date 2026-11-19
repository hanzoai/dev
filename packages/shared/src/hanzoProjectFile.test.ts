import * as Schema from "effect/Schema";
import { describe, expect, it } from "vite-plus/test";

import { parseHanzoProjectFile, HanzoProjectFileFromJson } from "./hanzoProjectFile.ts";

const decodeJson = Schema.decodeUnknownSync(HanzoProjectFileFromJson);

describe("HanzoProjectFileFromJson", () => {
  it("decodes lenient JSONC with comments and trailing commas", () => {
    const decoded = decodeJson(`{
      // team scripts
      "iconPath": "assets/logo.svg",
      "scripts": [
        { "name": "Dev", "command": "pnpm dev", "stopOnSettle": true, },
      ],
    }`);

    expect(decoded.iconPath).toBe("assets/logo.svg");
    expect(decoded.scripts?.[0]).toEqual({ name: "Dev", command: "pnpm dev", stopOnSettle: true });
  });
});

describe("parseHanzoProjectFile", () => {
  it("reads project defaults from JSONC with a URL in a block comment", () => {
    expect(
      parseHanzoProjectFile(`{
        /* See https://dev.hanzo.ai/schema/hanzo.json */
        "defaultThreadEnvMode": "worktree"
      }`),
    ).toEqual({ defaultThreadEnvMode: "worktree" });
  });
});
