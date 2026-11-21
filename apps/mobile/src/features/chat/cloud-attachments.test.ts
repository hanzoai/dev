import { describe, expect, it } from "vite-plus/test";

import { cloudAttachmentKind } from "./cloud-attachments";

const file = (name: string, mimeType: string) =>
  ({ id: "1", type: "file", name, mimeType, sizeBytes: 10_000_000, fileUri: "file:///x" }) as const;

describe("cloud chat attachments", () => {
  it("sends text and PDFs, and refuses what a chat cannot read", () => {
    expect(cloudAttachmentKind(file("notes.md", "application/octet-stream"))).toBe("text");
    expect(cloudAttachmentKind(file("data.json", "application/json"))).toBe("text");
    expect(cloudAttachmentKind(file("paper.pdf", "application/pdf"))).toBe("pdf");
    expect(cloudAttachmentKind(file("clip.mov", "video/quicktime"))).toBe("other");
  });
});
