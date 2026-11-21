import { describe, expect, it } from "vite-plus/test";

import { readAttachments } from "./homeAttachments";

describe("readAttachments", () => {
  it("reads a text file inline, an image as a data URL, and skips a file with no bytes", async () => {
    const out = await readAttachments(
      [{ file: new File([new Uint8Array([137, 80])], "a.png", { type: "image/png" }) } as never],
      [
        { file: new File(["hello"], "n.md", { type: "text/markdown" }) } as never,
        { file: new File([new Uint8Array([1])], "d.pdf", { type: "application/pdf" }) } as never,
        { file: null } as never,
      ],
    );
    expect(out.map((a) => a.kind)).toEqual(["image", "text", "file"]);
    expect(out[0]).toMatchObject({ dataUrl: expect.stringMatching(/^data:image\/png;base64,/) });
    expect(out[1]).toEqual({ kind: "text", name: "n.md", text: "hello" });
  });
});
