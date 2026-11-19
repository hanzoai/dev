import type { ProjectIconOverride } from "@hanzodev/contracts";
import { describe, expect, it } from "vite-plus/test";

import { projectAutoMonogram, resolveProjectChipIcon } from "./projectIcon";

const project = (title: string, projectIcon?: ProjectIconOverride) => ({
  title,
  workspaceRoot: `/work/${title}`,
  projectIcon,
});

describe("resolveProjectChipIcon", () => {
  const favicon = () => "https://env.example/favicon.png";
  const noFavicon = () => null;

  it("shows the chat bubble for unknown threads and the no-project folder", () => {
    expect(resolveProjectChipIcon(null, "/scratch", favicon)).toBeNull();
    expect(resolveProjectChipIcon(project("scratch"), "/work/scratch", favicon)).toBeNull();
  });

  it("prefers a custom icon over the favicon", () => {
    expect(
      resolveProjectChipIcon(
        project("hanzodev", { kind: "monogram", text: "Q7", color: "violet" }),
        null,
        favicon,
      ),
    ).toEqual({ kind: "monogram", text: "Q7", color: "violet" });
    expect(
      resolveProjectChipIcon(project("hanzodev", { kind: "emoji", emoji: "🚀" }), null, favicon),
    ).toEqual({ kind: "emoji", emoji: "🚀" });
    expect(
      resolveProjectChipIcon(
        project("lucide proj", { kind: "lucide", name: "rocket", color: "emerald" }),
        null,
        favicon,
      ),
    ).toEqual({ kind: "monogram", text: "LP", color: "emerald" });
  });

  it("uses the favicon, then a monogram of the title", () => {
    expect(resolveProjectChipIcon(project("hanzodev"), null, favicon)).toEqual({
      kind: "favicon",
      uri: "https://env.example/favicon.png",
    });
    expect(resolveProjectChipIcon(project("Auto Monogram"), null, noFavicon)).toEqual({
      kind: "monogram",
      text: "AM",
      color: "lime",
    });
    expect(
      resolveProjectChipIcon(
        project("hanzodev"),
        null,
        () => "https://env.example/v1/assets/project-favicon-missing",
      ),
    ).toMatchObject({ kind: "monogram" });
  });
});

describe("projectAutoMonogram", () => {
  it("matches desktop's deriveProjectIdentity", () => {
    const desktop = [
      ["hanzodev", "HV", "rose"],
      ["Auto Monogram", "AM", "lime"],
      ["blitzdiff", "BF", "pink"],
      ["uoalbumdotclub", "UB", "gray"],
      ["  Tagium  ", "TM", "lime"],
      ["Café Ünïcode", "CÜ", "fuchsia"],
      ["web2 app", "W2", "emerald"],
    ] as const;
    for (const [title, text, color] of desktop) {
      expect(projectAutoMonogram(title)).toEqual({ kind: "monogram", text, color });
    }
  });

  it("leaves untitled projects to the folder icon, like desktop", () => {
    expect(projectAutoMonogram("")).toBeNull();
    expect(projectAutoMonogram("   ")).toBeNull();
  });
});
