import { describe, expect, it } from "vite-plus/test";

import { NAVIGATION_TAB_ORDER } from "./navigation-tab-order";
import {
  DEFAULT_SECTION,
  INITIAL_SECTION_STATE,
  SECTION_ICONS,
  WORKSPACE_SECTIONS,
  devContents,
  isWebSection,
  sectionOfTab,
  sectionReducer,
} from "./workspace-section.logic";

describe("drawer sections", () => {
  it("are hanzo.app's pinned nav, in its order, with an icon each", () => {
    expect(WORKSPACE_SECTIONS).toEqual(["chat", "dev", "design", "library", "tabs"]);
    expect(Object.keys(SECTION_ICONS)).toEqual([...WORKSPACE_SECTIONS]);
  });

  it("opens on chat", () => {
    expect(DEFAULT_SECTION).toBe("chat");
    expect(INITIAL_SECTION_STATE.section).toBe("chat");
  });

  it("draws only design, library and tabs in a web view", () => {
    expect(WORKSPACE_SECTIONS.filter(isWebSection)).toEqual(["design", "library", "tabs"]);
  });
});

describe("Dev content", () => {
  it("holds every list the app had, plus the linked Macs", () => {
    const contents = devContents(undefined, undefined);
    for (const tab of NAVIGATION_TAB_ORDER) expect(contents).toContain(tab);
    expect(contents.at(-1)).toBe("macs");
  });

  it("drops only what the user hid and keeps their order", () => {
    expect(devContents(["usage", "threads"], ["issues"])).toEqual([
      "usage",
      "threads",
      "pullRequests",
      "macs",
    ]);
  });

  it("puts pull requests, issues and usage in Dev; threads stay where they were asked", () => {
    expect(sectionOfTab("pullRequests", "chat")).toBe("dev");
    expect(sectionOfTab("issues", "chat")).toBe("dev");
    expect(sectionOfTab("usage", "chat")).toBe("dev");
    expect(sectionOfTab("threads", "chat")).toBe("chat");
    expect(sectionOfTab("threads", "dev")).toBe("dev");
    expect(sectionOfTab("threads", "design")).toBe("design");
  });
});

describe("section reducer", () => {
  it("opens a section and leaves the Macs view with Dev", () => {
    let state = sectionReducer(INITIAL_SECTION_STATE, { type: "open", section: "dev" });
    state = sectionReducer(state, { type: "macs", macs: true });
    expect(state).toEqual({ section: "dev", macs: true });
    expect(sectionReducer(state, { type: "open", section: "dev" })).toBe(state);
    expect(sectionReducer(state, { type: "open", section: "tabs" })).toEqual({
      section: "tabs",
      macs: false,
    });
  });
});
