import { describe, expect, it } from "vite-plus/test";

import {
  DRAWER_FLICK,
  drawerAccountCaption,
  drawerAccountInitial,
  drawerAxis,
  drawerPull,
  drawerReach,
  drawerReducer,
  drawerSettles,
  drawerAccountName,
  drawerThreadRows,
  floatingHeaderTop,
  FLOATING_HEADER_HEIGHT,
  phoneHeaderMode,
  workspaceHeaderShown,
} from "./workspace-drawer.logic";

describe("drawer geometry", () => {
  it("leaves a strip of the pane and caps the width", () => {
    expect(drawerReach(390)).toBe(326);
    expect(drawerReach(1000)).toBe(360);
    expect(drawerReach(40)).toBe(0);
  });

  it("holds the pane between home and open", () => {
    expect(drawerPull(0, -30, 326)).toBe(0);
    expect(drawerPull(0, 100, 326)).toBe(100);
    expect(drawerPull(300, 100, 326)).toBe(326);
  });

  it("settles by flick first, then by half", () => {
    expect(drawerSettles(10, 326, DRAWER_FLICK)).toBe(true);
    expect(drawerSettles(320, 326, -DRAWER_FLICK)).toBe(false);
    expect(drawerSettles(200, 326, 0)).toBe(true);
    expect(drawerSettles(100, 326, 0)).toBe(false);
  });

  it("tells a pull from a scroll", () => {
    expect(drawerAxis(3, 3)).toBeNull();
    expect(drawerAxis(20, 5)).toBe("x");
    expect(drawerAxis(20, 20)).toBe("y");
  });
});

describe("drawer open state", () => {
  it("opens, closes, toggles, and stays referentially stable when nothing changes", () => {
    const closed = { open: false };
    const open = drawerReducer(closed, { type: "open" });
    expect(open).toEqual({ open: true });
    expect(drawerReducer(open, { type: "open" })).toBe(open);
    expect(drawerReducer(open, { type: "close" })).toEqual({ open: false });
    expect(drawerReducer(closed, { type: "close" })).toBe(closed);
    expect(drawerReducer(closed, { type: "toggle" })).toEqual({ open: true });
  });
});

describe("drawer account", () => {
  it("calls only the reserved admin org SuperAdmin", () => {
    expect(drawerAccountCaption({ owner: "admin", planName: "Pro" }).kind).toBe("superAdmin");
    expect(drawerAccountCaption({ owner: "hanzo", planName: "Pro" })).toEqual({
      kind: "plan",
      plan: "Pro",
    });
  });

  it("takes an initial from the name", () => {
    expect(drawerAccountInitial("  zed")).toBe("Z");
    expect(drawerAccountInitial("")).toBe("?");
  });
});

describe("phone header mode", () => {
  const home = { drawer: true, settingsOpen: false, section: "chat" as const };
  it("floats on Chat and the web sections", () => {
    expect(phoneHeaderMode(home)).toBe("floating");
    expect(phoneHeaderMode({ ...home, settingsOpen: true })).toBe("native");
    expect(phoneHeaderMode({ ...home, section: "dev" })).toBe("native");
    expect(phoneHeaderMode({ ...home, section: "design" })).toBe("floating");
    expect(phoneHeaderMode({ ...home, drawer: false })).toBe("native");
  });
  it("hides the native header only when floating", () => {
    expect(workspaceHeaderShown("floating")).toBe(false);
    expect(workspaceHeaderShown("native")).toBe(true);
  });
  it("pushes content below the circles only when floating", () => {
    expect(floatingHeaderTop(47, "floating")).toBe(47 + FLOATING_HEADER_HEIGHT);
    expect(floatingHeaderTop(47, "native")).toBe(47);
  });
});

describe("drawer threads", () => {
  const threads = [
    { key: "a", title: "Plan the launch" },
    { key: "b", title: "" },
    { key: "c", title: "Fix login" },
  ];
  it("names untitled threads and keeps order", () => {
    expect(drawerThreadRows(threads, "", "New chat").map((r) => r.title)).toEqual([
      "Plan the launch",
      "New chat",
      "Fix login",
    ]);
  });
  it("filters by the search field, ignoring case and padding", () => {
    expect(drawerThreadRows(threads, "  LOGIN ", "New chat")).toEqual([
      { key: "c", title: "Fix login", pinned: false },
    ]);
  });
});

describe("drawer account name", () => {
  const base = { displayName: null, email: null, subject: "z" };
  it("prefers the display name, then email, then subject", () => {
    expect(drawerAccountName({ ...base, displayName: " Z A ", email: "z@x.io" }, "Hanzo")).toBe(
      "Z A",
    );
    expect(drawerAccountName({ ...base, email: "z@x.io" }, "Hanzo")).toBe("z@x.io");
    expect(drawerAccountName(base, "Hanzo")).toBe("z");
  });
  it("falls back only with no account", () => {
    expect(drawerAccountName(null, "Hanzo")).toBe("Hanzo");
  });
});
