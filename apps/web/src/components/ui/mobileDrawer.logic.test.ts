import { describe, expect, it } from "vite-plus/test";

import {
  DRAWER_FLICK,
  drawerAccountCaption,
  drawerAccountInitial,
  drawerAxis,
  drawerDim,
  drawerGrabs,
  drawerPull,
  drawerReach,
  drawerReducer,
  drawerSettles,
} from "./mobileDrawer.logic";

describe("drawer geometry", () => {
  it("leaves a strip of the page and caps the width", () => {
    expect(drawerReach(390)).toBe(326);
    expect(drawerReach(767)).toBe(360);
    expect(drawerReach(40)).toBe(0);
  });

  it("holds the drawer between closed and open", () => {
    expect(drawerPull(0, -30, 326)).toBe(0);
    expect(drawerPull(0, 100, 326)).toBe(100);
    expect(drawerPull(300, 100, 326)).toBe(326);
    expect(drawerPull(326, -100, 326)).toBe(226);
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
    expect(drawerAxis(-20, 5)).toBe("x");
  });

  it("starts a drag at the edge when closed and anywhere when open", () => {
    expect(drawerGrabs(10, false)).toBe(true);
    expect(drawerGrabs(80, false)).toBe(false);
    expect(drawerGrabs(200, true)).toBe(true);
  });

  it("dims in step with the slide", () => {
    expect(drawerDim(0, 326)).toBe(0);
    expect(drawerDim(163, 326)).toBeCloseTo(0.225);
    expect(drawerDim(0, 0)).toBe(0);
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
    expect(drawerAccountCaption({ owner: undefined, planName: null }).kind).toBe("plan");
  });

  it("falls back to an initial", () => {
    expect(drawerAccountInitial("  zed")).toBe("Z");
    expect(drawerAccountInitial("")).toBe("?");
  });
});
