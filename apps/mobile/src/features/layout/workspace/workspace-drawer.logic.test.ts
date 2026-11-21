import { describe, expect, it } from "vite-plus/test";

import { NAVIGATION_TAB_ORDER } from "./navigation-tab-order";
import {
  ALL_DRAWER_DESTINATIONS,
  DRAWER_FLICK,
  barDestinations,
  drawerAxis,
  drawerDestinations,
  drawerPull,
  drawerReach,
  drawerReducer,
  drawerSettles,
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

describe("drawer destinations", () => {
  it("lists every section and Settings by default", () => {
    expect(drawerDestinations(undefined, undefined, false)).toEqual(ALL_DRAWER_DESTINATIONS);
    expect(ALL_DRAWER_DESTINATIONS).toEqual([...NAVIGATION_TAB_ORDER, "settings"]);
  });

  it("is a superset of what the bar reached, whatever the user hid or reordered", () => {
    const orders = [undefined, ["usage", "issues"], ["issues", "threads", "usage", "pullRequests"]];
    const hiddens = [undefined, [], ["usage"], ["pullRequests", "issues"], ["threads"]];
    for (const order of orders) {
      for (const hidden of hiddens) {
        const drawer = new Set(drawerDestinations(order, hidden, true));
        for (const destination of barDestinations(order, hidden)) {
          expect(drawer.has(destination)).toBe(true);
        }
        expect(drawer.has("settings")).toBe(true);
      }
    }
  });

  it("keeps the order the user chose and drops only what they hid", () => {
    expect(drawerDestinations(["usage", "threads"], ["issues"], true)).toEqual([
      "usage",
      "threads",
      "pullRequests",
      "settings",
    ]);
  });
});
