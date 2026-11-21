import { describe, expect, it } from "vite-plus/test";

import { ENSO_MAX_GAP, ensoRingGap, ensoRingPath, isEnsoModel } from "./ensoRing.ts";

const LEVELS = ["low", "medium", "high"];

describe("ensoRingGap", () => {
  it.each([
    { current: "low", gap: (ENSO_MAX_GAP * 3) / 4 },
    { current: "medium", gap: (ENSO_MAX_GAP * 2) / 4 },
    { current: "high", gap: (ENSO_MAX_GAP * 1) / 4 },
    { current: "max", gap: 0 },
    { current: "default", gap: 0 },
    { current: null, gap: 0 },
    { current: "unheard-of", gap: 0 },
  ])("maps $current to a gap of $gap", ({ current, gap }) => {
    expect(ensoRingGap(LEVELS, current)).toBeCloseTo(gap);
  });

  it("closes more as effort rises", () => {
    const gaps = LEVELS.map((level) => ensoRingGap(LEVELS, level));
    expect(gaps).toEqual([...gaps].sort((a, b) => b - a));
    expect(new Set(gaps).size).toBe(3);
  });

  it("ignores Max when counting ordinary levels", () => {
    expect(ensoRingGap([...LEVELS, "max"], "high")).toBeCloseTo(ENSO_MAX_GAP / 4);
  });
});

describe("ensoRingPath", () => {
  it("draws the closed ring as a full circle", () => {
    expect(ensoRingPath(0, 16, 2)).toMatch(/^M1 8a7 7 0 1 0 14 0a7 7 0 1 0 -14 0$/);
  });

  it("leaves the gap at the top", () => {
    const [, x1] = /^M([\d.]+) /.exec(ensoRingPath(0.25, 16, 2))!;
    const [, x2] = /A.* ([\d.]+) [\d.]+$/.exec(ensoRingPath(0.25, 16, 2))!;
    expect(Number(x1)).toBeGreaterThan(8);
    expect(Number(x2)).toBeLessThan(8);
  });
});

describe("isEnsoModel", () => {
  it.each([
    ["enso", true],
    ["enso-auto", true],
    ["enso-max", true],
    ["zen6", false],
    ["ensorcelled", false],
  ])("%s -> %s", (slug, expected) => expect(isEnsoModel(slug)).toBe(expected));
});
