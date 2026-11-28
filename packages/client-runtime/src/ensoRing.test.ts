import { describe, expect, it } from "vite-plus/test";

import {
  ENSO_MAX_GAP,
  ENSO_MODEL_GAPS,
  ensoModelGap,
  ensoRingDot,
  ensoRingGap,
  ensoRingPath,
  isEnsoModel,
} from "./ensoRing.ts";

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

  it("leaves the gap at the upper right", () => {
    const d = ensoRingPath(0.125, 16, 2);
    const [, sx, sy] = /^M([\d.]+) ([\d.]+)/.exec(d)!;
    const [, ex, ey] = /([\d.]+) ([\d.]+)$/.exec(d)!;
    // The stroke starts below the gap on the right and ends along the top, left of it.
    expect(Number(sx)).toBeGreaterThan(8);
    expect(Number(sy)).toBeGreaterThan(Number(ey));
    expect(Number(ex)).toBeGreaterThan(8);
  });
});

describe("ensoRingDot", () => {
  it("marks Max alone, so Auto stays a plain closed ring", () => {
    expect(ensoRingDot("max")).toBe(true);
    expect([null, "auto", "default", "high"].some(ensoRingDot)).toBe(false);
  });
});

describe("isEnsoModel", () => {
  it.each([
    ["enso", true],
    ["enso-auto", true],
    ["enso-max", true],
    ["hanzo/enso-ultra", true],
    ["HANZO/ENSO-FLASH", true],
    ["zen6", false],
    ["ensorcelled", false],
  ])("%s -> %s", (slug, expected) => expect(isEnsoModel(slug)).toBe(expected));
});

describe("ensoModelGap", () => {
  it("closes Enso and Enso Auto and opens the remaining tiers from Ultra to Flash", () => {
    expect(ENSO_MODEL_GAPS).toEqual({
      "enso-auto": 0,
      "enso-ultra": 0.08,
      "enso-max": 0.16,
      "enso-pro": 0.24,
      enso: 0,
      "enso-flash": 0.42,
    });
    const order = ["enso-auto", "enso-ultra", "enso-max", "enso-pro", "enso-flash"];
    const gaps = order.map(ensoModelGap);
    expect(gaps[0]).toBe(0);
    expect(ensoModelGap("enso")).toBe(0);
    expect(ensoModelGap("enso-hybrid")).toBeGreaterThan(0);
    expect(ensoModelGap("hanzo/enso-preview")).toBeGreaterThan(0);
    for (let i = 1; i < gaps.length; i++) {
      expect(gaps[i]!).toBeGreaterThan(gaps[i - 1]!);
    }
  });

  it("normalizes hanzo/ prefixes and mixed case", () => {
    expect(ensoModelGap("hanzo/enso-auto")).toBe(0);
    expect(ensoModelGap("hanzo/enso-ultra")).toBe(0.08);
    expect(ensoModelGap("HANZO/ENSO-MAX")).toBe(0.16);
    expect(ensoModelGap("Enso-Pro")).toBe(0.24);
    expect(ensoModelGap("hanzo/enso")).toBe(0);
    expect(ensoModelGap("hanzo/enso-flash")).toBe(0.42);
  });
});
