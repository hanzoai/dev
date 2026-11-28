/**
 * The ensō ring: one arc whose open gap shows effort. A plain closed ring is Auto. Ordinary levels
 * (Low, Medium, High) open the ring from a third of the circle at the lowest down to a sliver at
 * the highest; Max closes it and adds a center dot.
 */

/** Share of the circle left open at the lowest effort. */
export const ENSO_MAX_GAP = 0.5;

const CLOSED_LEVELS = new Set(["auto", "default", "max", "ultracode"]);
/** Where the gap is centered, clockwise from the top: 1:30 on a clock face. */
const GAP_CENTER = Math.PI / 4;
const OFF_LEVELS = new Set(["off", "none"]);

/** Normalize an Enso slug: trim, lowercase, and strip any `hanzo/` router prefix. */
function normalizeEnsoSlug(slug: string): string {
  return slug
    .trim()
    .toLowerCase()
    .replace(/^hanzo\//, "");
}

/**
 * Enso and Enso Auto use a closed ring. Other tiers open progressively from Ultra to Flash.
 */
export const ENSO_MODEL_GAPS: Readonly<Record<string, number>> = {
  "enso-auto": 0,
  "enso-ultra": 0.08,
  "enso-max": 0.16,
  "enso-pro": 0.24,
  enso: 0,
  "enso-flash": 0.42,
};

/** Enso is Hanzo's router family: `enso`, `enso-auto`, `enso-flash`, `enso-pro`, `enso-max`, `enso-ultra`. */
export function isEnsoModel(slug: string): boolean {
  return /^enso($|-)/i.test(normalizeEnsoSlug(slug));
}

/**
 * Unknown Enso tiers get an open ring so they cannot be mistaken for Enso or Enso Auto.
 */
export function ensoModelGap(slug: string): number {
  return ENSO_MODEL_GAPS[normalizeEnsoSlug(slug)] ?? 0.32;
}

/**
 * Open share of the ring, from 0 (closed) to `ENSO_MAX_GAP`. `levels` are the effort ids in
 * ascending order; an unknown or unset `current` is Auto and closes the ring.
 */
export function ensoRingGap(levels: ReadonlyArray<string>, current: string | null): number {
  if (current === null || CLOSED_LEVELS.has(current)) return 0;
  const ordinary = levels.filter((id) => !CLOSED_LEVELS.has(id));
  const index = ordinary.indexOf(current);
  if (index < 0) return 0;
  if (OFF_LEVELS.has(current)) return ENSO_MAX_GAP;
  return (ENSO_MAX_GAP * (ordinary.length - index)) / (ordinary.length + 1);
}

/** Max is the closed ring with a dot at its center, so it never reads as Auto. */
export function ensoRingDot(current: string | null): boolean {
  return current === "max";
}

/**
 * SVG path of the ring on a square box of side `size`. The gap is centered at the upper right and the
 * stroke runs clockwise around the rest; a closed ring is the full circle.
 */
export function ensoRingPath(gap: number, size: number, strokeWidth: number): string {
  const c = size / 2;
  const r = (size - strokeWidth) / 2;
  const fixed = (n: number) => Number(n.toFixed(3));
  if (gap <= 0) {
    return `M${fixed(c - r)} ${fixed(c)}a${fixed(r)} ${fixed(r)} 0 1 0 ${fixed(2 * r)} 0a${fixed(r)} ${fixed(r)} 0 1 0 ${fixed(-2 * r)} 0`;
  }
  const half = Math.PI * gap;
  // Angles measured from the top, clockwise; the stroke starts past the gap and runs around to it.
  const point = (angle: number) =>
    `${fixed(c + r * Math.sin(angle))} ${fixed(c - r * Math.cos(angle))}`;
  const large = 2 * Math.PI - 2 * half > Math.PI ? 1 : 0;
  return `M${point(GAP_CENTER + half)}A${fixed(r)} ${fixed(r)} 0 ${large} 1 ${point(GAP_CENTER - half + 2 * Math.PI)}`;
}
