/**
 * The ensō ring: one arc whose open gap shows effort. A closed ring is Auto. Ordinary levels
 * (Low, Medium, High) open the ring from half at the lowest down to a sliver at the highest; Max
 * and the agent's own default close it.
 */

/** Share of the circle left open at the lowest effort. */
export const ENSO_MAX_GAP = 0.5;

const CLOSED_LEVELS = new Set(["auto", "default", "max", "ultracode"]);
const OFF_LEVELS = new Set(["off", "none"]);

/** Enso is Hanzo's router family: `enso`, `enso-auto`, `enso-flash`, `enso-ultra`, `enso-max`. */
export function isEnsoModel(slug: string): boolean {
  return /^enso($|-)/i.test(slug);
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

/**
 * SVG path of the ring on a square box of side `size`. The gap is centered at the top and the
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
  // Angles measured from the top, clockwise.
  const point = (angle: number) =>
    `${fixed(c + r * Math.sin(angle))} ${fixed(c - r * Math.cos(angle))}`;
  const large = 2 * Math.PI - 2 * half > Math.PI ? 1 : 0;
  return `M${point(half)}A${fixed(r)} ${fixed(r)} 0 ${large} 1 ${point(2 * Math.PI - half)}`;
}
