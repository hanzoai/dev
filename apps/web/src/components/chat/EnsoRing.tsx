import { ensoRingPath } from "@hanzodev/client-runtime/enso-ring";

const BOX = 16;
const STROKE = 1.75;

/**
 * The Enso mark: one arc on a 16-unit box, open by `gap` (0 closed, up to half). Static and
 * monochrome, so it takes the text color and costs nothing to paint.
 */
export function EnsoRing(props: { gap: number; dot?: boolean; className?: string }) {
  return (
    <svg
      aria-hidden="true"
      viewBox={`0 0 ${BOX} ${BOX}`}
      fill="none"
      stroke="currentColor"
      strokeWidth={STROKE}
      strokeLinecap="round"
      className={props.className ?? "size-4"}
      // The composer control CSS forces unsized SVGs to 16px.
      data-enso-ring={props.dot ? "max" : props.gap}
    >
      <path d={ensoRingPath(props.gap, BOX, STROKE)} />
      {props.dot ? (
        <circle cx={BOX / 2} cy={BOX / 2} r={1.75} fill="currentColor" stroke="none" />
      ) : null}
    </svg>
  );
}
