import { ensoRingPath } from "@hanzodev/client-runtime/enso-ring";
import { Circle, Path, Svg } from "react-native-svg";

const BOX = 16;
const STROKE = 1.75;

/** The Enso mark: one static arc, open by `gap` (0 closed, up to half), in the given color. */
export function EnsoRing(props: {
  readonly color: string;
  readonly gap: number;
  readonly dot?: boolean;
  readonly size: number;
}) {
  return (
    <Svg width={props.size} height={props.size} viewBox={`0 0 ${BOX} ${BOX}`} fill="none">
      <Path
        d={ensoRingPath(props.gap, BOX, STROKE)}
        stroke={props.color}
        strokeWidth={STROKE}
        strokeLinecap="round"
      />
      {props.dot ? <Circle cx={BOX / 2} cy={BOX / 2} r={1.75} fill={props.color} /> : null}
    </Svg>
  );
}
