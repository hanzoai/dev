import type { ColorValue } from "react-native";
import Svg, { Path } from "react-native-svg";
import { withUniwind } from "uniwind";

const DEV = "Dev";

const ThemedPath = withUniwind(Path);

/** The Dev mark `</>`, the same strokes as the desktop tray glyph. */
export function DevMark(props: {
  readonly height: number;
  readonly color?: ColorValue;
  readonly colorClassName?: string;
}) {
  return (
    <Svg
      accessibilityLabel={DEV}
      height={props.height}
      width={props.height * 1.2}
      viewBox="0 0 36 34"
    >
      <ThemedPath
        d="M10.5 9.5L3.5 17.5L10.5 25.5M24.5 9.5L31.5 17.5L24.5 25.5M20.5 7.5L14.5 27.5"
        color={props.color}
        colorClassName={props.colorClassName}
        fill="none"
        stroke="currentColor"
        strokeWidth={3}
      />
    </Svg>
  );
}
