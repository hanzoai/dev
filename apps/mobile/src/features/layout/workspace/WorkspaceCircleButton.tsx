import type { ReactNode } from "react";
import { Pressable, View } from "react-native";

import { SymbolView } from "../../../components/ui/AppSymbol";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { NATIVE_LIQUID_GLASS_SUPPORTED } from "../../../native/native-glass";

/** One circle for both header buttons. Liquid Glass draws the circle; before it, this does. */
function CircleButton(props: {
  readonly accessibilityLabel: string;
  readonly onPress: () => void;
  readonly children: ReactNode;
}) {
  return (
    <Pressable
      accessibilityRole="button"
      accessibilityLabel={props.accessibilityLabel}
      onPress={props.onPress}
      hitSlop={4}
      className={
        NATIVE_LIQUID_GLASS_SUPPORTED
          ? "h-10 w-10 items-center justify-center"
          : "h-10 w-10 items-center justify-center rounded-full bg-grouped-card"
      }
    >
      {props.children}
    </Pressable>
  );
}

export function WorkspaceMenuButton(props: {
  readonly accessibilityLabel: string;
  readonly onPress: () => void;
}) {
  const { colors } = useControlTheme();
  const line = {
    width: 18,
    height: 2,
    borderRadius: 1,
    backgroundColor: colors["--color-foreground"],
  };
  return (
    <CircleButton {...props}>
      <View style={{ gap: 5 }}>
        <View style={line} />
        <View style={line} />
      </View>
    </CircleButton>
  );
}

export function WorkspaceComposeButton(props: {
  readonly accessibilityLabel: string;
  readonly onPress: () => void;
}) {
  const { colors } = useControlTheme();
  return (
    <CircleButton {...props}>
      <SymbolView name="square.and.pencil" size={18} tintColor={colors["--color-foreground"]} />
    </CircleButton>
  );
}
