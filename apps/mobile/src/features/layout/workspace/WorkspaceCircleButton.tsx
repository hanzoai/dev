import type { ReactNode } from "react";
import { Pressable, View } from "react-native";

import { SymbolView } from "../../../components/ui/AppSymbol";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { GlassSurface } from "../../../components/ui/GlassSurface";

/** One glass circle for both floating header buttons. */
function CircleButton(props: {
  readonly accessibilityLabel: string;
  readonly onPress: () => void;
  readonly children: ReactNode;
}) {
  return (
    <GlassSurface
      chrome="none"
      style={{
        width: 46,
        height: 46,
        borderRadius: 23,
        borderCurve: "circular",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <Pressable
        accessibilityRole="button"
        accessibilityLabel={props.accessibilityLabel}
        onPress={props.onPress}
        hitSlop={4}
        style={{
          width: 46,
          height: 46,
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        {props.children}
      </Pressable>
    </GlassSurface>
  );
}

export function WorkspaceMenuButton(props: {
  readonly accessibilityLabel: string;
  readonly onPress: () => void;
}) {
  const { colors } = useControlTheme();
  const line = {
    width: 21,
    height: 2.25,
    borderRadius: 1.125,
    backgroundColor: colors["--color-foreground"],
  };
  return (
    <CircleButton {...props}>
      <View style={{ alignItems: "center", justifyContent: "center", gap: 6 }}>
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
      <SymbolView
        name="square.and.pencil"
        size={22}
        weight="medium"
        resizeMode="scaleAspectFit"
        tintColor={colors["--color-foreground"]}
        style={{ width: 22, height: 22, transform: [{ translateX: 1 }, { translateY: -0.5 }] }}
      />
    </CircleButton>
  );
}
