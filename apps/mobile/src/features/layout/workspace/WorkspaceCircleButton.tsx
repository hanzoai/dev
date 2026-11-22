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
        width: 44,
        height: 44,
        borderRadius: 22,
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
          width: 44,
          height: 44,
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
    width: 18,
    height: 2,
    borderRadius: 1,
    backgroundColor: colors["--color-foreground"],
  };
  return (
    <CircleButton {...props}>
      <View style={{ alignItems: "center", justifyContent: "center", gap: 5 }}>
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
        size={19}
        resizeMode="scaleAspectFit"
        tintColor={colors["--color-foreground"]}
        style={{ width: 19, height: 19 }}
      />
    </CircleButton>
  );
}
