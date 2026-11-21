import { useEffect, useMemo, useState, type ReactNode } from "react";
import { Pressable, StyleSheet, View, useColorScheme, useWindowDimensions } from "react-native";
import { Gesture, GestureDetector } from "react-native-gesture-handler";
import Reanimated, {
  Easing,
  interpolate,
  ReduceMotion,
  type SharedValue,
  runOnJS,
  useAnimatedStyle,
  useSharedValue,
  withTiming,
} from "react-native-reanimated";
import { useGT } from "gt-react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import {
  DRAWER_CARD_RADIUS,
  DRAWER_CARD_SHADOW,
  DRAWER_DURATION,
  DRAWER_EDGE,
  drawerPull,
  drawerReach,
  drawerSettles,
} from "./workspace-drawer.logic";
import {
  closeWorkspaceDrawer,
  dispatchWorkspaceDrawer,
  useWorkspaceDrawerOpen,
} from "./workspace-drawer-store";

// hanzo.app's --ease-emphasis; its --duration-base is DRAWER_DURATION.
const DRAWER_EASING = Easing.bezier(0.16, 1, 0.3, 1);

/**
 * The phone's side drawer. The menu stands still at the leading edge; the pane slides toward the
 * trailing edge by one shared value that the finger, the menu button and a tap on the dimmed pane
 * all move. The pane's transform and the dim's opacity are the only animated properties.
 */
/** Shared-value writes live here, outside the component, where the React Compiler does not track them. */
function createDrawerGesture(input: {
  readonly enabled: boolean;
  readonly open: boolean;
  readonly reach: number;
  readonly at: SharedValue<number>;
  readonly target: SharedValue<number>;
  readonly from: SharedValue<number>;
}) {
  const { enabled, open, reach, at, target, from } = input;
  return (
    Gesture.Pan()
      .enabled(enabled)
      // Closed, only the leading edge begins a pull; open, anywhere on the dimmed pane closes.
      .hitSlop(open ? undefined : { left: 0, width: DRAWER_EDGE })
      .activeOffsetX([-8, 8])
      .failOffsetY([-16, 16])
      .onBegin(() => {
        from.value = at.value;
        // A finger catches a moving drawer where it is.
        at.value = at.value;
      })
      .onUpdate((event) => {
        at.value = drawerPull(from.value, event.translationX, reach);
      })
      .onEnd((event) => {
        const stays = drawerSettles(at.value, reach, event.velocityX / 1000);
        const next = stays ? reach : 0;
        target.value = next;
        at.value = withTiming(next, {
          duration: DRAWER_DURATION,
          easing: DRAWER_EASING,
          reduceMotion: ReduceMotion.System,
        });
        runOnJS(dispatchWorkspaceDrawer)({ type: stays ? "open" : "close" });
      })
  );
}

export function WorkspaceDrawer(props: {
  readonly children: ReactNode;
  readonly menu: ReactNode;
  /** Drawn on the card above the content; moves with it. */
  readonly floating?: ReactNode;
  /** Whether the edge swipe may begin; false while another screen owns the edge. */
  readonly enabled: boolean;
}) {
  const gt = useGT();
  const { width } = useWindowDimensions();
  const insets = useSafeAreaInsets();
  const isDark = useColorScheme() === "dark";
  const open = useWorkspaceDrawerOpen();
  const reach = drawerReach(width);
  const at = useSharedValue(0);
  const target = useSharedValue(0);
  const from = useSharedValue(0);
  const [interactive, setInteractive] = useState(false);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- drives the one shared value when the menu button or a close request changes the open state
  useEffect(() => {
    const next = open ? reach : 0;
    if (target.value !== next) {
      target.value = next;
      at.value = withTiming(next, {
        duration: DRAWER_DURATION,
        easing: DRAWER_EASING,
        reduceMotion: ReduceMotion.System,
      });
    }
    setInteractive(open);
  }, [open, reach, at, target]);

  const gesture = useMemo(
    () => createDrawerGesture({ enabled: props.enabled || open, open, reach, at, target, from }),
    [open, props.enabled, reach, at, target, from],
  );

  // The pane is a raised card: it slides, shrinks a little and rounds its corners on the one value.
  const paneStyle = useAnimatedStyle(() => {
    const progress = reach === 0 ? 0 : at.value / reach;
    return {
      transform: [{ translateX: at.value }],
      shadowOpacity:
        progress * (isDark ? DRAWER_CARD_SHADOW.opacityDark : DRAWER_CARD_SHADOW.opacityLight),
    };
  });
  // The floating header's circles leave as the drawer opens, on the same value.
  const floatingStyle = useAnimatedStyle(() => ({
    opacity: reach === 0 ? 1 : 1 - Math.min(1, at.value / reach),
  }));
  // Only the start corners round, as hanzo.app's pane does; a hairline edge rides with them.
  const clipStyle = useAnimatedStyle(() => {
    const radius = interpolate(reach === 0 ? 0 : at.value / reach, [0, 1], [0, DRAWER_CARD_RADIUS]);
    return { borderTopLeftRadius: radius, borderBottomLeftRadius: radius };
  });

  return (
    <View className="flex-1 bg-drawer">
      <View
        style={[styles.menu, { width: reach, paddingTop: insets.top }]}
        accessibilityElementsHidden={!open}
        importantForAccessibility={open ? "auto" : "no-hide-descendants"}
      >
        {props.menu}
      </View>
      <GestureDetector gesture={gesture}>
        <Reanimated.View
          className="flex-1"
          style={[styles.card, paneStyle]}
          accessibilityElementsHidden={open}
          importantForAccessibility={open ? "no-hide-descendants" : "auto"}
        >
          <Reanimated.View className="flex-1 bg-screen" style={[styles.clip, clipStyle]}>
            {props.children}
            <Reanimated.View
              pointerEvents={interactive ? "none" : "box-none"}
              style={[StyleSheet.absoluteFill, floatingStyle]}
            >
              {props.floating}
            </Reanimated.View>
            {interactive ? (
              <Pressable
                accessibilityRole="button"
                accessibilityLabel={gt("Close menu")}
                style={StyleSheet.absoluteFill}
                onPress={closeWorkspaceDrawer}
              />
            ) : null}
          </Reanimated.View>
        </Reanimated.View>
      </GestureDetector>
    </View>
  );
}

const styles = StyleSheet.create({
  menu: { position: "absolute", top: 0, bottom: 0, left: 0 },
  clip: {
    overflow: "hidden",
    borderCurve: "continuous",
    borderWidth: StyleSheet.hairlineWidth,
    borderColor: "rgba(128,128,128,0.32)",
  },
  card: {
    shadowColor: "#000",
    shadowOffset: { width: 0, height: DRAWER_CARD_SHADOW.offsetY },
    shadowRadius: DRAWER_CARD_SHADOW.radius,
  },
});
