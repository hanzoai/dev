import { File, Paths } from "expo-file-system";
import { msg, useGT, useMessages } from "gt-react-native";
import { useRef, useState } from "react";
import { Modal, PanResponder, Pressable, View } from "react-native";
import Svg, { Path } from "react-native-svg";

import { AppText as Text } from "../../components/ui/AppText";
import { GlassSurface } from "../../components/ui/GlassSurface";

const PAD_HEIGHT = 360;
const LABELS = { cancel: msg("Cancel"), clear: msg("Clear"), attach: msg("Attach") };

/** A drawing pad in a sheet. "Attach" hands the drawing over as a PNG file URI, drawn on white. */
export function ComposerSketchPad(props: {
  readonly visible: boolean;
  readonly onClose: () => void;
  readonly onAttach: (uri: string) => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const [strokes, setStrokes] = useState<
    ReadonlyArray<{ readonly id: number; readonly d: string }>
  >([]);
  const nextId = useRef(0);
  const svg = useRef<{ toDataURL: (callback: (base64: string) => void) => void } | null>(null);

  const [pan] = useState(() =>
    PanResponder.create({
      onStartShouldSetPanResponder: () => true,
      onMoveShouldSetPanResponder: () => true,
      onPanResponderGrant: (event) => {
        const { locationX: x, locationY: y } = event.nativeEvent;
        setStrokes((current) => [...current, { id: nextId.current++, d: `M${x} ${y} L${x} ${y}` }]);
      },
      onPanResponderMove: (event) => {
        const { locationX: x, locationY: y } = event.nativeEvent;
        setStrokes((current) => {
          const last = current.at(-1);
          return last
            ? [...current.slice(0, -1), { id: last.id, d: `${last.d} L${x} ${y}` }]
            : current;
        });
      },
    }),
  );

  const attach = () => {
    svg.current?.toDataURL((base64) => {
      const file = new File(Paths.cache, `sketch-${Date.now()}.png`);
      file.create();
      file.write(Uint8Array.from(atob(base64), (char) => char.charCodeAt(0)));
      setStrokes([]);
      props.onAttach(file.uri);
      props.onClose();
    });
  };

  return (
    <Modal
      visible={props.visible}
      animationType="slide"
      presentationStyle="pageSheet"
      onRequestClose={props.onClose}
    >
      <GlassSurface className="flex-1 gap-3 p-4" chrome="none">
        <View className="flex-row items-center justify-between">
          <Pressable accessibilityRole="button" onPress={props.onClose}>
            <Text textRole="body">{m(LABELS.cancel)}</Text>
          </Pressable>
          <Text textRole="headline">{gt("Sketch")}</Text>
          <Pressable
            accessibilityRole="button"
            disabled={strokes.length === 0}
            onPress={attach}
            style={{ opacity: strokes.length === 0 ? 0.4 : 1 }}
          >
            <Text textRole="body">{m(LABELS.attach)}</Text>
          </Pressable>
        </View>
        <View
          accessibilityLabel={gt("Drawing pad")}
          style={{
            height: PAD_HEIGHT,
            backgroundColor: "#ffffff",
            borderRadius: 16,
            overflow: "hidden",
          }}
          {...pan.panHandlers}
        >
          <Svg ref={svg as never} width="100%" height={PAD_HEIGHT} pointerEvents="none">
            {strokes.map((stroke) => (
              <Path
                key={stroke.id}
                d={stroke.d}
                stroke="#111111"
                strokeWidth={3}
                strokeLinecap="round"
                strokeLinejoin="round"
                fill="none"
              />
            ))}
          </Svg>
        </View>
        <Pressable
          accessibilityRole="button"
          disabled={strokes.length === 0}
          onPress={() => setStrokes([])}
        >
          <Text textRole="body" className="text-center">
            {m(LABELS.clear)}
          </Text>
        </Pressable>
      </GlassSurface>
    </Modal>
  );
}
