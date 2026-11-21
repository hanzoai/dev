import type { MenuAction } from "@react-native-menu/menu";
import { msg, useGT, useMessages } from "gt-react-native";
import { Pressable } from "react-native";

import { useAndroidControlSizing } from "../../components/ui/useAndroidControlSizing";
import { SymbolView } from "../../components/ui/AppSymbol";
import { Menu } from "../../components/ui/Menu";

const ATTACHMENT_MENU_ACTIONS: MenuAction[] = [
  { id: "photos", title: msg("Photo Library"), image: "photo" },
  { id: "files", title: msg("Choose Files"), image: "folder" },
];

/**
 * The `+`. With `onOpenMenu` it opens the full menu (`ComposerPlusSheet`); without, it only
 * attaches (a question's answer carries files, nothing else).
 */
export function ComposerAttachmentButton(props: {
  readonly disabled?: boolean;
  readonly size?: "regular" | "large";
  readonly supportsFiles: boolean;
  readonly onPickMedia: () => Promise<void>;
  readonly onPickFiles: () => Promise<void>;
  readonly onOpenMenu?: () => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const { scale } = useAndroidControlSizing();
  const label = props.onOpenMenu ? gt("Add files, agents, skills and more") : gt("Add attachment");
  const button = (
    <Pressable
      accessibilityLabel={label}
      accessibilityRole="button"
      accessibilityState={{ disabled: props.disabled }}
      className="size-[44px] shrink-0 items-center justify-center rounded-full active:opacity-70 disabled:opacity-50"
      disabled={props.disabled}
      onPress={
        props.onOpenMenu ?? (props.supportsFiles ? undefined : () => void props.onPickMedia())
      }
    >
      <SymbolView
        name="plus"
        size={Math.round((props.size === "large" ? 26 : 20) * scale)}
        weight="regular"
        tintColorClassName="accent-icon"
        type="monochrome"
      />
    </Pressable>
  );

  if (props.disabled || props.onOpenMenu || !props.supportsFiles) {
    return button;
  }

  return (
    <Menu
      accessible
      accessibilityLabel={label}
      accessibilityRole="button"
      actions={ATTACHMENT_MENU_ACTIONS.map((action) => ({ ...action, title: m(action.title) }))}
      onPressAction={({ nativeEvent }) => {
        if (nativeEvent.event === "photos") {
          void props.onPickMedia();
        } else if (nativeEvent.event === "files") {
          void props.onPickFiles();
        }
      }}
    >
      {button}
    </Menu>
  );
}
