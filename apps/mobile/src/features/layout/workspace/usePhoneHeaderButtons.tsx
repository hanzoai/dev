import { useGT } from "gt-react-native";
import { View } from "react-native";

import { Toolbar } from "../../../components/ui/Toolbar";
import { useWorkspaceState } from "../../../state/workspace";
import { startNewCloudChat } from "../../chat/cloud-chat";
import { useAdaptiveWorkspaceLayout } from "../adaptive-workspace-context";
import { openWorkspaceDrawer } from "./workspace-drawer-store";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { WorkspaceComposeButton, WorkspaceMenuButton } from "./WorkspaceCircleButton";

function usePhoneNewChat() {
  const { startNewTask } = useAdaptiveWorkspaceLayout();
  const { state } = useWorkspaceState();
  const selectTab = useSelectWorkspaceTab();
  return () => {
    if (state.hasReadyEnvironment) startNewTask();
    else {
      startNewCloudChat();
      selectTab("threads");
    }
  };
}

/** The same two circles in the native header, for the screens that keep one. */
export function usePhoneHeaderButtons() {
  const gt = useGT();
  const newChat = usePhoneNewChat();
  return {
    menu: (
      <Toolbar.Custom>
        <WorkspaceMenuButton accessibilityLabel={gt("Open menu")} onPress={openWorkspaceDrawer} />
      </Toolbar.Custom>
    ),
    compose: (
      <Toolbar.Custom>
        <WorkspaceComposeButton accessibilityLabel={gt("New chat")} onPress={newChat} />
      </Toolbar.Custom>
    ),
  };
}

/**
 * The phone's two floating circles, over the content with no title or bar. New chat starts a task
 * on a connected environment; with none it opens a fresh cloud chat.
 */
export function PhoneFloatingHeader(props: { readonly top: number }) {
  const gt = useGT();
  const newChat = usePhoneNewChat();
  return (
    <View
      pointerEvents="box-none"
      style={{
        position: "absolute",
        top: props.top,
        left: 16,
        right: 16,
        flexDirection: "row",
        justifyContent: "space-between",
      }}
    >
      <WorkspaceMenuButton accessibilityLabel={gt("Open menu")} onPress={openWorkspaceDrawer} />
      <WorkspaceComposeButton accessibilityLabel={gt("New chat")} onPress={newChat} />
    </View>
  );
}
