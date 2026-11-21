import { useGT } from "gt-react-native";
import { View } from "react-native";

import { useWorkspaceState } from "../../../state/workspace";
import { startNewCloudChat } from "../../chat/cloud-chat";
import { useAdaptiveWorkspaceLayout } from "../adaptive-workspace-context";
import { openWorkspaceDrawer } from "./workspace-drawer-store";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { WorkspaceComposeButton, WorkspaceMenuButton } from "./WorkspaceCircleButton";

/**
 * The phone's two floating circles, over the content with no title or bar. New chat starts a task
 * on a connected environment; with none it opens a fresh cloud chat.
 */
export function PhoneFloatingHeader(props: { readonly top: number }) {
  const gt = useGT();
  const { startNewTask } = useAdaptiveWorkspaceLayout();
  const { state } = useWorkspaceState();
  const selectTab = useSelectWorkspaceTab();
  const newChat = () => {
    if (state.hasReadyEnvironment) startNewTask();
    else {
      startNewCloudChat();
      selectTab("threads");
    }
  };
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
