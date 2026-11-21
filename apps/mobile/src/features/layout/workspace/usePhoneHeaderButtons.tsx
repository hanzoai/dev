import { useGT } from "gt-react-native";

import { Toolbar } from "../../../components/ui/Toolbar";
import { useWorkspaceState } from "../../../state/workspace";
import { startNewCloudChat } from "../../chat/cloud-chat";
import { useAdaptiveWorkspaceLayout } from "../adaptive-workspace-context";
import { openWorkspaceDrawer } from "./workspace-drawer-store";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { WorkspaceComposeButton, WorkspaceMenuButton } from "./WorkspaceCircleButton";

/**
 * The phone header's two circles. New chat starts a task on a connected environment; with none it
 * returns to the chat landing, which needs no environment.
 */
export function usePhoneHeaderButtons() {
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
