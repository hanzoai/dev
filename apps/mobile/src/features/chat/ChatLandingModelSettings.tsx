import { cloudChatEffort, cloudChatSelection } from "@hanzodev/client-runtime/cloud-chat";
import { useGT } from "gt-react-native";
import { useNavigation } from "@react-navigation/native";
import type { NativeStackNavigationProp } from "@react-navigation/native-stack";

import { buildModelOptions, groupByProvider } from "../../lib/modelOptions";
import { resolveProviderOptionDescriptors } from "../../lib/providerOptions";
import { useComposerDraft } from "../../state/use-composer-drafts";
import { ThreadSettingsPickerScreen } from "../threads/ThreadSettingsSheet";
import { useCloudChat } from "./cloud-chat";
import { CHAT_LANDING_DRAFT_KEY, markLocalOptions } from "./chat-landing.logic";

/** The thread's model picker over the catalog, with the linked Mac's models in a Local group: with no environment, the catalog is the config. */
export function ChatLandingModelSettingsRouteScreen() {
  const gt = useGT();
  const navigation = useNavigation<NativeStackNavigationProp<Record<string, object | undefined>>>();
  const chat = useCloudChat();
  const prompt = useComposerDraft(CHAT_LANDING_DRAFT_KEY).text;
  const selection = cloudChatSelection(chat.models, chat.state.model, chat.state.effort);
  const options = markLocalOptions(
    buildModelOptions(chat.serverConfig, selection, undefined, null),
    chat.macOnline,
    gt("Mac offline"),
  );
  const current = options.find((option) => option.selection.model === selection.model) ?? null;
  return (
    <ThreadSettingsPickerScreen
      environmentId={null}
      projectId={null}
      cwd={null}
      providerInstanceId={selection.instanceId}
      providerGroups={groupByProvider(options)}
      selectedModel={selection}
      prompt={prompt}
      onPromptChange={() => undefined}
      onSelectModel={(option) =>
        chat.selectModel(option.selection.model, cloudChatEffort(option.selection))
      }
      optionDescriptors={resolveProviderOptionDescriptors({
        capabilities: current?.capabilities,
        selections: selection.options,
      })}
      onUpdateOptionSelections={(next) =>
        chat.selectModel(selection.model, cloudChatEffort({ ...selection, options: next }))
      }
      runtimeMode="full-access"
      offersRuntime={false}
      onUpdateRuntimeMode={() => undefined}
      onClose={() => navigation.goBack()}
    />
  );
}
