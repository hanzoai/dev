import {
  CloudChatError,
  toWireMessage,
  type LocalChatHost,
} from "@hanzodev/client-runtime/cloud-chat";

import {
  chatOnDevice,
  downloadOnDeviceModel,
  onDeviceEngineAvailable,
  onDeviceModels,
} from "../../native/hanzoEngine";

/** The picker's name for models that run in this app, with nothing leaving the phone. */
export const ON_DEVICE_ENGINE = "On this iPhone · Private";

/**
 * Chat with a Zen model running inside this app on Hanzo Engine (Metal). The weights come from Hanzo's
 * model host once, on first use, and the conversation never leaves the phone.
 */
export const onDeviceChatHost: LocalChatHost | null = onDeviceEngineAvailable
  ? {
      models: async () =>
        onDeviceModels()
          .filter((model) => model.fits)
          .map((model) => ({ id: model.id, engine: ON_DEVICE_ENGINE })),
      stream: async ({ model, messages, onDelta, signal }) => {
        const entry = onDeviceModels().find((m) => m.id === model);
        if (entry === undefined) throw new CloudChatError("offline", "That model is not on this phone");
        if (!entry.installed) {
          try {
            await downloadOnDeviceModel(model);
          } catch {
            throw new CloudChatError("offline", "The model could not be downloaded");
          }
        }
        let reply = "";
        await chatOnDevice({
          model,
          messages: messages.map(toWireMessage).map((m) => ({
            role: m.role,
            content:
              typeof m.content === "string"
                ? m.content
                : m.content.flatMap((p) => (p.type === "text" ? [p.text] : [])).join("\n"),
          })),
          onDelta: (text) => {
            reply += text;
            onDelta(text);
          },
          signal,
        });
        return reply;
      },
    }
  : null;
