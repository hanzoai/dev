import { describe, expect, it } from "vite-plus/test";

import { cloudChatServerConfig } from "@hanzodev/client-runtime/cloud-chat";

import { buildModelOptions, groupByProvider } from "../../lib/modelOptions";
import { landsInChat, markLocalOptions } from "./chat-landing.logic";

describe("chat landing", () => {
  it("lands a signed-in user in chat, never on an environment gate", () => {
    expect(landsInChat({ signedIn: true })).toBe(true);
  });
});

describe("local model rows", () => {
  const config = cloudChatServerConfig(
    [{ id: "enso-flash", label: "Enso Flash" }],
    [{ id: "zen4", engine: "Hanzo Engine" }],
  );
  const options = () => buildModelOptions(config, null, undefined, null);

  it("lists the Mac's models in a Local group after the cloud ones", () => {
    const groups = groupByProvider(markLocalOptions(options(), true, "Mac offline"));
    expect(groups.map((group) => group.providerLabel)).toEqual(["Enso", "Local"]);
    expect(groups[1]?.models.map((model) => model.selection.model)).toEqual(["local:zen4"]);
    expect(groups[1]?.models[0]?.isUnavailable).toBeUndefined();
  });

  it("disables the Local rows and says Mac offline while the Mac is away, leaving cloud rows alone", () => {
    const marked = markLocalOptions(options(), false, "Mac offline");
    const local = marked.filter((option) => option.providerDriver === "local");
    expect(local.length).toBe(1);
    expect(local.every((option) => option.isUnavailable === true)).toBe(true);
    expect(local[0]?.subtitle).toBe("Mac offline");
    expect(
      marked.find((option) => option.selection.model === "enso-flash")?.isUnavailable,
    ).toBeUndefined();
  });

  it("never offers a disabled local row as the default pick", () => {
    const marked = markLocalOptions(options(), false, "Mac offline");
    expect(marked.find((option) => !option.isUnavailable)?.selection.model).toBe("enso-flash");
  });
});
