/**
 * The `/v1/models` catalog as the provider list the model picker already renders. Without an
 * environment there is no server config, so the catalog stands in for one: the same providers,
 * groups (Enso, Hanzo, Claude, Codex, ...) and effort panel as a thread's picker.
 */
import {
  DEFAULT_SERVER_SETTINGS,
  ProviderDriverKind,
  ProviderInstanceId,
  type ModelSelection,
  type ServerConfig,
  type ServerProvider,
  type ServerProviderModel,
} from "@hanzodev/contracts";
import { HANZO_REASONING_CAPABILITIES } from "@hanzodev/shared/model";

import type { CloudChatModel } from "./api.ts";
import { isLocalChatSlug, LOCAL_CHAT_DRIVER, localChatSlug, type LocalChatModel } from "./local.ts";

const REASONING_EFFORT_ID = "reasoningEffort";

/** The catalog's owner names that stand for a provider the picker already knows. */
const OWNERS: Readonly<Record<string, string>> = {
  anthropic: "claudeAgent",
  openai: "codex",
  xai: "grok",
  "x-ai": "grok",
  hanzo: "hanzo",
  hanzoai: "hanzo",
  zen: "hanzo",
  zenlm: "hanzo",
};

const FAMILIES: ReadonlyArray<{ readonly driver: string; readonly pattern: RegExp }> = [
  { driver: "claudeAgent", pattern: /^(anthropic\/)?claude/i },
  { driver: "codex", pattern: /^(openai\/)?(gpt|o\d|codex)/i },
  { driver: "grok", pattern: /^(xai\/)?grok/i },
];

const slug = (value: string) =>
  value
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");

/**
 * The provider a catalog model belongs to: the catalog's own owner when it names one, else a guess
 * from the model id. Hanzo's families and unknown models are Hanzo's.
 */
export function cloudChatDriver(model: CloudChatModel): string {
  const owner = model.provider === undefined ? "" : slug(model.provider);
  if (owner !== "") return OWNERS[owner] ?? owner;
  return FAMILIES.find((family) => family.pattern.test(model.id))?.driver ?? "hanzo";
}

const KNOWN_DRIVERS: Readonly<Record<string, true>> = {
  hanzo: true,
  claudeAgent: true,
  codex: true,
  grok: true,
};

const CHECKED_AT = "1970-01-01T00:00:00.000Z";

/** The "Local" group: the Mac's models, or one placeholder row while none are known. */
function localProvider(models: ReadonlyArray<LocalChatModel>): ServerProvider {
  return {
    instanceId: ProviderInstanceId.make(LOCAL_CHAT_DRIVER),
    driver: ProviderDriverKind.make(LOCAL_CHAT_DRIVER),
    displayName: "Local",
    enabled: true,
    installed: true,
    version: null,
    status: "ready",
    auth: { status: "authenticated" },
    checkedAt: CHECKED_AT,
    models: (models.length > 0 ? models : [{ id: "", engine: undefined }]).map((model) => ({
      slug: localChatSlug(model.id),
      name: model.id === "" ? "Local models" : model.id,
      ...(model.engine === undefined ? {} : { subProvider: model.engine }),
      isCustom: false,
      isDefault: false,
      capabilities: null,
    })),
    slashCommands: [],
    skills: [],
  };
}

/** Cloud providers from the catalog, then the "Local" group last when the Mac's models are given. */
export function cloudChatProviders(
  models: ReadonlyArray<CloudChatModel>,
  local?: ReadonlyArray<LocalChatModel>,
): ReadonlyArray<ServerProvider> {
  const byDriver = new Map<string, ServerProviderModel[]>();
  const owners = new Map<string, string>();
  for (const [index, model] of models.entries()) {
    const driver = cloudChatDriver(model);
    const list = byDriver.get(driver) ?? [];
    if (!owners.has(driver) && model.provider !== undefined) owners.set(driver, model.provider);
    list.push({
      slug: model.id,
      name: model.label,
      isCustom: false,
      isDefault: index === 0,
      // Only Hanzo's own models take an effort here; the chat route forwards it as-is.
      capabilities: driver === "hanzo" ? HANZO_REASONING_CAPABILITIES : null,
    });
    byDriver.set(driver, list);
  }
  const cloud: ServerProvider[] = [...byDriver.entries()].map(([driver, providerModels]) => ({
    instanceId: ProviderInstanceId.make(driver),
    driver: ProviderDriverKind.make(driver),
    // A provider the picker does not know is named as the catalog names it.
    ...(driver in KNOWN_DRIVERS || owners.get(driver) === undefined
      ? {}
      : { displayName: owners.get(driver) }),
    enabled: true,
    installed: true,
    version: null,
    status: "ready",
    auth: { status: "authenticated" },
    checkedAt: CHECKED_AT,
    models: providerModels,
    slashCommands: [],
    skills: [],
  }));
  return local === undefined ? cloud : [...cloud, localProvider(local)];
}

/**
 * The catalog as the server config the thread composer reads. Only providers and the defaults
 * the composer consults exist; nothing here describes a machine, so it advertises no capabilities.
 */
export function cloudChatServerConfig(
  models: ReadonlyArray<CloudChatModel>,
  local?: ReadonlyArray<LocalChatModel>,
): ServerConfig {
  return {
    environment: { capabilities: {} },
    settings: DEFAULT_SERVER_SETTINGS,
    providers: cloudChatProviders(models, local),
  } as unknown as ServerConfig;
}

/** The picker's selection for a chat model, carrying its effort when one is set. */
export function cloudChatSelection(
  models: ReadonlyArray<CloudChatModel>,
  modelId: string,
  effort: string | null,
): ModelSelection {
  const driver = isLocalChatSlug(modelId)
    ? LOCAL_CHAT_DRIVER
    : cloudChatDriver(
        models.find((model) => model.id === modelId) ?? { id: modelId, label: modelId },
      );
  const instanceId = ProviderInstanceId.make(driver);
  return effort === null || driver !== "hanzo"
    ? { instanceId, model: modelId }
    : { instanceId, model: modelId, options: [{ id: REASONING_EFFORT_ID, value: effort }] };
}

/** The effort a selection carries, if any. */
export function cloudChatEffort(selection: ModelSelection): string | null {
  const value = selection.options?.find((option) => option.id === REASONING_EFFORT_ID)?.value;
  return typeof value === "string" ? value : null;
}

const INPUT_KINDS = {
  image: ["image", "images", "vision"],
  file: ["file", "files", "pdf", "document", "documents"],
} as const;

/**
 * Whether the model reads this kind of input. A catalog that does not list inputs for a model
 * says nothing against it, so the model is tried.
 */
export function cloudChatAcceptsInput(model: CloudChatModel | undefined, kind: "image" | "file") {
  if (model?.inputs === undefined) return true;
  const accepted: ReadonlyArray<string> = INPUT_KINDS[kind];
  return model.inputs.some((input) => accepted.includes(input.toLowerCase()));
}
