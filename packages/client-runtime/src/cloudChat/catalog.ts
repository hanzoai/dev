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

export const REASONING_EFFORT_ID = "reasoningEffort";

const FAMILIES: ReadonlyArray<{ readonly driver: string; readonly pattern: RegExp }> = [
  { driver: "claudeAgent", pattern: /^(anthropic\/)?claude/i },
  { driver: "codex", pattern: /^(openai\/)?(gpt|o\d|codex)/i },
  { driver: "grok", pattern: /^(xai\/)?grok/i },
];

/** The provider a catalog model belongs to; Hanzo's own families and unknown models are Hanzo's. */
export function cloudChatDriver(modelId: string): string {
  return FAMILIES.find((family) => family.pattern.test(modelId))?.driver ?? "hanzo";
}

const CHECKED_AT = "1970-01-01T00:00:00.000Z";

export function cloudChatProviders(
  models: ReadonlyArray<CloudChatModel>,
): ReadonlyArray<ServerProvider> {
  const byDriver = new Map<string, ServerProviderModel[]>();
  for (const [index, model] of models.entries()) {
    const driver = cloudChatDriver(model.id);
    const list = byDriver.get(driver) ?? [];
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
  return [...byDriver.entries()].map(([driver, providerModels]) => ({
    instanceId: ProviderInstanceId.make(driver),
    driver: ProviderDriverKind.make(driver),
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
}

/**
 * The catalog as the server config the thread composer reads. Only providers and the defaults
 * the composer consults exist; nothing here describes a machine, so it advertises no capabilities.
 */
export function cloudChatServerConfig(models: ReadonlyArray<CloudChatModel>): ServerConfig {
  return {
    environment: { capabilities: {} },
    settings: DEFAULT_SERVER_SETTINGS,
    providers: cloudChatProviders(models),
  } as unknown as ServerConfig;
}

/** The picker's selection for a chat model, carrying its effort when one is set. */
export function cloudChatSelection(modelId: string, effort: string | null): ModelSelection {
  const instanceId = ProviderInstanceId.make(cloudChatDriver(modelId));
  return effort === null || cloudChatDriver(modelId) !== "hanzo"
    ? { instanceId, model: modelId }
    : { instanceId, model: modelId, options: [{ id: REASONING_EFFORT_ID, value: effort }] };
}

/** The effort a selection carries, if any. */
export function cloudChatEffort(selection: ModelSelection): string | null {
  const value = selection.options?.find((option) => option.id === REASONING_EFFORT_ID)?.value;
  return typeof value === "string" ? value : null;
}
