import * as Schema from "effect/Schema";

import { TrimmedNonEmptyString, TrimmedString } from "./baseSchemas.ts";

/** `name@marketplace`, the id Claude Code's plugin commands take. */
export const PluginId = TrimmedNonEmptyString.check(
  Schema.isMaxLength(200),
  Schema.isPattern(/^[A-Za-z0-9][A-Za-z0-9._-]*(@[A-Za-z0-9][A-Za-z0-9._-]*)?$/),
);
export type PluginId = typeof PluginId.Type;

/** A plugin the agent's CLI has installed (`installed`) or a marketplace offers. */
export const Plugin = Schema.Struct({
  id: PluginId,
  name: TrimmedNonEmptyString,
  /** Provider that owns this plugin (e.g. "claude", "codex", "antigravity"). */
  provider: Schema.optionalKey(TrimmedNonEmptyString),
  marketplace: Schema.NullOr(TrimmedNonEmptyString),
  version: Schema.NullOr(TrimmedString),
  description: Schema.NullOr(TrimmedString),
  installed: Schema.Boolean,
  enabled: Schema.Boolean,
});
export type Plugin = typeof Plugin.Type;

export const PluginListInput = Schema.Struct({
  /** Narrows what marketplaces offer; installed plugins always list. */
  query: Schema.optionalKey(TrimmedString.check(Schema.isMaxLength(120))),
});
export type PluginListInput = typeof PluginListInput.Type;

export const PluginListResult = Schema.Struct({
  plugins: Schema.Array(Plugin),
  /** Offers that matched beyond the ones listed. */
  more: Schema.Number,
});
export type PluginListResult = typeof PluginListResult.Type;

export const PluginSetEnabledInput = Schema.Struct({ id: PluginId, enabled: Schema.Boolean });
export type PluginSetEnabledInput = typeof PluginSetEnabledInput.Type;

export const PluginInstallInput = Schema.Struct({ id: PluginId });
export type PluginInstallInput = typeof PluginInstallInput.Type;

export const PluginOperationResult = Schema.Struct({ plugin: Plugin });
export type PluginOperationResult = typeof PluginOperationResult.Type;

export class PluginOperationError extends Schema.TaggedError<PluginOperationError>()(
  "PluginOperationError",
  {
    reason: Schema.Literals(["cli_missing", "failed"]),
    message: Schema.String,
  },
) {}
