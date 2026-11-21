/**
 * The usage data model. One definition, shared by the local usage panel, the tray,
 * and the linked-desktop report that registers a desktop with the cloud.
 *
 * Reports carry metadata only (tokens, model, cost). `content` exists for the user's
 * explicit per-device opt-in and is never populated by the scanners in this package.
 */
import * as Schema from "effect/Schema";

export const ProviderId = Schema.String.check(Schema.isPattern(/^[a-z0-9][a-z0-9-]*$/));
export type ProviderId = typeof ProviderId.Type;

/** ISO-8601 instant. */
const Instant = Schema.String;
const Percent = Schema.Number.check(Schema.isBetween({ minimum: 0, maximum: 100 }));

export const WindowKind = Schema.Literals([
  "session",
  "daily",
  "weekly",
  "monthly",
  "credits",
  "other",
]);
export type WindowKind = typeof WindowKind.Type;

export const UsageWindow = Schema.Struct({
  kind: WindowKind,
  label: Schema.String,
  usedPercent: Percent,
  windowMinutes: Schema.optional(Schema.Number),
  resetsAt: Schema.optional(Instant),
});
export type UsageWindow = typeof UsageWindow.Type;

export const Credits = Schema.Struct({
  currency: Schema.String,
  balance: Schema.optional(Schema.Number),
  spent: Schema.optional(Schema.Number),
  limit: Schema.optional(Schema.Number),
});
export type Credits = typeof Credits.Type;

export const StatusIndicator = Schema.Literals([
  "none",
  "minor",
  "major",
  "critical",
  "maintenance",
  "unknown",
]);
export type StatusIndicator = typeof StatusIndicator.Type;

export const Incident = Schema.Struct({
  name: Schema.String,
  impact: StatusIndicator,
  url: Schema.optional(Schema.String),
});
export type Incident = typeof Incident.Type;

export const ProviderStatus = Schema.Struct({
  indicator: StatusIndicator,
  description: Schema.optional(Schema.String),
  incidents: Schema.Array(Incident),
  checkedAt: Instant,
});
export type ProviderStatus = typeof ProviderStatus.Type;

export const SnapshotSource = Schema.Literals(["oauth", "api", "local", "cookies", "none"]);
export type SnapshotSource = typeof SnapshotSource.Type;

export const ProviderSnapshot = Schema.Struct({
  provider: ProviderId,
  fetchedAt: Instant,
  source: SnapshotSource,
  plan: Schema.optional(Schema.String),
  account: Schema.optional(Schema.String),
  windows: Schema.Array(UsageWindow),
  credits: Schema.optional(Credits),
  status: Schema.optional(ProviderStatus),
  /** Set when the last refresh failed; windows/credits then hold the last good values or none. */
  error: Schema.optional(Schema.String),
});
export type ProviderSnapshot = typeof ProviderSnapshot.Type;

/** One model call, as seen in a local session log. Never carries prompt or completion text unless opted in. */
export const UsageEvent = Schema.Struct({
  id: Schema.String,
  provider: ProviderId,
  model: Schema.String,
  at: Instant,
  inputTokens: Schema.Number,
  outputTokens: Schema.Number,
  cacheReadTokens: Schema.Number,
  cacheWriteTokens: Schema.Number,
  /** Estimated from the pricing table; absent when the model has no known price. */
  costUsd: Schema.optional(Schema.Number),
  sessionId: Schema.optional(Schema.String),
  /** Opt-in only. */
  content: Schema.optional(Schema.String),
});
export type UsageEvent = typeof UsageEvent.Type;

export const CostDay = Schema.Struct({
  day: Schema.String,
  provider: ProviderId,
  model: Schema.String,
  inputTokens: Schema.Number,
  outputTokens: Schema.Number,
  cacheTokens: Schema.Number,
  costUsd: Schema.optional(Schema.Number),
});
export type CostDay = typeof CostDay.Type;

/** What a linked desktop sends the cloud registry. */
export const UsageReport = Schema.Struct({
  version: Schema.Literal(1),
  deviceId: Schema.String,
  from: Instant,
  to: Instant,
  events: Schema.Array(UsageEvent),
  snapshots: Schema.Array(ProviderSnapshot),
});
export type UsageReport = typeof UsageReport.Type;

/** What the tray and the panel read. */
export const UsageSummary = Schema.Struct({
  generatedAt: Instant,
  snapshots: Schema.Array(ProviderSnapshot),
  cost: Schema.Array(CostDay),
});
export type UsageSummary = typeof UsageSummary.Type;

export const TrayMode = Schema.Literals(["merged", "perProvider"]);
export type TrayMode = typeof TrayMode.Type;

/** Per-provider user choice. Opt-in for anything that reads browser cookies is explicit. */
export const ProviderSetting = Schema.Struct({
  enabled: Schema.Boolean,
  cookiesOptIn: Schema.optional(Schema.Boolean),
});
export type ProviderSetting = typeof ProviderSetting.Type;

export const UsageSettings = Schema.Struct({
  providers: Schema.Record(Schema.String, ProviderSetting),
  trayMode: TrayMode,
  notifyAtPercent: Schema.Array(Percent),
  refreshSeconds: Schema.Number,
});
export type UsageSettings = typeof UsageSettings.Type;

export const defaultUsageSettings: UsageSettings = {
  providers: {},
  trayMode: "merged",
  notifyAtPercent: [80, 95],
  refreshSeconds: 300,
};
