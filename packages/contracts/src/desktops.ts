import * as Schema from "effect/Schema";

import { IsoDateTime, TrimmedNonEmptyString } from "./baseSchemas.ts";

/** HIP-1350. A desktop is one installed Dev app linked to one Hanzo IAM user. */
export const DesktopId = TrimmedNonEmptyString.check(Schema.isMaxLength(128));
export type DesktopId = typeof DesktopId.Type;

export const DesktopName = TrimmedNonEmptyString.check(Schema.isMaxLength(120));

export const DesktopProviderKind = Schema.Literals(["login", "key"]);
export const DesktopProviderStateName = Schema.Literals(["ready", "needs_login", "limited"]);

/** What a heartbeat says about one provider instance: state only, never a credential. */
export const DesktopProviderState = Schema.Struct({
  provider: TrimmedNonEmptyString,
  instance: TrimmedNonEmptyString,
  kind: DesktopProviderKind,
  state: DesktopProviderStateName,
  plan: Schema.optionalKey(TrimmedNonEmptyString),
  /** Whether this instance is shared with the user's other surfaces. Off until the user turns it on. */
  shared: Schema.Boolean,
});
export type DesktopProviderState = typeof DesktopProviderState.Type;

/** Provider catalog names whose subscription login includes class `desktop` (HIP-1350). */
export const DESKTOP_ROUTABLE_PROVIDERS = ["claude", "codex", "copilot"] as const;

/** A provider row on this desktop: the heartbeat state plus whether the user can share it. */
export const DesktopProviderRow = Schema.Struct({
  ...DesktopProviderState.fields,
  shareable: Schema.Boolean,
});
export type DesktopProviderRow = typeof DesktopProviderRow.Type;

export const DesktopSetSharedInput = Schema.Struct({
  instance: TrimmedNonEmptyString,
  shared: Schema.Boolean,
});

export const DesktopPresence = Schema.Literals(["online", "offline", "revoked"]);

export const DesktopSourceKind = Schema.Literals(["desktop", "browser"]);

/** A linked source: a desktop (this app) or a browser (the extension), one registry. */
export const Desktop = Schema.Struct({
  id: DesktopId,
  kind: DesktopSourceKind,
  name: DesktopName,
  os: Schema.String,
  arch: Schema.String,
  version: Schema.String,
  state: DesktopPresence,
  lastSeen: Schema.NullOr(IsoDateTime),
  linkedAt: IsoDateTime,
  providers: Schema.Array(DesktopProviderState),
});
export type Desktop = typeof Desktop.Type;

export const DesktopReportLevel = Schema.Literals(["metadata", "metadata+content"]);

/** A code the user types at the verification page while this desktop waits (RFC 8628). */
export const DesktopDeviceCode = Schema.Struct({
  verificationUri: Schema.String,
  userCode: Schema.String,
});

export const DesktopLinkStatus = Schema.Struct({
  signedIn: Schema.Boolean,
  /** Set while a device-code sign-in waits for the user. */
  deviceCode: Schema.NullOr(DesktopDeviceCode),
  /** The IAM identity and organization the link belongs to. */
  identity: Schema.NullOr(Schema.String),
  organization: Schema.NullOr(Schema.String),
  /** This install's own registry row, or null when it is not linked. */
  desktop: Schema.NullOr(Desktop),
  lastHeartbeatAt: Schema.NullOr(IsoDateTime),
  level: DesktopReportLevel,
  /** Provider state this install reports, built the way each heartbeat builds it. */
  providers: Schema.Array(DesktopProviderRow),
  /** The exact JSON of the last heartbeat sent, or what the next one will send. */
  report: Schema.Struct({ body: Schema.String, sent: Schema.Boolean }),
});
export type DesktopLinkStatus = typeof DesktopLinkStatus.Type;

export const DesktopLinkInput = Schema.Struct({
  /**
   * With no credential yet, how this install signs in: the browser (loopback PKCE) or a device code.
   */
  signIn: Schema.optionalKey(Schema.Literals(["browser", "device"])),
  /**
   * Hanzo IAM tokens from a web sign-in, stored as `hanzo connect login` stores them.
   * Omitted when the server already holds a credential.
   */
  token: Schema.optionalKey(
    Schema.Struct({
      accessToken: TrimmedNonEmptyString,
      refreshToken: Schema.String,
      expiresAtEpochMs: Schema.Number,
    }),
  ),
});
export type DesktopLinkInput = typeof DesktopLinkInput.Type;

export const DesktopRenameInput = Schema.Struct({ name: DesktopName });
export type DesktopRenameInput = typeof DesktopRenameInput.Type;

export const DesktopRevokeInput = Schema.Struct({ id: DesktopId });
export type DesktopRevokeInput = typeof DesktopRevokeInput.Type;

export const DesktopListResult = Schema.Struct({ desktops: Schema.Array(Desktop) });
export type DesktopListResult = typeof DesktopListResult.Type;

export const DesktopOk = Schema.Struct({ ok: Schema.Boolean });

/** Why a desktop operation failed, in words a user can act on. */
export class DesktopLinkError extends Schema.TaggedError<DesktopLinkError>()("DesktopLinkError", {
  reason: Schema.Literals([
    "sign_in_required",
    "unreachable",
    "rejected",
    "limit",
    "not_linked",
    "not_shared",
    "storage",
  ]),
  message: Schema.String,
  cause: Schema.optionalKey(Schema.Defect()),
}) {}

export const DesktopOutboxState = Schema.Literals(["pending", "sent", "superseded", "rejected"]);

/** One usage report, as written to the local outbox before it is sent. `body` is the sent JSON. */
export const DesktopOutboxBatch = Schema.Struct({
  seq: Schema.Number,
  at: IsoDateTime,
  state: DesktopOutboxState,
  body: Schema.String,
});

export const DesktopOutbox = Schema.Struct({
  sendNothing: Schema.Boolean,
  /** The newest batches first, at most 100. */
  batches: Schema.Array(DesktopOutboxBatch),
});
export type DesktopOutbox = typeof DesktopOutbox.Type;

export const DesktopSendNothingInput = Schema.Struct({ sendNothing: Schema.Boolean });

/** Providers a user can hold an API key for (HIP-1350 Provider policy). */
export const API_KEY_PROVIDERS = [
  "anthropic",
  "openai",
  "gemini",
  "xai",
  "openrouter",
  "opencode",
  "kiro",
] as const;

export const ProviderRouteClass = Schema.Literals(["local", "desktop", "key"]);
export type ProviderRouteClass = typeof ProviderRouteClass.Type;

export const ProviderAccount = Schema.Struct({
  id: TrimmedNonEmptyString,
  provider: TrimmedNonEmptyString,
  kind: Schema.Literal("key"),
  label: Schema.String,
  /** First 8 hex of the sha256 of the key. The value itself is never returned. */
  fingerprint: Schema.String,
  created: IsoDateTime,
  route: ProviderRouteClass,
});
export type ProviderAccount = typeof ProviderAccount.Type;

export const ProviderAccountListResult = Schema.Struct({ accounts: Schema.Array(ProviderAccount) });

export const ProviderAccountAddInput = Schema.Struct({
  provider: Schema.Literals(API_KEY_PROVIDERS),
  label: TrimmedNonEmptyString.check(Schema.isMaxLength(120)),
  /** Sent to Hanzo KMS once; this desktop never stores it. */
  value: TrimmedNonEmptyString.check(Schema.isMaxLength(4096)),
});

export const ProviderAccountRemoveInput = Schema.Struct({ id: TrimmedNonEmptyString });
