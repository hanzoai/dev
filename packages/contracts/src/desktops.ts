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
});
export type DesktopProviderState = typeof DesktopProviderState.Type;

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

export const DesktopLinkStatus = Schema.Struct({
  signedIn: Schema.Boolean,
  /** The IAM identity and organization the link belongs to. */
  identity: Schema.NullOr(Schema.String),
  organization: Schema.NullOr(Schema.String),
  /** This install's own registry row, or null when it is not linked. */
  desktop: Schema.NullOr(Desktop),
  lastHeartbeatAt: Schema.NullOr(IsoDateTime),
  level: DesktopReportLevel,
  /** Provider state this install reports, built the way each heartbeat builds it. */
  providers: Schema.Array(DesktopProviderState),
  /** The exact JSON of the last heartbeat sent, or what the next one will send. */
  report: Schema.Struct({ body: Schema.String, sent: Schema.Boolean }),
});
export type DesktopLinkStatus = typeof DesktopLinkStatus.Type;

export const DesktopLinkInput = Schema.Struct({
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
    "storage",
  ]),
  message: Schema.String,
  cause: Schema.optionalKey(Schema.Defect()),
}) {}
