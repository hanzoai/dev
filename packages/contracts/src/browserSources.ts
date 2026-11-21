import * as Schema from "effect/Schema";

import { TrimmedNonEmptyString } from "./baseSchemas.ts";

/** HIP-1350 Browser sources: the Hanzo extension, joined to this computer's ZAP router. */
export const BrowserSourceBrowser = Schema.Literals(["firefox", "chrome", "safari", "other"]);
export type BrowserSourceBrowser = typeof BrowserSourceBrowser.Type;

/** What the extension detects for one AI product: state only, never a cookie or page content. */
export const BrowserSourceProvider = Schema.Struct({
  provider: TrimmedNonEmptyString,
  plan: Schema.optionalKey(TrimmedNonEmptyString),
  state: Schema.Literals(["ready", "needs_login"]),
});
export type BrowserSourceProvider = typeof BrowserSourceProvider.Type;

export const BrowserSource = Schema.Struct({
  /** The router's id for this browser node, `browser/<host>/<engine>-<id>`. */
  id: TrimmedNonEmptyString,
  browser: BrowserSourceBrowser,
  name: TrimmedNonEmptyString,
  /** What the extension says it serves, as the router lists it. */
  capabilities: Schema.Array(Schema.String),
  /** A browser login is never routable (HIP-1350): the rows are for the user to see. */
  providers: Schema.Array(BrowserSourceProvider),
});
export type BrowserSource = typeof BrowserSource.Type;

export const BrowserSourceListResult = Schema.Struct({ sources: Schema.Array(BrowserSource) });
export type BrowserSourceListResult = typeof BrowserSourceListResult.Type;

export class BrowserSourceError extends Schema.TaggedError<BrowserSourceError>()(
  "BrowserSourceError",
  {
    reason: Schema.Literals(["no_router", "rejected", "refused_origin", "not_found", "timeout"]),
    message: Schema.String,
  },
) {}
