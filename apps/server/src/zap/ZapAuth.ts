import { AuthSessionId, AuthStandardClientScopes } from "@hanzodev/contracts";
import * as Context from "effect/Context";
import * as DateTime from "effect/DateTime";
import * as Effect from "effect/Effect";
import * as Jwt from "effect/jose/Jwt";
import * as Layer from "effect/Layer";
import * as Schema from "effect/Schema";
import type * as Signature from "effect/crypto/Signature";
import type * as Jwks from "effect/jose/Jwks";

import type { AuthenticatedSession } from "../auth/EnvironmentAuth.ts";

/** The claims Hanzo IAM puts on an access token that name its person. */
const IamClaims = Schema.Struct({
  owner: Schema.optional(Schema.String),
  name: Schema.optional(Schema.String),
});

export class ZapAuthError extends Schema.TaggedError<ZapAuthError>()("ZapAuthError", {
  reason: Schema.Literals(["invalid_token", "wrong_owner"]),
}) {}

/**
 * Who may open a ZAP session on this server: a bearer that Hanzo IAM issued (signature checked
 * against the issuer's JWKS, issuer and audience pinned), whose person is the one this
 * environment belongs to. Anyone else is refused.
 */
export class ZapAuth extends Context.Service<
  ZapAuth,
  {
    readonly authenticate: (bearer: string) => Effect.Effect<AuthenticatedSession, ZapAuthError>;
  }
>()("hanzo/zap/ZapAuth") {}

export const layerIam = (options: {
  readonly issuer: string;
  readonly audience: string;
  /** The IAM person this environment belongs to, as `<owner>/<name>`. */
  readonly principal: string;
}): Layer.Layer<ZapAuth, never, Jwks.Jwks | Signature.Signature> =>
  Layer.effect(ZapAuth)(
    Effect.gen(function* () {
      const context = yield* Effect.context<Jwks.Jwks | Signature.Signature>();
      return {
        authenticate: Effect.fn("ZapAuth.authenticate")((bearer: string) =>
          Jwt.verifyWithKeySet(IamClaims, bearer, {
            algorithms: ["RS256", "ES256", "EdDSA"],
            issuer: options.issuer,
            audience: options.audience,
            clockTolerance: "30 seconds",
          }).pipe(
            Effect.provideContext(context),
            Effect.mapError(() => new ZapAuthError({ reason: "invalid_token" })),
            Effect.flatMap(({ claims, registeredClaims }) => {
              const principal = `${claims.owner ?? ""}/${claims.name ?? ""}`;
              // An access token with no expiry would hold a session open forever.
              if (registeredClaims.exp === undefined) {
                return Effect.fail(new ZapAuthError({ reason: "invalid_token" }));
              }
              if (principal !== options.principal) {
                return Effect.fail(new ZapAuthError({ reason: "wrong_owner" }));
              }
              return Effect.succeed<AuthenticatedSession>({
                sessionId: AuthSessionId.make(`iam:${principal}`),
                subject: principal,
                method: "bearer-access-token",
                scopes: AuthStandardClientScopes,
                expiresAt: DateTime.makeUnsafe(registeredClaims.exp * 1000),
              });
            }),
          ),
        ),
      };
    }),
  );
