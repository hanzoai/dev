import { AuthSessionId, AuthStandardClientScopes } from "@hanzodev/contracts";
import * as Context from "effect/Context";
import * as DateTime from "effect/DateTime";
import * as Effect from "effect/Effect";
import * as Jwt from "effect/jose/Jwt";
import * as Layer from "effect/Layer";
import * as Schema from "effect/Schema";
import type * as Signature from "effect/crypto/Signature";
import * as Jwks from "effect/jose/Jwks";
import * as FetchHttpClient from "effect/http/FetchHttpClient";
import * as Scope from "effect/Scope";
import type * as HttpClient from "effect/http/HttpClient";
import { resolveIamEndpoints } from "@hanzodev/shared/hanzoIamAuth";

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

/**
 * The issuer and key set come from OIDC discovery at `origin` (`resolveIamEndpoints`), never from
 * a configured host. A host with no discovery document yields no issuer or key set, and every
 * session is refused.
 */
export const layerIam = (options: {
  /** Where discovery is read: the IAM sign-in origin. */
  readonly origin: string;
  readonly audience: string;
  /** The IAM person this environment belongs to, as `<owner>/<name>`. */
  readonly principal: string;
  readonly fetch?: typeof globalThis.fetch;
}): Layer.Layer<ZapAuth, never, HttpClient.HttpClient | Signature.Signature> =>
  Layer.effect(ZapAuth)(
    Effect.gen(function* () {
      const context = yield* Effect.context<HttpClient.HttpClient | Signature.Signature>();
      const scope = yield* Effect.scope;
      // One remote key set per advertised jwks_uri; each caches and rotates its own keys.
      const keySets = new Map<string, Jwks.Jwks["Service"]>();
      const keySetFor = Effect.fn("ZapAuth.keySetFor")(function* (url: string) {
        const known = keySets.get(url);
        if (known !== undefined) return known;
        const made = yield* Jwks.makeRemote({ url }).pipe(
          Effect.provideContext(context),
          Effect.provideService(Scope.Scope, scope),
        );
        keySets.set(url, made);
        return made;
      });
      const invalid = new ZapAuthError({ reason: "invalid_token" });
      // The HTTP client reads these from the fiber making the request. A key set that redirects is
      // refused, so redirects are never followed.
      const forRequests = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
        effect.pipe(
          Effect.provideService(FetchHttpClient.RequestInit, { redirect: "manual" }),
          options.fetch === undefined
            ? (self) => self
            : Effect.provideService(FetchHttpClient.Fetch, options.fetch),
        );
      return {
        authenticate: Effect.fn("ZapAuth.authenticate")(function* (bearer: string) {
          const endpoints = yield* Effect.tryPromise({
            try: () => resolveIamEndpoints(options.origin, options.fetch),
            catch: () => invalid,
          });
          if (endpoints.issuer === null || endpoints.jwks === null) return yield* invalid;
          const keySet = yield* keySetFor(endpoints.jwks).pipe(
            forRequests,
            Effect.mapError(() => invalid),
          );
          const { claims, registeredClaims } = yield* Jwt.verifyWithKeySet(IamClaims, bearer, {
            algorithms: ["RS256", "ES256", "EdDSA"],
            issuer: endpoints.issuer,
            audience: options.audience,
            clockTolerance: "30 seconds",
          }).pipe(
            Effect.provideContext(context),
            Effect.provideService(Jwks.Jwks, keySet),
            forRequests,
            Effect.mapError(() => invalid),
          );
          // An access token with no expiry would hold a session open forever.
          if (registeredClaims.exp === undefined) return yield* invalid;
          const principal = `${claims.owner ?? ""}/${claims.name ?? ""}`;
          if (principal !== options.principal) {
            return yield* new ZapAuthError({ reason: "wrong_owner" });
          }
          return {
            sessionId: AuthSessionId.make(`iam:${principal}`),
            subject: principal,
            method: "bearer-access-token",
            scopes: AuthStandardClientScopes,
            expiresAt: DateTime.makeUnsafe(registeredClaims.exp * 1000),
          } satisfies AuthenticatedSession;
        }),
      };
    }),
  );
