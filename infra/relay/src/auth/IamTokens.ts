import { createRemoteJWKSet, jwtVerify, type JWTPayload } from "jose";

import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";

export class IamTokenVerificationFailed extends Schema.TaggedError<IamTokenVerificationFailed>()(
  "IamTokenVerificationFailed",
  { cause: Schema.Defect() },
) {
  override get message(): string {
    return "Hanzo IAM token verification failed";
  }
}

export interface VerifiedIamToken {
  readonly sub: string;
  readonly email: string | undefined;
  readonly aud: JWTPayload["aud"];
}

type KeySet = ReturnType<typeof createRemoteJWKSet>;

// One key set per issuer for the life of the isolate; jose caches and rotates the keys.
const keySets = new Map<string, Promise<KeySet>>();

function keySetFor(issuer: string): Promise<KeySet> {
  const known = keySets.get(issuer);
  if (known) return known;
  const loaded = (async () => {
    // @effect-diagnostics-next-line globalFetch:off -- one-time OIDC discovery inside the key-set loader jose caches.
    const response = await fetch(`${issuer}/.well-known/openid-configuration`);
    if (!response.ok) throw new Error(`OIDC discovery failed (${response.status}).`);
    const document = (await response.json()) as { readonly jwks_uri?: unknown };
    if (typeof document.jwks_uri !== "string") throw new Error("OIDC discovery has no jwks_uri.");
    return createRemoteJWKSet(new URL(document.jwks_uri));
  })();
  keySets.set(issuer, loaded);
  loaded.catch(() => keySets.delete(issuer));
  return loaded;
}

/** Verifies a Hanzo IAM access token (signature against the issuer's JWKS, `iss`, `aud`, `exp`). */
export const verifyIamAccessToken = (
  config: { readonly iamIssuer: string; readonly iamAudience: string },
  token: string,
) =>
  Effect.tryPromise({
    try: async (): Promise<VerifiedIamToken> => {
      const { payload } = await jwtVerify(token, await keySetFor(config.iamIssuer), {
        issuer: config.iamIssuer,
        audience: config.iamAudience,
      });
      if (!payload.sub) throw new Error("missing_subject");
      return {
        sub: payload.sub,
        email: typeof payload.email === "string" ? payload.email : undefined,
        aud: payload.aud,
      };
    },
    catch: (cause) => new IamTokenVerificationFailed({ cause }),
  }).pipe(
    Effect.withSpan("verify_iam_access_token", {
      attributes: { "relay.auth.token_length": token.length },
    }),
  );
