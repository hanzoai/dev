import { resolveIamEndpoints } from "@hanzodev/shared/hanzoIamAuth";
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

interface Trust {
  readonly issuer: string;
  readonly keys: ReturnType<typeof createRemoteJWKSet>;
}

// One trust record per IAM origin for the life of the isolate; jose caches and rotates the keys.
const trusts = new Map<string, Promise<Trust>>();

function trustFor(origin: string): Promise<Trust> {
  const known = trusts.get(origin);
  if (known) return known;
  const loaded = (async () => {
    // The issuer and the keys are what the origin's discovery document names; neither is assumed.
    const found = await resolveIamEndpoints(origin);
    // Verification is strict where sign-in is lenient: no issuer or keys named, no trust.
    if (found.issuer === null || found.jwks === null) {
      throw new Error("OIDC discovery names no issuer or jwks_uri.");
    }
    return { issuer: found.issuer, keys: createRemoteJWKSet(new URL(found.jwks)) };
  })();
  trusts.set(origin, loaded);
  loaded.catch(() => trusts.delete(origin));
  return loaded;
}

/** Verifies a Hanzo IAM access token (signature against the issuer's JWKS, `iss`, `aud`, `exp`). */
export const verifyIamAccessToken = (
  config: { readonly iamOrigin: string; readonly iamAudience: string },
  token: string,
) =>
  Effect.tryPromise({
    try: async (): Promise<VerifiedIamToken> => {
      const trust = await trustFor(config.iamOrigin);
      const { payload } = await jwtVerify(token, trust.keys, {
        issuer: trust.issuer,
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
