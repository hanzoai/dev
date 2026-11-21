import * as Clock from "effect/Clock";
import * as Duration from "effect/Duration";
import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";
import * as HttpClient from "effect/http/HttpClient";
import * as HttpClientRequest from "effect/http/HttpClientRequest";
import * as HttpClientResponse from "effect/http/HttpClientResponse";

const DISCOVERY_TIMEOUT = Duration.seconds(10);
const DISCOVERY_TTL_MS = 60 * 60_000;

const DiscoveryDocument = Schema.Struct({
  issuer: Schema.String,
  authorization_endpoint: Schema.String,
  token_endpoint: Schema.String,
  jwks_uri: Schema.String,
  device_authorization_endpoint: Schema.optional(Schema.String),
});

export interface IamEndpoints {
  readonly issuer: string;
  readonly authorizationEndpoint: string;
  readonly tokenEndpoint: string;
  readonly jwksUri: string;
  readonly deviceAuthorizationEndpoint: string | null;
}

export class IamDiscoveryError extends Schema.TaggedError<IamDiscoveryError>()(
  "IamDiscoveryError",
  { cause: Schema.Defect() },
) {
  override get message(): string {
    return "Could not read Hanzo's sign-in configuration. Check the connection and try again.";
  }
}

const isHttpsUrl = (value: string): boolean => {
  try {
    return new URL(value).protocol === "https:";
  } catch {
    return false;
  }
};

// One document per login origin; an unreachable or malformed document is never replaced by a default.
const cache = new Map<string, { readonly at: number; readonly endpoints: IamEndpoints }>();

/** The sign-in endpoints named by `<loginUrl>/.well-known/openid-configuration`. Fails closed. */
export const discoverIamEndpoints = Effect.fn("cloud.iam.discover")(function* (loginUrl: string) {
  const now = yield* Clock.currentTimeMillis;
  const cached = cache.get(loginUrl);
  if (cached !== undefined && now - cached.at < DISCOVERY_TTL_MS) return cached.endpoints;
  const client = (yield* HttpClient.HttpClient).pipe(HttpClient.filterStatusOk);
  const document = yield* HttpClientRequest.get(
    `${loginUrl}/.well-known/openid-configuration`,
  ).pipe(
    client.execute,
    Effect.flatMap(HttpClientResponse.schemaBodyJson(DiscoveryDocument)),
    Effect.timeout(DISCOVERY_TIMEOUT),
    Effect.mapError((cause) => new IamDiscoveryError({ cause })),
  );
  const urls = [
    document.issuer,
    document.authorization_endpoint,
    document.token_endpoint,
    document.jwks_uri,
    ...(document.device_authorization_endpoint === undefined
      ? []
      : [document.device_authorization_endpoint]),
  ];
  if (!urls.every(isHttpsUrl)) {
    return yield* new IamDiscoveryError({ cause: "Discovery named a non-HTTPS endpoint." });
  }
  const endpoints: IamEndpoints = {
    issuer: document.issuer,
    authorizationEndpoint: document.authorization_endpoint,
    tokenEndpoint: document.token_endpoint,
    jwksUri: document.jwks_uri,
    deviceAuthorizationEndpoint: document.device_authorization_endpoint ?? null,
  };
  cache.set(loginUrl, { at: now, endpoints });
  return endpoints;
});
