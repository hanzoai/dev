import {
  connectLoopbackRedirectUri,
  CONNECT_OAUTH_SCOPES,
  DEFAULT_HOSTED_APP_URL,
} from "@hanzodev/shared/connectAuth";
import {
  DEFAULT_HANZO_API_BASE_URL,
  DEFAULT_HANZO_LOGIN_URL,
  DEFAULT_HANZO_IAM_CLIENT_ID,
  DEFAULT_HANZO_IAM_ISSUER,
} from "@hanzodev/shared/hanzoIamAuth";
import { discoverIamEndpoints } from "./iamDiscovery.ts";
import { isRelayTracingEnabled } from "@hanzodev/shared/otelEnvironment";
import { normalizeSecureRelayUrl } from "@hanzodev/shared/relayUrl";
import * as Config from "effect/Config";
import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";
import * as SchemaIssue from "effect/SchemaIssue";

declare const __HANZO_DEV_BUILD_RELAY_URL__: string | undefined;
declare const __HANZO_DEV_BUILD_CHANNEL__: "nightly" | "latest" | undefined;
declare const __HANZO_DEV_BUILD_IAM_ISSUER__: string | undefined;
declare const __HANZO_DEV_BUILD_IAM_CLIENT_ID__: string | undefined;
declare const __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_URL__: string | undefined;
declare const __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_DATASET__: string | undefined;
declare const __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_TOKEN__: string | undefined;

const CLOUD_CLI_OAUTH_LOOPBACK_PORT = 34338;
const CLOUD_CLI_OAUTH_SCOPES = CONNECT_OAUTH_SCOPES;

function validateRelayUrl(value: string) {
  const relayUrl = normalizeSecureRelayUrl(value);
  return relayUrl === null
    ? Effect.fail(
        new Config.ConfigError(
          new Schema.SchemaError(
            new SchemaIssue.InvalidValue({
              message: "Relay URL must be a secure absolute HTTPS origin.",
            }),
          ),
        ),
      )
    : Effect.succeed(relayUrl);
}

function readBuildTimeValue(value: string | undefined): string {
  return typeof value === "undefined" ? "" : value.trim();
}

function normalizeSecureUrl(value: string): string | null {
  try {
    const url = new URL(value);
    return url.protocol === "https:" ? url.toString() : null;
  } catch {
    return null;
  }
}

const buildTimeRelayUrl =
  typeof __HANZO_DEV_BUILD_RELAY_URL__ === "undefined"
    ? ""
    : (normalizeSecureRelayUrl(__HANZO_DEV_BUILD_RELAY_URL__) ?? "");
const buildTimeIamIssuer = readBuildTimeValue(
  typeof __HANZO_DEV_BUILD_IAM_ISSUER__ === "undefined"
    ? undefined
    : __HANZO_DEV_BUILD_IAM_ISSUER__,
);
const buildTimeIamClientId = readBuildTimeValue(
  typeof __HANZO_DEV_BUILD_IAM_CLIENT_ID__ === "undefined"
    ? undefined
    : __HANZO_DEV_BUILD_IAM_CLIENT_ID__,
);
const buildTimeRelayClientTracing = {
  tracesUrl: readBuildTimeValue(
    typeof __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_URL__ === "undefined"
      ? undefined
      : __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_URL__,
  ),
  tracesDataset: readBuildTimeValue(
    typeof __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_DATASET__ === "undefined"
      ? undefined
      : __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_DATASET__,
  ),
  tracesToken: readBuildTimeValue(
    typeof __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_TOKEN__ === "undefined"
      ? undefined
      : __HANZO_DEV_BUILD_RELAY_CLIENT_OTLP_TRACES_TOKEN__,
  ),
} as const;

export function resolveRelayClientTracingConfig(
  env: Readonly<Record<string, string | undefined>> = process.env,
  fallback = buildTimeRelayClientTracing,
) {
  if (!isRelayTracingEnabled(env)) {
    return null;
  }

  const tracesUrl = env.HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_URL?.trim() || fallback.tracesUrl;
  const tracesDataset =
    env.HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_DATASET?.trim() || fallback.tracesDataset;
  const tracesToken = env.HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_TOKEN?.trim() || fallback.tracesToken;
  const normalizedTracesUrl = normalizeSecureUrl(tracesUrl);
  return normalizedTracesUrl && tracesDataset && tracesToken
    ? { tracesUrl: normalizedTracesUrl, tracesDataset, tracesToken }
    : null;
}

export function makeRelayUrlConfig(fallback = buildTimeRelayUrl) {
  const runtimeConfig = Config.NonEmptyString("HANZO_DEV_RELAY_URL");
  return (fallback ? runtimeConfig.pipe(Config.withDefault(fallback)) : runtimeConfig).pipe(
    Config.mapEffect(validateRelayUrl),
  );
}

export const relayUrlConfig = makeRelayUrlConfig();

export const hostedAppUrlConfig = makePublicValueConfig(
  "HANZO_DEV_HOSTED_APP_URL",
  DEFAULT_HOSTED_APP_URL,
).pipe(Config.mapEffect(validateOriginUrl));

/** The unified Hanzo API origin; `/v1/desktops` (HIP-1350) is served here. */
export const apiBaseUrlConfig = makePublicValueConfig(
  "HANZO_DEV_API_URL",
  DEFAULT_HANZO_API_BASE_URL,
).pipe(Config.mapEffect(validateOriginUrl));

const NIGHTLY_HOSTED_APP_URL = "https://nightly.hanzo.bot";

/**
 * Where agent sign-ins on the Hanzo Link address are approved. A nightly build defaults to the
 * nightly hosted app, which ships the same approval page; hanzo.bot serves stable.
 */
export const agentApprovalAppUrlConfig = makePublicValueConfig(
  "HANZO_DEV_HOSTED_APP_URL",
  typeof __HANZO_DEV_BUILD_CHANNEL__ !== "undefined" && __HANZO_DEV_BUILD_CHANNEL__ === "nightly"
    ? NIGHTLY_HOSTED_APP_URL
    : DEFAULT_HOSTED_APP_URL,
).pipe(Config.mapEffect(validateOriginUrl));

function validateOriginUrl(value: string) {
  try {
    const url = new URL(value);
    const isLoopbackHttp =
      url.protocol === "http:" &&
      (url.hostname === "localhost" || url.hostname === "127.0.0.1" || url.hostname === "[::1]");
    if (
      (url.protocol !== "https:" && !isLoopbackHttp) ||
      url.pathname !== "/" ||
      url.search !== "" ||
      url.hash !== ""
    ) {
      throw new Error("invalid hosted app origin");
    }
    return Effect.succeed(url.origin);
  } catch {
    return Effect.fail(
      new Config.ConfigError(
        new Schema.SchemaError(
          new SchemaIssue.InvalidValue({
            message: "Public URL must be an absolute HTTPS origin (or HTTP loopback origin).",
          }),
        ),
      ),
    );
  }
}

function makePublicValueConfig(name: string, fallback: string) {
  const runtimeConfig = Config.NonEmptyString(name);
  return (fallback ? runtimeConfig.pipe(Config.withDefault(fallback)) : runtimeConfig).pipe(
    Config.map((value) => value.trim()),
  );
}

/** Where people sign in and where the sign-in configuration is published. */
export const loginUrlConfig = makePublicValueConfig(
  "HANZO_DEV_LOGIN_URL",
  DEFAULT_HANZO_LOGIN_URL,
).pipe(Config.mapEffect(validateOriginUrl));

export interface CloudCliOAuthConfig {
  readonly authorizationEndpoint: string;
  readonly tokenEndpoint: string;
  readonly deviceAuthorizationEndpoint: string | null;
  readonly clientId: string;
  readonly loopbackPort: number;
  readonly redirectUri: string;
  readonly scopes: typeof CLOUD_CLI_OAUTH_SCOPES;
}

/** The Hanzo IAM this build signs people in with. */
export function makeHanzoIamConfig({
  iamIssuerFallback = buildTimeIamIssuer || DEFAULT_HANZO_IAM_ISSUER,
  iamClientIdFallback = buildTimeIamClientId || DEFAULT_HANZO_IAM_CLIENT_ID,
}: {
  readonly iamIssuerFallback?: string;
  readonly iamClientIdFallback?: string;
} = {}) {
  return Config.all({
    iamIssuer: makePublicValueConfig("HANZO_DEV_IAM_ISSUER", iamIssuerFallback),
    iamClientId: makePublicValueConfig("HANZO_DEV_IAM_CLIENT_ID", iamClientIdFallback),
  });
}

/**
 * Token and device endpoints come from the login host's discovery document, not from a
 * constant, so a change of identity host needs no release.
 */
export const makeCloudCliOAuthConfig = Effect.fn("cloud.cli_oauth.config")(function* (
  options: Parameters<typeof makeHanzoIamConfig>[0] = {},
) {
  const { iamClientId } = yield* makeHanzoIamConfig(options);
  const loginUrl = yield* loginUrlConfig;
  const endpoints = yield* discoverIamEndpoints(loginUrl);
  return {
    authorizationEndpoint: endpoints.authorizationEndpoint,
    tokenEndpoint: endpoints.tokenEndpoint,
    deviceAuthorizationEndpoint: endpoints.deviceAuthorizationEndpoint,
    clientId: iamClientId,
    loopbackPort: CLOUD_CLI_OAUTH_LOOPBACK_PORT,
    redirectUri: connectLoopbackRedirectUri(CLOUD_CLI_OAUTH_LOOPBACK_PORT),
    scopes: CLOUD_CLI_OAUTH_SCOPES,
  } satisfies CloudCliOAuthConfig;
});

export const cloudCliOAuthConfig = Effect.suspend(() => makeCloudCliOAuthConfig());

export const hasCloudPublicConfig = Boolean(
  normalizeSecureRelayUrl(process.env.HANZO_DEV_RELAY_URL ?? "") ?? buildTimeRelayUrl,
);
