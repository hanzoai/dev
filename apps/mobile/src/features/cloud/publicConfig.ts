import Constants from "expo-constants";
import {
  DEFAULT_HANZO_IAM_CLIENT_ID,
  DEFAULT_HANZO_IAM_ISSUER,
  DEFAULT_HANZO_RELAY_URL,
  normalizeHanzoIssuer,
} from "@hanzodev/shared/hanzoIamAuth";
import { relayClerkTokenOptions } from "@hanzodev/shared/relayAuth";
import { normalizeSecureRelayUrl } from "@hanzodev/shared/relayUrl";
import * as Schema from "effect/Schema";

export class CloudPublicConfigMissingError extends Schema.TaggedError<CloudPublicConfigMissingError>()(
  "CloudPublicConfigMissingError",
  {
    key: Schema.Literal("HANZO_DEV_CLERK_JWT_TEMPLATE"),
  },
) {
  override get message(): string {
    return `${this.key} is not configured.`;
  }
}

export interface CloudPublicConfig {
  readonly iam: {
    readonly issuer: string;
    readonly clientId: string;
  };
  readonly clerk: {
    readonly publishableKey: string | null;
    readonly jwtTemplate: string | null;
  };
  readonly relay: {
    readonly url: string | null;
  };
  readonly observability: {
    readonly tracesUrl: string | null;
    readonly tracesDataset: string | null;
    readonly tracesToken: string | null;
  };
}

type UntrustedSection<T> = {
  readonly [Key in keyof T]?: unknown;
};

type ExpoExtra =
  | {
      readonly [Section in keyof CloudPublicConfig]?: UntrustedSection<CloudPublicConfig[Section]>;
    }
  | undefined;

function trimNonEmpty(value: unknown): string | null {
  return typeof value === "string" && value.trim() ? value.trim() : null;
}

function normalizeSecureUrl(value: unknown): string | null {
  const raw = trimNonEmpty(value);
  if (raw === null) {
    return null;
  }
  try {
    const url = new URL(raw);
    return url.protocol === "https:" ? url.toString() : null;
  } catch {
    return null;
  }
}

export function resolveCloudPublicConfig(extra: ExpoExtra = Constants.expoConfig?.extra) {
  const relayInput = trimNonEmpty(extra?.relay?.url) ?? DEFAULT_HANZO_RELAY_URL;
  return {
    iam: {
      issuer: normalizeHanzoIssuer(trimNonEmpty(extra?.iam?.issuer) ?? DEFAULT_HANZO_IAM_ISSUER),
      clientId: trimNonEmpty(extra?.iam?.clientId) ?? DEFAULT_HANZO_IAM_CLIENT_ID,
    },
    clerk: {
      publishableKey: trimNonEmpty(extra?.clerk?.publishableKey),
      jwtTemplate: trimNonEmpty(extra?.clerk?.jwtTemplate),
    },
    relay: {
      url: normalizeSecureRelayUrl(relayInput),
    },
    observability: {
      tracesUrl: normalizeSecureUrl(extra?.observability?.tracesUrl),
      tracesDataset: trimNonEmpty(extra?.observability?.tracesDataset),
      tracesToken: trimNonEmpty(extra?.observability?.tracesToken),
    },
  } satisfies CloudPublicConfig;
}

export function hasCloudPublicConfig(extra: ExpoExtra = Constants.expoConfig?.extra): boolean {
  const config = resolveCloudPublicConfig(extra);
  return Boolean(config.iam.issuer && config.iam.clientId && config.relay.url);
}

type Configured<T> = {
  readonly [Key in keyof T]: NonNullable<T[Key]>;
};

type TracingPublicConfig = Omit<CloudPublicConfig, "observability"> & {
  readonly observability: Configured<CloudPublicConfig["observability"]>;
};

export function hasTracingPublicConfig(
  config: CloudPublicConfig = resolveCloudPublicConfig(),
): config is TracingPublicConfig {
  return Boolean(
    config.observability.tracesUrl &&
    config.observability.tracesDataset &&
    config.observability.tracesToken,
  );
}

export function resolveRelayClerkTokenOptions() {
  const jwtTemplate = resolveCloudPublicConfig().clerk.jwtTemplate ?? "hanzo-relay";
  return relayClerkTokenOptions(jwtTemplate);
}
