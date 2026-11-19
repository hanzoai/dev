import {
  DEFAULT_HANZO_IAM_CLIENT_ID,
  DEFAULT_HANZO_IAM_ISSUER,
  DEFAULT_HANZO_RELAY_URL,
  normalizeHanzoIssuer,
} from "@hanzodev/shared/hanzoIamAuth";
import {
  clerkFrontendApiHostnameFromPublishableKey,
  relayClerkTokenOptions,
} from "@hanzodev/shared/relayAuth";
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
  readonly iamIssuer: string;
  readonly iamClientId: string;
  readonly clerkPublishableKey: string | null;
  readonly clerkJwtTemplate: string | null;
  readonly relayUrl: string | null;
  readonly relayTracing: {
    readonly tracesUrl: string | null;
    readonly tracesDataset: string | null;
    readonly tracesToken: string | null;
  };
}

export function trimNonEmpty(value: string | undefined): string | null {
  return value?.trim() || null;
}

function normalizeSecureUrl(value: string): string | null {
  try {
    const url = new URL(value);
    return url.protocol === "https:" ? url.toString() : null;
  } catch {
    return null;
  }
}

export function resolveCloudPublicConfig(): CloudPublicConfig {
  const configuredRelayRaw = trimNonEmpty(
    import.meta.env.VITE_HANZO_DEV_RELAY_URL as string | undefined,
  );
  const relayUrl = configuredRelayRaw
    ? normalizeSecureRelayUrl(configuredRelayRaw)
    : DEFAULT_HANZO_RELAY_URL;
  return {
    iamIssuer: normalizeHanzoIssuer(
      trimNonEmpty(import.meta.env.VITE_HANZO_DEV_IAM_ISSUER as string | undefined) ??
        DEFAULT_HANZO_IAM_ISSUER,
    ),
    iamClientId:
      trimNonEmpty(import.meta.env.VITE_HANZO_DEV_IAM_CLIENT_ID as string | undefined) ??
      DEFAULT_HANZO_IAM_CLIENT_ID,
    clerkPublishableKey: trimNonEmpty(
      import.meta.env.VITE_CLERK_PUBLISHABLE_KEY as string | undefined,
    ),
    clerkJwtTemplate: trimNonEmpty(import.meta.env.VITE_CLERK_JWT_TEMPLATE as string | undefined),
    relayUrl,
    relayTracing: {
      tracesUrl: normalizeSecureUrl(
        (import.meta.env.VITE_RELAY_OTLP_TRACES_URL as string | undefined) ?? "",
      ),
      tracesDataset: trimNonEmpty(
        import.meta.env.VITE_RELAY_OTLP_TRACES_DATASET as string | undefined,
      ),
      tracesToken: trimNonEmpty(import.meta.env.VITE_RELAY_OTLP_TRACES_TOKEN as string | undefined),
    },
  };
}

let desktopRelayTelemetry: boolean | null = null;

/** Whether relay spans may be exported now; the desktop host may not know yet at startup. */
export function relayTelemetryEnabled(): boolean | null {
  if (typeof window === "undefined" || !window.desktopBridge) return true;
  desktopRelayTelemetry ??= window.desktopBridge.getRelayTelemetryEnabled();
  return desktopRelayTelemetry;
}

export function resolveRelayTracingConfig() {
  // The relay exporter is created during module initialization, so this must be synchronous.
  if (relayTelemetryEnabled() === false) {
    return null;
  }
  const { relayTracing } = resolveCloudPublicConfig();
  return relayTracing.tracesUrl && relayTracing.tracesDataset && relayTracing.tracesToken
    ? {
        tracesUrl: relayTracing.tracesUrl,
        tracesDataset: relayTracing.tracesDataset,
        tracesToken: relayTracing.tracesToken,
      }
    : null;
}

export function hasCloudPublicConfig(): boolean {
  const config = resolveCloudPublicConfig();
  if (!config.relayUrl) {
    return false;
  }

  if (!config.clerkPublishableKey) {
    return Boolean(config.iamIssuer && config.iamClientId);
  }

  if (!config.clerkJwtTemplate) {
    return false;
  }

  // Electron has its own auth runtime; development keys also support private browser origins.
  if (
    typeof window === "undefined" ||
    window.desktopBridge !== undefined ||
    !config.clerkPublishableKey.startsWith("pk_live_")
  ) {
    return true;
  }

  try {
    // Custom Frontend API hosts use clerk.<production domain>.
    const domain = clerkFrontendApiHostnameFromPublishableKey(config.clerkPublishableKey).replace(
      /^clerk\./u,
      "",
    );
    const { hostname, protocol, port } = window.location;
    return (
      protocol === "https:" &&
      port === "" &&
      (hostname === domain || hostname.endsWith(`.${domain}`))
    );
  } catch {
    return false;
  }
}

export function resolveRelayClerkTokenOptions() {
  const { clerkPublishableKey, clerkJwtTemplate } = resolveCloudPublicConfig();
  if (clerkPublishableKey && !clerkJwtTemplate) {
    throw new CloudPublicConfigMissingError({ key: "HANZO_DEV_CLERK_JWT_TEMPLATE" });
  }
  return relayClerkTokenOptions(clerkJwtTemplate ?? "hanzo-relay");
}
