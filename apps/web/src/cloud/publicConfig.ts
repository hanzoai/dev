import {
  DEFAULT_HANZO_IAM_CLIENT_ID,
  DEFAULT_HANZO_IAM_ISSUER,
  normalizeHanzoIssuer,
} from "@hanzodev/shared/hanzoIamAuth";
import { normalizeSecureRelayUrl } from "@hanzodev/shared/relayUrl";

export interface CloudPublicConfig {
  readonly iamIssuer: string;
  readonly iamClientId: string;
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
  const relayUrl = configuredRelayRaw ? normalizeSecureRelayUrl(configuredRelayRaw) : null;
  return {
    iamIssuer: normalizeHanzoIssuer(
      trimNonEmpty(import.meta.env.VITE_HANZO_DEV_IAM_ISSUER as string | undefined) ??
        DEFAULT_HANZO_IAM_ISSUER,
    ),
    iamClientId:
      trimNonEmpty(import.meta.env.VITE_HANZO_DEV_IAM_CLIENT_ID as string | undefined) ??
      DEFAULT_HANZO_IAM_CLIENT_ID,
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
  // The account and sign-in need the identity provider only; a linked Mac is reached over ZAP, not a relay.
  return Boolean(config.iamIssuer && config.iamClientId);
}
