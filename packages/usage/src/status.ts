import type { ProviderDescriptor } from "./catalog.ts";
import type { UsageEnv } from "./env.ts";
import type { Incident, ProviderStatus, StatusIndicator } from "./schema.ts";

const indicator = (v: unknown): StatusIndicator =>
  v === "none" || v === "minor" || v === "major" || v === "critical" || v === "maintenance"
    ? v
    : "unknown";

/** Reads a statuspage.io summary. Any failure is "unknown", never an error: a status page must not break the panel. */
export const fetchStatus = async (
  env: UsageEnv,
  p: ProviderDescriptor,
): Promise<ProviderStatus | undefined> => {
  if (!p.statusPage) return undefined;
  const checkedAt = env.now().toISOString();
  try {
    const res = await env.fetch(`${p.statusPage}/api/v2/summary.json`, {
      headers: { Accept: "application/json" },
    });
    if (!res.ok) return { indicator: "unknown", incidents: [], checkedAt };
    const body = (await res.json()) as {
      status?: { indicator?: string; description?: string };
      incidents?: Array<{ name?: string; impact?: string; shortlink?: string }>;
    };
    const incidents: Incident[] = (body.incidents ?? []).map((i) => ({
      name: i.name ?? "Incident",
      impact: indicator(i.impact),
      ...(i.shortlink ? { url: i.shortlink } : {}),
    }));
    return {
      indicator: indicator(body.status?.indicator),
      ...(body.status?.description ? { description: body.status.description } : {}),
      incidents,
      checkedAt,
    };
  } catch {
    return { indicator: "unknown", incidents: [], checkedAt };
  }
};
