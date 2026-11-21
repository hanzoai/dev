import type { UsageSettings, UsageSummary } from "@hanzodev/usage/schema";

import { readDesktopPrimaryBearerToken } from "../environments/primary/desktopAuth";
import { resolvePrimaryEnvironmentHttpUrl } from "../environments/primary/target";

async function call<T>(method: "GET" | "POST" | "PUT", path: string, body?: unknown): Promise<T> {
  const headers: Record<string, string> = { Accept: "application/json" };
  let credentials: RequestCredentials = "include";
  const token = await readDesktopPrimaryBearerToken().catch(() => null);
  if (token) {
    headers.Authorization = `Bearer ${token}`;
    credentials = "omit";
  }
  if (body !== undefined) headers["Content-Type"] = "application/json";
  const response = await fetch(resolvePrimaryEnvironmentHttpUrl(path), {
    method,
    headers,
    credentials,
    ...(body !== undefined ? { body: JSON.stringify(body) } : {}),
  });
  if (!response.ok) throw new Error(`Usage request failed (${response.status}).`);
  return (await response.json()) as T;
}

export const fetchUsage = () => call<UsageSummary>("GET", "/v1/usage");
export const refreshUsage = () => call<UsageSummary>("POST", "/v1/usage/refresh");
export const fetchUsageSettings = () => call<UsageSettings>("GET", "/v1/usage/settings");
export const saveUsageSettings = (settings: UsageSettings) =>
  call<UsageSettings>("PUT", "/v1/usage/settings", settings);
export const connectUsage = (provider: string) =>
  call<UsageSummary>("POST", "/v1/usage/connect", { provider });
