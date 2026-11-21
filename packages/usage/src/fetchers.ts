// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
/**
 * Live fetchers. Each reads the provider's own local session or an official API with a key the
 * user already holds, and returns a ProviderSnapshot. Credentials stay on this machine: nothing
 * here sends a token anywhere except the provider it belongs to.
 *
 * Request shapes are ported from CodexBar (MIT, Copyright (c) 2026 Peter Steinberger); see NOTICE.
 */
import type { UsageEnv } from "./env.ts";
import type { ProviderSnapshot, UsageWindow } from "./schema.ts";

/** A configured provider instance: one account the user signed in to. `home` is its config directory when it has one. */
export interface Instance {
  readonly id: string;
  readonly name: string;
  readonly home?: string;
  readonly binary?: string;
}

/** One reading per account the provider has on this machine. */
export type Fetcher = (
  env: UsageEnv,
  instances: ReadonlyArray<Instance>,
) => Promise<ReadonlyArray<ProviderSnapshot>>;

/** A reader of one credential. */
export type SingleFetcher = (env: UsageEnv) => Promise<ProviderSnapshot>;

export class NotConfigured extends Error {}

export const obj = (v: unknown): Record<string, unknown> =>
  v && typeof v === "object" ? (v as Record<string, unknown>) : {};
export const num = (v: unknown): number | undefined => {
  const n = typeof v === "string" ? Number(v) : v;
  return typeof n === "number" && Number.isFinite(n) ? n : undefined;
};
export const str = (v: unknown): string | undefined => (typeof v === "string" && v ? v : undefined);
export const clamp = (n: number) => Math.min(100, Math.max(0, n));
export const iso = (v: unknown, epochSeconds = false): string | undefined => {
  if (typeof v === "string")
    return Number.isNaN(Date.parse(v)) ? undefined : new Date(v).toISOString();
  const n = num(v);
  return n === undefined ? undefined : new Date(epochSeconds ? n * 1000 : n).toISOString();
};
export const json = async (res: Response): Promise<Record<string, unknown>> => {
  if (res.status === 401 || res.status === 403) throw new Error("Sign in again.");
  if (res.status === 429) throw new Error("Rate limited by the provider; will retry.");
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return obj(await res.json());
};
export const base = (env: UsageEnv, provider: string, source: ProviderSnapshot["source"]) => ({
  provider,
  fetchedAt: env.now().toISOString(),
  source,
});
export const parseJson = (text: string | undefined): Record<string, unknown> => {
  if (!text) return {};
  try {
    return obj(JSON.parse(text));
  } catch {
    return {};
  }
};
export const firstEnv = (env: UsageEnv, keys: ReadonlyArray<string>) =>
  keys.map((k) => env.env[k]?.trim()).find((v): v is string => !!v);

const copilotToken = async (env: UsageEnv): Promise<string | undefined> => {
  const direct = firstEnv(env, ["COPILOT_API_TOKEN"]);
  if (direct) return direct;
  for (const name of ["apps.json", "hosts.json"]) {
    const file = parseJson(await env.readText(`${env.home}/.config/github-copilot/${name}`));
    for (const entry of Object.values(file)) {
      const t = str(obj(entry).oauth_token);
      if (t) return t;
    }
  }
  // The GitHub CLI's own sign-in; Copilot's usage endpoint accepts it.
  const out = await env.exec("gh", ["auth", "token"], {
    env: {
      HOME: env.home,
      PATH: `${env.home}/.local/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin`,
    },
    cwd: env.home,
    timeoutMs: 10_000,
  });
  return out?.trim() || undefined;
};

export const parseCopilot = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "plan" | "account"> => {
  const snaps = obj(body.quota_snapshots);
  const resetsAt = iso(body.quota_reset_date);
  const win = (label: string, raw: unknown): UsageWindow | undefined => {
    const s = obj(raw);
    // A plan with no entitlement for this quota reports 0% remaining; that is absent, not exhausted.
    if (s.unlimited === true || s.has_quota === false) return undefined;
    const remaining = num(s.percent_remaining);
    if (remaining === undefined) return undefined;
    return {
      kind: "monthly",
      label,
      usedPercent: clamp(100 - remaining),
      ...(resetsAt ? { resetsAt } : {}),
    };
  };
  const plan = str(body.copilot_plan);
  return {
    windows: [
      win("Premium", snaps.premium_interactions),
      win("Chat", snaps.chat),
      win("Completions", snaps.completions),
    ].filter((w): w is UsageWindow => !!w),
    ...(plan ? { plan: plan.charAt(0).toUpperCase() + plan.slice(1) } : {}),
    ...(str(body.login) ? { account: str(body.login)! } : {}),
  };
};

export const copilot: SingleFetcher = async (env) => {
  const token = await copilotToken(env);
  if (!token) throw new NotConfigured("Sign in to GitHub Copilot in your editor or CLI.");
  const res = await env.fetch("https://api.github.com/copilot_internal/user", {
    headers: {
      Authorization: `token ${token}`,
      Accept: "application/json",
      "Editor-Version": "vscode/1.96.2",
      "X-Github-Api-Version": "2025-04-01",
    },
  });
  return { ...base(env, "copilot", "oauth"), ...parseCopilot(await json(res)) };
};

export const openrouter: SingleFetcher = async (env) => {
  const key = firstEnv(env, ["OPENROUTER_API_KEY"]);
  if (!key) throw new NotConfigured("Set OPENROUTER_API_KEY.");
  const data = obj(
    (
      await json(
        await env.fetch("https://openrouter.ai/api/v1/credits", {
          headers: { Authorization: `Bearer ${key}` },
        }),
      )
    ).data,
  );
  const total = num(data.total_credits);
  const spent = num(data.total_usage);
  const usedPercent = total && spent !== undefined ? clamp((spent / total) * 100) : 0;
  return {
    ...base(env, "openrouter", "api"),
    windows: total ? [{ kind: "credits", label: "Credits", usedPercent }] : [],
    credits: {
      currency: "USD",
      ...(total !== undefined && spent !== undefined ? { balance: total - spent } : {}),
      ...(spent !== undefined ? { spent } : {}),
    },
  };
};

export const deepseek: SingleFetcher = async (env) => {
  const key = firstEnv(env, ["DEEPSEEK_API_KEY"]);
  if (!key) throw new NotConfigured("Set DEEPSEEK_API_KEY.");
  const body = await json(
    await env.fetch("https://api.deepseek.com/user/balance", {
      headers: { Authorization: `Bearer ${key}` },
    }),
  );
  const info = obj(Array.isArray(body.balance_infos) ? body.balance_infos[0] : undefined);
  const balance = num(info.total_balance);
  return {
    ...base(env, "deepseek", "api"),
    windows: [],
    credits: {
      currency: str(info.currency) ?? "CNY",
      ...(balance !== undefined ? { balance } : {}),
    },
  };
};

/** Credential-only providers: one reading from one key or session. */
export const single =
  (fetcher: SingleFetcher): Fetcher =>
  async (env) => [await fetcher(env)];

export const SINGLE_FETCHERS: Readonly<Record<string, Fetcher>> = {
  copilot: single(copilot),
  openrouter: single(openrouter),
  deepseek: single(deepseek),
};
