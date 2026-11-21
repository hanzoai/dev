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

export type Fetcher = (env: UsageEnv) => Promise<ProviderSnapshot>;

export class NotConfigured extends Error {}

const obj = (v: unknown): Record<string, unknown> =>
  v && typeof v === "object" ? (v as Record<string, unknown>) : {};
const num = (v: unknown): number | undefined => {
  const n = typeof v === "string" ? Number(v) : v;
  return typeof n === "number" && Number.isFinite(n) ? n : undefined;
};
const str = (v: unknown): string | undefined => (typeof v === "string" && v ? v : undefined);
const clamp = (n: number) => Math.min(100, Math.max(0, n));
const iso = (v: unknown, epochSeconds = false): string | undefined => {
  if (typeof v === "string")
    return Number.isNaN(Date.parse(v)) ? undefined : new Date(v).toISOString();
  const n = num(v);
  return n === undefined ? undefined : new Date(epochSeconds ? n * 1000 : n).toISOString();
};
const json = async (res: Response): Promise<Record<string, unknown>> => {
  if (res.status === 401 || res.status === 403)
    throw new Error("Sign in again with the provider's own CLI.");
  if (res.status === 429) throw new Error("Rate limited by the provider; will retry.");
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return obj(await res.json());
};
const base = (env: UsageEnv, provider: string, source: ProviderSnapshot["source"]) => ({
  provider,
  fetchedAt: env.now().toISOString(),
  source,
});
const parseJson = (text: string | undefined): Record<string, unknown> => {
  if (!text) return {};
  try {
    return obj(JSON.parse(text));
  } catch {
    return {};
  }
};
const firstEnv = (env: UsageEnv, keys: ReadonlyArray<string>) =>
  keys.map((k) => env.env[k]?.trim()).find((v): v is string => !!v);

const claudeToken = async (env: UsageEnv): Promise<string | undefined> => {
  const fromEnv = env.env.CLAUDE_CODE_OAUTH_TOKEN?.trim();
  if (fromEnv) return fromEnv;
  const file = parseJson(await env.readText(`${env.home}/.claude/.credentials.json`));
  const fromFile = str(obj(file.claudeAiOauth).accessToken);
  if (fromFile) return fromFile;
  const keychain = parseJson(await env.keychain("Claude Code-credentials"));
  return str(obj(keychain.claudeAiOauth).accessToken);
};

const claudeWindow = (
  kind: UsageWindow["kind"],
  label: string,
  minutes: number,
  raw: unknown,
): UsageWindow | undefined => {
  const w = obj(raw);
  const used = num(w.utilization);
  if (used === undefined) return undefined;
  const resetsAt = iso(w.resets_at);
  return {
    kind,
    label,
    usedPercent: clamp(used),
    windowMinutes: minutes,
    ...(resetsAt ? { resetsAt } : {}),
  };
};

export const parseClaude = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "credits"> => {
  const windows = [
    claudeWindow("session", "Session", 300, body.five_hour),
    claudeWindow("weekly", "Weekly", 10080, body.seven_day),
    claudeWindow("weekly", "Sonnet", 10080, body.seven_day_sonnet),
    claudeWindow("weekly", "Opus", 10080, body.seven_day_opus),
  ].filter((w): w is UsageWindow => !!w);
  const extra = obj(body.extra_usage);
  const limit = num(extra.monthly_limit);
  const spent = num(extra.used_credits);
  const credits =
    extra.is_enabled && limit !== undefined
      ? {
          currency: "USD",
          limit: limit / 100,
          ...(spent !== undefined ? { spent: spent / 100 } : {}),
        }
      : undefined;
  return { windows, ...(credits ? { credits } : {}) };
};

export const claude: Fetcher = async (env) => {
  const token = await claudeToken(env);
  if (!token) throw new NotConfigured("Run `claude` to sign in.");
  const res = await env.fetch("https://api.anthropic.com/api/oauth/usage", {
    headers: {
      Authorization: `Bearer ${token}`,
      Accept: "application/json",
      "anthropic-beta": "oauth-2025-04-20",
      "User-Agent": "claude-cli/2.1.0 (external, cli)",
    },
  });
  return { ...base(env, "claude", "oauth"), ...parseClaude(await json(res)) };
};

export const parseCodex = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "credits" | "plan"> => {
  const limits = obj(body.rate_limit);
  const win = (kind: UsageWindow["kind"], label: string, raw: unknown): UsageWindow | undefined => {
    const w = obj(raw);
    const used = num(w.used_percent);
    if (used === undefined) return undefined;
    const seconds = num(w.limit_window_seconds);
    const resetsAt = iso(w.reset_at, true);
    return {
      kind,
      label,
      usedPercent: clamp(used),
      ...(seconds ? { windowMinutes: Math.round(seconds / 60) } : {}),
      ...(resetsAt ? { resetsAt } : {}),
    };
  };
  const secondary = obj(limits.secondary_window);
  const weekly = (num(secondary.limit_window_seconds) ?? 604800) >= 604800;
  const windows = [
    win("session", "Session", limits.primary_window),
    win(weekly ? "weekly" : "other", weekly ? "Weekly" : "Secondary", secondary),
  ].filter((w): w is UsageWindow => !!w);
  const balance = num(obj(body.credits).balance);
  const plan = str(body.plan_type);
  return {
    windows,
    ...(balance !== undefined ? { credits: { currency: "credits", balance } } : {}),
    ...(plan ? { plan } : {}),
  };
};

export const codex: Fetcher = async (env) => {
  const home = env.env.CODEX_HOME?.trim() || `${env.home}/.codex`;
  const auth = parseJson(await env.readText(`${home}/auth.json`));
  const tokens = obj(auth.tokens);
  const token = str(tokens.access_token);
  if (!token) throw new NotConfigured("Run `codex login`.");
  const accountId = str(tokens.account_id);
  const res = await env.fetch("https://chatgpt.com/backend-api/wham/usage", {
    headers: {
      Authorization: `Bearer ${token}`,
      Accept: "application/json",
      ...(accountId ? { "ChatGPT-Account-Id": accountId } : {}),
    },
  });
  return { ...base(env, "codex", "oauth"), ...parseCodex(await json(res)) };
};

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
  return undefined;
};

export const parseCopilot = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "plan"> => {
  const snaps = obj(body.quota_snapshots);
  const resetsAt = iso(body.quota_reset_date);
  const win = (label: string, raw: unknown): UsageWindow | undefined => {
    const s = obj(raw);
    if (s.unlimited === true) return undefined;
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
    windows: [win("Premium", snaps.premium_interactions), win("Chat", snaps.chat)].filter(
      (w): w is UsageWindow => !!w,
    ),
    ...(plan ? { plan } : {}),
  };
};

export const copilot: Fetcher = async (env) => {
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

export const openrouter: Fetcher = async (env) => {
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

export const deepseek: Fetcher = async (env) => {
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

const hanzoToken = async (env: UsageEnv): Promise<string | undefined> => {
  const direct = firstEnv(env, ["HANZO_API_KEY"]);
  if (direct) return direct;
  // The token Hanzo IAM issued to this machine; read from the identity file, used only for api.hanzo.ai.
  const ids = parseJson(await env.readText(`${env.home}/.hanzo/identities.json`));
  const accounts = Array.isArray(ids.accounts)
    ? ids.accounts
    : Object.values(obj(ids.accounts ?? ids));
  const active = str(ids.active) ?? str(ids.activeAccountId);
  const entries = accounts.map(obj);
  const pick = entries.find((e) => str(e.id) === active) ?? entries[0];
  return str(pick?.access_token);
};

export const parseHanzo = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "credits" | "plan"> => {
  const cents = (k: string, alt: string) => num(body[k]) ?? num(body[alt]);
  const used = cents("monthly_used_cents", "used_cents");
  const limit = cents("monthly_limit_cents", "limit_cents");
  const balance = cents("credit_balance_cents", "balance_cents");
  const plan = str(body.plan);
  const resetsAt = iso(body.period_end ?? body.resets_at);
  return {
    windows:
      used !== undefined && limit
        ? [
            {
              kind: "monthly",
              label: "Monthly",
              usedPercent: clamp((used / limit) * 100),
              ...(resetsAt ? { resetsAt } : {}),
            },
          ]
        : [],
    credits: {
      currency: "USD",
      ...(balance !== undefined ? { balance: balance / 100 } : {}),
      ...(used !== undefined ? { spent: used / 100 } : {}),
      ...(limit !== undefined ? { limit: limit / 100 } : {}),
    },
    ...(plan ? { plan } : {}),
  };
};

export const hanzo: Fetcher = async (env) => {
  const token = await hanzoToken(env);
  if (!token) throw new NotConfigured("Sign in to Hanzo.");
  const res = await env.fetch("https://api.hanzo.ai/v1/billing/summary", {
    headers: { Authorization: `Bearer ${token}`, Accept: "application/json" },
  });
  return { ...base(env, "hanzo", "oauth"), ...parseHanzo(await json(res)) };
};

export const FETCHERS: Readonly<Record<string, Fetcher>> = {
  claude,
  codex,
  copilot,
  openrouter,
  deepseek,
  hanzo,
};
