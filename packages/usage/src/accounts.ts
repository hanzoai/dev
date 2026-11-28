// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
/**
 * Readers for providers that can hold several accounts on one machine, or that have no host session
 * to ask (Claude and Codex logins, Hanzo identities, Antigravity, Cursor, z.ai). Each reads the
 * provider's own local login read-only and calls only that provider's own endpoint. A login is never
 * refreshed here: refresh tokens are single use and belong to the tool that issued them.
 *
 * Request shapes are ported from CodexBar (MIT, Copyright (c) 2026 Peter Steinberger); see NOTICE.
 */
import * as NodeCrypto from "node:crypto";

import type { UsageEnv } from "./env.ts";
import {
  base,
  clamp,
  firstEnv,
  firstEnvOrSettings,
  iso,
  NotConfigured,
  num,
  obj,
  parseDotEnv,
  parseJson,
  str,
  type Fetcher,
  type Instance,
} from "./fetchers.ts";
import type { ProviderSnapshot, UsageWindow } from "./schema.ts";

const SIGN_IN_AGAIN = "Sign in again.";

const failed = (
  env: UsageEnv,
  provider: string,
  identity: Identity,
  error: string,
): ProviderSnapshot => ({
  ...base(env, provider, "none"),
  ...identity,
  windows: [],
  error,
});

/** Who a reading is for. */
interface Identity {
  readonly instance: string;
  readonly label?: string;
  readonly account?: string;
  readonly unconfigured?: { readonly path: string };
}

const jwtClaims = (token: string): Record<string, unknown> => {
  const part = token.split(".")[1];
  if (!part) return {};
  try {
    return obj(JSON.parse(Buffer.from(part, "base64url").toString("utf8")));
  } catch {
    return {};
  }
};
const expired = (env: UsageEnv, token: string): boolean => {
  const exp = num(jwtClaims(token).exp);
  return exp !== undefined && exp * 1000 <= env.now().getTime() + 30_000;
};
const home = (env: UsageEnv, path: string) =>
  path.startsWith("~/") ? `${env.home}/${path.slice(2)}` : path;
const title = (s: string) => s.charAt(0).toUpperCase() + s.slice(1);

/**
 * The directories one provider keeps logins in: every configured instance, then any `<prefix>*`
 * directory beside the default that holds a login no instance uses. Those are returned as
 * unconfigured, never merged into an instance.
 */
const accountDirs = async (
  env: UsageEnv,
  instances: ReadonlyArray<Instance>,
  options: {
    readonly defaultDir: string;
    readonly prefix: string;
    readonly marker: (dir: string) => string;
    readonly hasAccount?: (dir: string) => Promise<boolean>;
  },
): Promise<ReadonlyArray<{ dir: string; instance?: Instance }>> => {
  const out: Array<{ dir: string; instance?: Instance }> = (
    instances.length ? instances : [{ id: "default", name: "Default" }]
  ).map((instance) => ({
    dir: instance.home ? home(env, instance.home) : options.defaultDir,
    instance,
  }));
  const known = new Set(out.map((o) => o.dir));
  const found = (await env.listDir(env.home))
    .filter((name) => name.startsWith(options.prefix))
    .map((name) => `${env.home}/${name}`)
    .toSorted();
  for (const dir of found) {
    if (known.has(dir)) continue;
    const present = options.hasAccount
      ? await options.hasAccount(dir)
      : (await env.readText(options.marker(dir))) !== undefined;
    if (!present) continue;
    out.push({ dir });
  }
  return out;
};
const identityOf = (
  dir: string,
  instance: Instance | undefined,
  account: string | undefined,
): Identity => ({
  instance: instance ? instance.id : `dir:${dir}`,
  ...(instance
    ? { label: instance.name }
    : { label: dir.split("/").pop() ?? dir, unconfigured: { path: dir } }),
  ...(account ? { account } : {}),
});

const claudeProfile = async (env: UsageEnv, dir: string) => {
  const file = dir === `${env.home}/.claude` ? `${env.home}/.claude.json` : `${dir}/.claude.json`;
  const oauth = obj(parseJson(await env.readText(file)).oauthAccount);
  const display = str(oauth.planDisplayName);
  const tier = str(oauth.organizationRateLimitTier) ?? str(oauth.userRateLimitTier);
  const billing = str(oauth.billingType);
  const tierPlan = tier
    ? (() => {
        const mult = /(\d+x)$/.exec(tier)?.[1];
        if (/max/i.test(tier)) return mult ? `Max ${mult}` : "Max";
        if (/pro/i.test(tier)) return mult ? `Pro ${mult}` : "Pro";
        if (/team/i.test(tier)) return "Team";
        if (/enterprise/i.test(tier)) return "Enterprise";
        if (/free/i.test(tier)) return "Free";
        return undefined;
      })()
    : undefined;
  const plan = display ?? tierPlan ?? (billing === "free" ? "Free" : undefined);
  const isFree =
    billing === "free" ||
    /^free$/i.test(plan ?? "") ||
    str(oauth.subscriptionType)?.toLowerCase() === "free";
  return { email: str(oauth.emailAddress), plan, isFree };
};

const claudeHasLocalUsage = async (env: UsageEnv, dir: string): Promise<boolean> => {
  if ((await env.readText(`${dir}/stats-cache.json`)) !== undefined) return true;
  if ((await env.readText(`${dir}/history.jsonl`)) !== undefined) return true;
  return (await env.listDir(`${dir}/projects`)).length > 0;
};

const claudeCredential = async (env: UsageEnv, dir: string): Promise<Record<string, unknown>> => {
  const service =
    dir === `${env.home}/.claude`
      ? "Claude Code-credentials"
      : `Claude Code-credentials-${NodeCrypto.createHash("sha256").update(dir).digest("hex").slice(0, 8)}`;
  const raw = (await env.keychain(service)) ?? (await env.readText(`${dir}/.credentials.json`));
  return obj(parseJson(raw).claudeAiOauth);
};

const claudePlan = (oauth: Record<string, unknown>): string | undefined => {
  const kind = str(oauth.subscriptionType);
  if (!kind) return undefined;
  const mult = /(\d+x)$/.exec(str(oauth.rateLimitTier) ?? "")?.[1];
  return mult ? `${title(kind)} ${mult}` : title(kind);
};

export const parseClaude = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "credits"> => {
  const windows: UsageWindow[] = [];
  const add = (key: string, kind: UsageWindow["kind"], label: string, minutes?: number) => {
    const w = obj(body[key]);
    const used = num(w.utilization);
    if (used === undefined) return;
    const resetsAt = iso(w.resets_at);
    windows.push({
      kind,
      label,
      usedPercent: clamp(used),
      ...(minutes ? { windowMinutes: minutes } : {}),
      ...(resetsAt ? { resetsAt } : {}),
    });
  };
  add("five_hour", "session", "Session", 300);
  add("seven_day", "weekly", "Weekly", 10080);
  add("seven_day_opus", "weekly", "Weekly · Opus", 10080);
  add("seven_day_sonnet", "weekly", "Weekly · Sonnet", 10080);
  for (const raw of Array.isArray(body.limits) ? body.limits : []) {
    const l = obj(raw);
    const model = str(obj(obj(l.scope).model).display_name);
    const percent = num(l.percent);
    if (l.kind !== "weekly_scoped" || !model || percent === undefined) continue;
    const label = `Weekly · ${model}`;
    if (windows.some((w) => w.label === label)) continue;
    const resetsAt = iso(l.resets_at);
    windows.push({
      kind: "weekly",
      label,
      usedPercent: clamp(percent),
      windowMinutes: 10080,
      ...(resetsAt ? { resetsAt } : {}),
    });
  }
  const extra = obj(body.extra_usage);
  const places = num(extra.decimal_places) ?? 2;
  const scale = 10 ** places;
  const spent = num(extra.used_credits);
  const limit = num(extra.monthly_limit);
  return {
    windows,
    ...(extra.is_enabled === true
      ? {
          credits: {
            currency: str(extra.currency) ?? "USD",
            ...(spent !== undefined ? { spent: spent / scale } : {}),
            ...(limit !== undefined ? { limit: limit / scale } : {}),
          },
        }
      : {}),
  };
};

export const claude: Fetcher = async (env, instances) =>
  Promise.all(
    (
      await accountDirs(env, instances, {
        defaultDir: env.env.CLAUDE_CONFIG_DIR?.trim() || `${env.home}/.claude`,
        prefix: ".claude",
        marker: (dir) => `${dir}/.claude.json`,
        hasAccount: async (dir) =>
          (await env.readText(`${dir}/.claude.json`)) !== undefined ||
          (await env.readText(`${dir}/.credentials.json`)) !== undefined ||
          (await claudeHasLocalUsage(env, dir)),
      })
    ).map(async ({ dir, instance }): Promise<ProviderSnapshot> => {
      const profile = await claudeProfile(env, dir);
      const who = identityOf(dir, instance, profile.email);
      const oauth = await claudeCredential(env, dir);
      const plan = claudePlan(oauth) ?? profile.plan;
      const isFree =
        profile.isFree || str(oauth.subscriptionType)?.toLowerCase() === "free";
      const localFallback = async (error?: string): Promise<ProviderSnapshot> => {
        const hasLocal = await claudeHasLocalUsage(env, dir);
        if (hasLocal && isFree) {
          return {
            ...base(env, "claude", "local"),
            ...who,
            ...(plan ? { plan } : {}),
            windows: [],
          };
        }
        return {
          ...base(env, "claude", hasLocal ? "local" : "none"),
          ...who,
          ...(plan ? { plan } : {}),
          windows: [],
          ...(error ? { error } : {}),
        };
      };
      const token = str(oauth.accessToken);
      if (!token) {
        if (isFree && (await claudeHasLocalUsage(env, dir))) {
          return localFallback();
        }
        return localFallback("Not signed in. Run `claude` for this account.");
      }
      const expiresAt = num(oauth.expiresAt);
      if (expiresAt !== undefined && expiresAt <= env.now().getTime()) {
        return localFallback(SIGN_IN_AGAIN);
      }
      const res = await env.fetch("https://api.anthropic.com/api/oauth/usage", {
        headers: {
          Authorization: `Bearer ${token}`,
          "anthropic-beta": "oauth-2025-04-20",
          Accept: "application/json",
        },
      });
      if (res.status === 401 || res.status === 403) {
        return localFallback(isFree ? undefined : SIGN_IN_AGAIN);
      }
      if (res.status === 429) {
        return localFallback("Rate limited by Anthropic; will retry.");
      }
      if (!res.ok) return localFallback(`HTTP ${res.status}`);
      const parsed = parseClaude(obj(await res.json()));
      const source =
        parsed.windows.length > 0 || parsed.credits !== undefined
          ? "oauth"
          : (await claudeHasLocalUsage(env, dir))
            ? "local"
            : "oauth";
      return {
        ...base(env, "claude", source),
        ...who,
        ...(plan ? { plan } : {}),
        ...parsed,
      };
    }),
  );

export const parseCodex = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "plan" | "account" | "credits"> => {
  const limits = obj(body.rate_limit);
  const windows: UsageWindow[] = [];
  for (const [key, kind, label] of [
    ["primary_window", "session", "Session"],
    ["secondary_window", "weekly", "Weekly"],
  ] as const) {
    const w = obj(limits[key]);
    const used = num(w.used_percent);
    if (used === undefined) continue;
    const seconds = num(w.limit_window_seconds);
    const resetsAt = iso(w.reset_at, true);
    windows.push({
      kind,
      label,
      usedPercent: clamp(used),
      ...(seconds ? { windowMinutes: Math.round(seconds / 60) } : {}),
      ...(resetsAt ? { resetsAt } : {}),
    });
  }
  const credits = obj(body.credits);
  const balance = num(credits.balance);
  const plan = str(body.plan_type);
  const account = str(body.email);
  return {
    windows,
    ...(plan ? { plan: title(plan) } : {}),
    ...(account ? { account } : {}),
    ...(credits.has_credits === true && balance !== undefined
      ? { credits: { currency: "credits", balance } }
      : {}),
  };
};

export const codex: Fetcher = async (env, instances) =>
  Promise.all(
    (
      await accountDirs(env, instances, {
        defaultDir: env.env.CODEX_HOME?.trim() || `${env.home}/.codex`,
        prefix: ".codex",
        marker: (dir) => `${dir}/auth.json`,
      })
    ).map(async ({ dir, instance }): Promise<ProviderSnapshot> => {
      const auth = parseJson(await env.readText(`${dir}/auth.json`));
      const tokens = obj(auth.tokens);
      const token = str(tokens.access_token);
      const idEmail = str(jwtClaims(str(tokens.id_token) ?? "").email);
      const who = identityOf(dir, instance, idEmail);
      if (!token)
        return failed(env, "codex", who, "Not signed in. Run `codex login` for this account.");
      if (expired(env, token)) return failed(env, "codex", who, SIGN_IN_AGAIN);
      const res = await env.fetch("https://chatgpt.com/backend-api/wham/usage", {
        headers: {
          Authorization: `Bearer ${token}`,
          Accept: "application/json",
          ...(str(tokens.account_id) ? { "ChatGPT-Account-Id": str(tokens.account_id)! } : {}),
        },
      });
      if (res.status === 401 || res.status === 403) return failed(env, "codex", who, SIGN_IN_AGAIN);
      if (!res.ok) return failed(env, "codex", who, `HTTP ${res.status}`);
      const parsed = parseCodex(obj(await res.json()));
      return { ...base(env, "codex", "oauth"), ...who, ...parsed };
    }),
  );

/** commerce `GET /v1/billing/balance`: amounts in cents, available = balance - holds. */
export const parseHanzo = (
  body: Record<string, unknown>,
  tier: Record<string, unknown> = {},
): Pick<ProviderSnapshot, "windows" | "credits" | "plan"> => {
  const available = num(body.available) ?? num(body.balance);
  const holds = num(body.holds);
  const name = str(obj(tier.tier).displayName);
  const planName = str(tier.planName);
  const plan = [name, planName && planName !== name ? planName : undefined]
    .filter(Boolean)
    .join(" · ");
  return {
    windows: [],
    ...(plan ? { plan } : {}),
    credits: {
      currency: (str(body.currency) ?? "usd").toUpperCase(),
      ...(available !== undefined ? { balance: available / 100 } : {}),
      ...(holds ? { spent: holds / 100 } : {}),
    },
  };
};

/** The CLI keeps `{ "hanzo/<owner>/<name>": <TokenSet> }` and names the active identity in config.toml. */
const hanzoIdentities = async (
  env: UsageEnv,
): Promise<ReadonlyArray<{ key: string; token: string; active: boolean }>> => {
  const dir =
    env.env.HANZO_CONFIG_DIR ??
    (env.platform === "darwin"
      ? `${env.home}/Library/Application Support/hanzo`
      : `${env.env.XDG_DATA_HOME ?? `${env.home}/.local/share`}/hanzo`);
  const store = parseJson(await env.readText(`${dir}/credentials`));
  const toml = (await env.readText(`${dir}/config.toml`)) ?? "";
  const active = /\[auth\.active\]\s*\n\s*hanzo\s*=\s*"([^"]+)"/.exec(toml)?.[1];
  return Object.entries(store)
    .flatMap(([key, value]) => {
      const set = typeof value === "string" ? parseJson(value) : obj(value);
      const token = str(set.access_token);
      return token ? [{ key, token, active: key === `hanzo/${active}` }] : [];
    })
    .toSorted((a, b) => Number(b.active) - Number(a.active) || a.key.localeCompare(b.key));
};

/** The issuer Hanzo's own discovery document names; a token is sent to api.hanzo.ai only when it was issued by it. */
const hanzoIssuer = async (env: UsageEnv): Promise<string | undefined> => {
  const res = await env.fetch("https://hanzo.ai/.well-known/openid-configuration", {
    headers: { Accept: "application/json" },
  });
  return res.ok ? str(obj(await res.json()).issuer) : undefined;
};

export const hanzo: Fetcher = async (env) => {
  const key = firstEnv(env, ["HANZO_API_KEY"]);
  const [identities, hanzodToken] = await Promise.all([
    hanzoIdentities(env),
    env.readHanzoToken ? env.readHanzoToken().catch(() => undefined) : Promise.resolve(undefined),
  ]);
  if (!key && !hanzodToken && identities.length === 0) throw new NotConfigured("Sign in to Hanzo.");
  const issuer =
    identities.length > 0 || hanzodToken
      ? await hanzoIssuer(env).catch(() => undefined)
      : undefined;
  const read = async (
    who: Identity,
    token: string,
    source: ProviderSnapshot["source"],
  ): Promise<ProviderSnapshot> => {
    const headers = { Authorization: `Bearer ${token}`, Accept: "application/json" };
    const get = (path: string) => env.fetch(`https://api.hanzo.ai${path}`, { headers });
    const [balance, tier] = await Promise.all([
      get("/v1/billing/balance"),
      get("/v1/billing/tier"),
    ]);
    if (balance.status === 401 || balance.status === 403)
      return failed(env, "hanzo", who, SIGN_IN_AGAIN);
    if (!balance.ok) return failed(env, "hanzo", who, `HTTP ${balance.status}`);
    return {
      ...base(env, "hanzo", source),
      ...who,
      ...parseHanzo(obj(await balance.json()), tier.ok ? obj(await tier.json()) : {}),
    };
  };
  const out: ProviderSnapshot[] = [];
  if (key) out.push(await read({ instance: "api-key", label: "API key" }, key, "api"));
  let usedHanzodToken = false;
  for (const id of identities) {
    const token = id.active && hanzodToken ? hanzodToken : id.token;
    if (id.active && hanzodToken) usedHanzodToken = true;
    const claims = jwtClaims(token);
    const who: Identity = {
      instance: id.key,
      label: `${id.key.split("/").slice(1).join("/")}${id.active ? " · active" : ""}`,
      ...(str(claims.email) ? { account: str(claims.email)! } : {}),
    };
    if (expired(env, token)) out.push(failed(env, "hanzo", who, SIGN_IN_AGAIN));
    else if (issuer && claims.iss && claims.iss !== issuer)
      out.push(failed(env, "hanzo", who, "This sign-in was not issued by Hanzo; sign in again."));
    else out.push(await read(who, token, "oauth"));
  }
  if (hanzodToken && !usedHanzodToken) {
    const claims = jwtClaims(hanzodToken);
    const owner = str(claims.owner);
    const name = str(claims.name);
    const slug = owner && name ? `${owner}/${name}` : "hanzod";
    const who: Identity = {
      instance: `hanzo/${slug}`,
      label: `${slug} · active`,
      ...(str(claims.email) ? { account: str(claims.email)! } : {}),
    };
    if (expired(env, hanzodToken)) out.push(failed(env, "hanzo", who, SIGN_IN_AGAIN));
    else if (issuer && claims.iss && claims.iss !== issuer)
      out.push(failed(env, "hanzo", who, "This sign-in was not issued by Hanzo; sign in again."));
    else out.push(await read(who, hanzodToken, "oauth"));
  }
  return out;
};

export const parseAntigravity = (stdout: string): Pick<ProviderSnapshot, "windows"> => {
  const groups = obj(obj(obj(parseJson(stdout).command).data)).groups;
  const windows: UsageWindow[] = [];
  for (const g of Array.isArray(groups) ? groups : []) {
    const name = str(obj(g).name) ?? "Models";
    for (const b of Array.isArray(obj(g).buckets) ? (obj(g).buckets as unknown[]) : []) {
      const bucket = obj(b);
      const remaining = num(bucket.remaining_fraction);
      if (bucket.disabled === true || remaining === undefined) continue;
      const five = bucket.window === "5h";
      const rawReset = iso(bucket.reset_time);
      // Untouched Antigravity buckets report Unix epoch 0 (1970-01-01T00:00:00.000Z); omit it.
      const resetsAt = rawReset && Date.parse(rawReset) > 86_400_000 ? rawReset : undefined;
      windows.push({
        kind: five ? "session" : "weekly",
        label: `${name} · ${five ? "5-hour" : "Weekly"}`,
        usedPercent: clamp((1 - remaining) * 100),
        windowMinutes: five ? 300 : 10080,
        ...(resetsAt ? { resetsAt } : {}),
      });
    }
  }
  return { windows };
};

/**
 * `agy -p /usage` is Antigravity's own usage report. When a legacy standalone token file exists it
 * runs in a scratch HOME holding a copy of the token so the real file is never rewritten; on newer
 * installs whose sign-in lives in OS keychain / `~/.gemini/antigravity-cli`, it runs against the
 * local state with browser-open stubs so it never launches a browser window.
 */
export const antigravity: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const who: Identity = {
    instance: instance?.id ?? "antigravity",
    ...(instance ? { label: instance.name } : {}),
  };
  let tokenText: string | undefined;
  for (const file of [
    `${env.home}/.gemini/antigravity-cli/antigravity-oauth-token`,
    `${env.home}/.gemini/jetski-standalone-oauth-token`,
  ]) {
    tokenText = await env.readText(file);
    if (tokenText) break;
  }
  const hasLocalState =
    Boolean(tokenText) ||
    instances.length > 0 ||
    (await env.listDir(`${env.home}/.gemini/antigravity-cli`)).length > 0 ||
    (await env.listDir(`${env.home}/.gemini/antigravity`)).length > 0;
  if (!hasLocalState) throw new NotConfigured("Sign in to Antigravity (run `agy`).");
  const email = tokenText ? str(parseJson(tokenText).id_token_email) : undefined;
  const withEmail = email ? { ...who, account: email } : who;
  const scratchFiles: Record<string, string> = {
    "bin/open": "#!/bin/sh\nexit 1\n",
    "bin/xdg-open": "#!/bin/sh\nexit 1\n",
    ...(tokenText ? { ".gemini/antigravity-cli/antigravity-oauth-token": tokenText } : {}),
  };
  const stdout = await env.withScratch(scratchFiles, async (dir) => {
    // agy shells out to `open` to start a browser sign-in; the stubs keep it from ever opening one.
    const runHome = tokenText ? dir : env.home;
    return env.exec(
      instance?.binary || "agy",
      ["-p", "/usage", "--output-format", "json", "--print-timeout", "60s"],
      {
        env: {
          HOME: runHome,
          PWD: dir,
          SSH_TTY: "hanzo-usage",
          BROWSER: `${dir}/bin/open`,
          PATH: `${dir}/bin:${env.home}/.local/bin:/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin`,
        },
        cwd: dir,
        timeoutMs: 90_000,
      },
    );
  });
  if (!stdout)
    return [
      failed(
        env,
        "antigravity",
        withEmail,
        "Antigravity CLI (agy) did not answer; is it installed and signed in?",
      ),
    ];
  const parsed = parseAntigravity(stdout);
  if (parsed.windows.length === 0)
    return [
      failed(
        env,
        "antigravity",
        withEmail,
        "Antigravity reported no usage limits for this account.",
      ),
    ];
  return [{ ...base(env, "antigravity", "local"), ...withEmail, ...parsed }];
};

export const parseCursor = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "plan"> => {
  const plan = obj(obj(body.individualUsage).plan);
  const resetsAt = iso(body.billingCycleEnd);
  const windows: UsageWindow[] = [];
  for (const [key, label] of [
    ["totalPercentUsed", "Included"],
    ["autoPercentUsed", "Auto"],
    ["apiPercentUsed", "API"],
  ] as const) {
    const used = num(plan[key]);
    if (used === undefined) continue;
    windows.push({
      kind: "monthly",
      label,
      usedPercent: clamp(used),
      ...(resetsAt ? { resetsAt } : {}),
    });
  }
  const membership = str(body.membershipType);
  return { windows, ...(membership ? { plan: title(membership) } : {}) };
};

export const cursor: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const db =
    env.platform === "darwin"
      ? `${env.home}/Library/Application Support/Cursor/User/globalStorage/state.vscdb`
      : `${env.env.XDG_CONFIG_HOME ?? `${env.home}/.config`}/Cursor/User/globalStorage/state.vscdb`;
  const token = await env.sqliteValue(
    db,
    "SELECT value FROM ItemTable WHERE key = ?",
    "cursorAuth/accessToken",
  );
  const cliConfig = parseJson(await env.readText(`${env.home}/.cursor/cli-config.json`));
  const cliEmail = str(obj(cliConfig.authInfo).email);
  if (!token && !cliEmail) throw new NotConfigured("Sign in to Cursor.");
  const out: ProviderSnapshot[] = [];
  let desktopEmail: string | undefined;
  if (token) {
    desktopEmail = await env.sqliteValue(
      db,
      "SELECT value FROM ItemTable WHERE key = ?",
      "cursorAuth/cachedEmail",
    );
    const who: Identity = {
      instance: instance?.id ?? "cursor",
      ...(instance ? { label: instance.name } : {}),
      ...(desktopEmail ? { account: desktopEmail } : {}),
    };
    const user = str(jwtClaims(token).sub)?.split("|").pop();
    if (!user || !/^[\w.-]+$/.test(user) || expired(env, token)) {
      out.push(failed(env, "cursor", who, SIGN_IN_AGAIN));
    } else {
      const res = await env.fetch("https://cursor.com/api/usage-summary", {
        headers: {
          Cookie: `WorkosCursorSessionToken=${user}%3A%3A${token}`,
          Accept: "application/json",
        },
      });
      if (res.status === 401 || res.status === 403) out.push(failed(env, "cursor", who, SIGN_IN_AGAIN));
      else if (!res.ok) out.push(failed(env, "cursor", who, `HTTP ${res.status}`));
      else out.push({ ...base(env, "cursor", "oauth"), ...who, ...parseCursor(obj(await res.json())) });
    }
  }
  if (cliEmail && cliEmail !== desktopEmail) {
    out.push({
      ...base(env, "cursor", "local"),
      instance: "cursor-cli",
      label: "Cursor CLI",
      account: cliEmail,
      windows: [],
    });
  }
  return out;
};

const ZAI_REGIONS = [
  {
    host: "https://api.z.ai",
    keys: ["Z_AI_API_KEY", "ZAI_API_KEY", "ZAI_OPENAI_API_KEY"],
    label: "z.ai",
  },
  {
    host: "https://open.bigmodel.cn",
    keys: ["BIGMODEL_API_KEY", "ZHIPU_API_KEY", "ZHIPUAI_API_KEY", "GLM_API_KEY"],
    label: "BigModel",
  },
] as const;

const UNIT_MINUTES: Readonly<Record<number, number>> = { 1: 1440, 3: 60, 5: 1, 6: 10080 };

export const parseZai = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "plan"> => {
  const data = obj(body.data);
  const windows: UsageWindow[] = [];
  for (const raw of Array.isArray(data.limits) ? data.limits : []) {
    const l = obj(raw);
    const type = str(l.type);
    if (type !== "TOKENS_LIMIT" && type !== "TIME_LIMIT" && type !== "CREDIT_LIMIT") continue;
    const total = num(l.usage);
    const remaining = num(l.remaining);
    const current = num(l.currentValue);
    const used =
      total && total > 0 && remaining !== undefined
        ? clamp(((total - remaining) / total) * 100)
        : total && total > 0 && current !== undefined
          ? clamp((current / total) * 100)
          : clamp(num(l.percentage) ?? 0);
    const minutes = (num(l.number) ?? 0) * (UNIT_MINUTES[num(l.unit) ?? 0] ?? 0);
    const reset = num(l.nextResetTime);
    const monthly = type === "TIME_LIMIT";
    const kind: UsageWindow["kind"] = monthly
      ? "monthly"
      : minutes === 300
        ? "session"
        : minutes === 10080
          ? "weekly"
          : "other";
    windows.push({
      kind,
      label: monthly
        ? "MCP"
        : type === "CREDIT_LIMIT"
          ? "Credits"
          : minutes === 300
            ? "5-hour"
            : "Tokens",
      usedPercent: used,
      ...(monthly ? { windowMinutes: 43200 } : minutes > 0 ? { windowMinutes: minutes } : {}),
      ...(reset ? { resetsAt: new Date(reset).toISOString() } : {}),
    });
  }
  const plan = [data.planName, data.plan, data.packageName, data.level].map(str).find(Boolean);
  return {
    windows: windows.toSorted((a, b) => (a.windowMinutes ?? 1e9) - (b.windowMinutes ?? 1e9)),
    ...(plan ? { plan } : {}),
  };
};

export const zai: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const zaiConfigFiles = [
    `${env.home}/.zai/config.json`,
    `${env.home}/.config/zai/config.json`,
    `${env.home}/.config/z-ai/config.json`,
  ];
  const zaiEnvFiles = [`${env.home}/.zai/.env`, `${env.home}/.config/zai/.env`];
  const regions = (
    await Promise.all(
      ZAI_REGIONS.map(async (r) => {
        const key = await firstEnvOrSettings(env, r.keys, zaiConfigFiles, zaiEnvFiles);
        return key ? { ...r, key } : undefined;
      }),
    )
  ).filter((r): r is (typeof ZAI_REGIONS)[number] & { readonly key: string } => Boolean(r));
  if (regions.length === 0) {
    const localFiles = await env.listDir(`${env.home}/.zai`);
    if (localFiles.length > 0) {
      return [
        {
          ...base(env, "zai", "local"),
          instance: instance?.id ?? "zai-local",
          label: instance?.name ?? "z.ai",
          plan: "Local MCP",
          windows: [],
          error: "Set Z_AI_API_KEY to read live GLM coding-plan limits.",
        },
      ];
    }
    throw new NotConfigured("Set Z_AI_API_KEY to read your GLM coding plan.");
  }
  return Promise.all(
    regions.map(async (r): Promise<ProviderSnapshot> => {
      const who: Identity = {
        instance: instance?.id ?? `zai-${r.label}`,
        label: instance?.name ?? r.label,
      };
      const res = await env.fetch(`${r.host}/api/monitor/usage/quota/limit`, {
        headers: { Authorization: `Bearer ${r.key}`, Accept: "application/json" },
      });
      if (!res.ok) return failed(env, "zai", who, `HTTP ${res.status}`);
      const body = obj(await res.json());
      // The API answers HTTP 200 and reports a bad key in the body.
      if (body.code === 401)
        return failed(
          env,
          "zai",
          who,
          `${r.label} rejected this API key (expired or incorrect). Set a current one.`,
        );
      if (body.success !== true)
        return failed(env, "zai", who, `${r.label}: ${str(body.msg) ?? "unexpected response"}`);
      return { ...base(env, "zai", "api"), ...who, ...parseZai(body) };
    }),
  );
};

interface MuseLocalInfo {
  readonly token?: string;
  readonly account?: string;
  readonly plan?: string;
  readonly hasProfile: boolean;
}

/** Muse's local config without touching the keychain: checks env, top-level, providers.meta and providers.muse. */
const museLocalInfo = async (env: UsageEnv): Promise<MuseLocalInfo> => {
  const direct = firstEnv(env, ["MUSE_DEVICE_TOKEN"]);
  const file = parseJson(await env.readText(`${env.home}/.config/muse/auth.json`));
  const providers = obj(file.providers);
  const meta = obj(providers.meta);
  const museEntry = obj(providers.muse);
  const token =
    direct ?? str(file.access_token) ?? str(meta.access_token) ?? str(museEntry.access_token);
  const account = str(meta.user_email) ?? str(museEntry.user_email) ?? str(file.user_email);
  const subsActive = meta.subs_active === true || museEntry.subs_active === true;
  const plan =
    str(meta.subs_tier_name) ??
    str(museEntry.subs_tier_name) ??
    (subsActive ? "Muse Code" : undefined);
  const hasProfile = Boolean(account || subsActive || str(meta.storage) || str(museEntry.storage));
  return {
    ...(token ? { token } : {}),
    ...(account ? { account } : {}),
    ...(plan ? { plan } : {}),
    hasProfile,
  };
};

export const parseMuse = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "plan" | "account"> => {
  const usage = obj(body.subs_usage);
  const windows: UsageWindow[] = [];
  for (const [key, kind, label, minutes] of [
    ["window", "session", "Session", undefined],
    ["weekly", "weekly", "Weekly", 10080],
  ] as const) {
    const w = obj(usage[key]);
    const used = num(w.used_percent);
    if (used === undefined) continue;
    const mins = minutes ?? num(w.window_duration_mins);
    const resetsAt = iso(w.resets_at, true);
    windows.push({
      kind,
      label,
      usedPercent: clamp(used),
      ...(mins ? { windowMinutes: mins } : {}),
      ...(resetsAt ? { resetsAt } : {}),
    });
  }
  const plan = str(body.subs_tier_name);
  const account = str(body.user_email);
  return { windows, ...(plan ? { plan } : {}), ...(account ? { account } : {}) };
};

const readMuse = async (
  env: UsageEnv,
  instances: ReadonlyArray<Instance>,
  token: string,
): Promise<ReadonlyArray<ProviderSnapshot>> => {
  const instance = instances[0];
  const who: Identity = {
    instance: instance?.id ?? "muse",
    ...(instance ? { label: instance.name } : {}),
  };
  const res = await env.fetch("https://api.meta.ai/muse-code/key", {
    method: "POST",
    body: "{}",
    headers: {
      Authorization: `Bearer ${token}`,
      "Content-Type": "application/json",
      "x-api-version": "1.0.0",
    },
  });
  if (res.status === 401 || res.status === 403)
    return [failed(env, "muse", who, "Muse Code login was rejected. Run `muse login` again.")];
  if (!res.ok) return [failed(env, "muse", who, `HTTP ${res.status}`)];
  const body = obj(await res.json());
  if (body.is_subs_active !== true)
    return [failed(env, "muse", who, "No Muse Code subscription is active on this login.")];
  const parsed = parseMuse(body);
  return [{ ...base(env, "muse", "oauth"), ...who, ...parsed }];
};

/** The timer's reader: never the keychain, which asks the user for a password every time. */
export const muse: Fetcher = async (env, instances) => {
  const info = await museLocalInfo(env);
  if (info.token?.startsWith("dca:")) return readMuse(env, instances, info.token);
  if (info.hasProfile) {
    const instance = instances[0];
    return [
      {
        ...base(env, "muse", "local"),
        instance: instance?.id ?? "muse",
        ...(instance ? { label: instance.name } : {}),
        ...(info.account ? { account: info.account } : {}),
        ...(info.plan ? { plan: info.plan } : {}),
        windows: [],
        error: "Connect Muse Code to read live limits.",
      },
    ];
  }
  throw new NotConfigured("Connect Muse Code to read its usage.");
};

/**
 * The user's "Connect Muse Code": the one read of Muse's keychain item. Only the reading it yields
 * is kept (never the login), so no later refresh asks again.
 */
export const connectMuse: Fetcher = async (env, instances) => {
  const info = await museLocalInfo(env);
  const token =
    info.token ?? str(parseJson(await env.keychain("ai.meta.dev.credentials")).access_token);
  if (!token?.startsWith("dca:"))
    throw new NotConfigured("Sign in to Muse Code (run `muse login`).");
  return readMuse(env, instances, token);
};

export const grok: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const auth = parseJson(await env.readText(`${env.home}/.grok/auth.json`));
  const entries = Object.entries(auth)
    .map(([key, raw]) => ({ key, data: obj(raw) }))
    .filter(({ data }) => Boolean(str(data.email) || str(data.user_id) || str(data.key)));
  if (entries.length > 0) {
    return entries.map(({ key, data }, idx): ProviderSnapshot => {
      const email = str(data.email);
      const team = str(data.coding_data_team);
      const expiresAt = str(data.expires_at);
      const isExpired = expiresAt ? Date.parse(expiresAt) <= env.now().getTime() : false;
      const who: Identity = {
        instance: idx === 0 && instance ? instance.id : `grok:${str(data.user_id) ?? key}`,
        ...(idx === 0 && instance ? { label: instance.name } : {}),
        ...(email ? { account: email } : {}),
      };
      return {
        ...base(env, "grok", "local"),
        ...who,
        ...(team ? { plan: title(team) } : {}),
        windows: [],
        ...(isExpired ? { error: SIGN_IN_AGAIN } : {}),
      };
    });
  }
  const apiKey = await firstEnvOrSettings(env, ["XAI_API_KEY", "XAI_MANAGEMENT_API_KEY", "GROK_API_KEY"]);
  if (apiKey) {
    return [
      {
        ...base(env, "grok", "local"),
        instance: instance?.id ?? "grok-api",
        label: instance?.name ?? "API key",
        windows: [],
      },
    ];
  }
  throw new NotConfigured("Sign in to Grok CLI or set XAI_API_KEY.");
};

export const parseOpenCodeGo = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "plan"> => {
  const usage = obj(body.usage);
  const windows: UsageWindow[] = [];
  for (const [key, kind, label, minutes] of [
    ["rolling", "session", "Rolling", 300],
    ["weekly", "weekly", "Weekly", 10080],
    ["monthly", "monthly", "Monthly", 43200],
  ] as const) {
    const w = obj(usage[key]);
    const used = num(w.percent);
    if (used === undefined) continue;
    const resetsAt = iso(w.resetsAt);
    windows.push({
      kind,
      label,
      usedPercent: clamp(used),
      windowMinutes: minutes,
      ...(resetsAt ? { resetsAt } : {}),
    });
  }
  return { windows, plan: "Go" };
};

export const opencode: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const dataHome = env.env.XDG_DATA_HOME || `${env.home}/.local/share`;
  const dbPath = `${dataHome}/opencode/${env.env.OPENCODE_DB ?? "opencode.db"}`;
  const authFile = parseJson(await env.readText(`${dataHome}/opencode/auth.json`));
  const goAuth = obj(authFile["opencode-go"]);
  const fileKey = str(goAuth.type) === "api" ? str(goAuth.key) : undefined;
  const envKey = firstEnv(env, ["OPENCODE_API_KEY"]);

  const dbRow =
    (await env.sqliteValue(
      dbPath,
      "SELECT value FROM credential WHERE integration_id = ? AND active IS NOT 0 ORDER BY active DESC, time_created DESC, id DESC LIMIT 1",
      "opencode",
    )) ??
    (await env.sqliteValue(
      dbPath,
      "SELECT value FROM credential WHERE integration_id = ? AND active IS NOT 0 ORDER BY active DESC, time_created DESC, id DESC LIMIT 1",
      "opencode-go",
    ));
  const stored = parseJson(dbRow);
  const storedType = str(stored.type);
  const storedMeta = obj(stored.metadata);
  const storedKey = storedType === "key" ? str(stored.key) : undefined;
  const apiKey = storedKey ?? fileKey ?? envKey;

  const who: Identity = {
    instance: instance?.id ?? "opencode",
    ...(instance ? { label: instance.name } : {}),
    ...(str(storedMeta.email) ? { account: str(storedMeta.email)! } : {}),
  };

  if (apiKey) {
    const res = await env.fetch("https://opencode.ai/zen/go/v1/usage", {
      headers: { Authorization: `Bearer ${apiKey}`, Accept: "application/json" },
    });
    if (res.ok) {
      return [{ ...base(env, "opencode", "api"), ...who, ...parseOpenCodeGo(obj(await res.json())) }];
    }
    if (res.status === 401) return [failed(env, "opencode", who, SIGN_IN_AGAIN)];
    // HTTP 403 means a valid Zen key without a Go subscription; surface local presence cleanly.
    return [{ ...base(env, "opencode", "local"), ...who, plan: "Zen", windows: [] }];
  }

  if (storedType === "oauth" && (str(stored.access) || str(storedMeta.email))) {
    const expires = num(stored.expires);
    const isExpired = expires !== undefined && expires <= env.now().getTime();
    const orgName = str(storedMeta.orgName);
    return [
      {
        ...base(env, "opencode", "local"),
        ...who,
        ...(orgName ? { plan: orgName === "Default" ? "Console" : orgName } : {}),
        windows: [],
        ...(isExpired ? { error: SIGN_IN_AGAIN } : {}),
      },
    ];
  }

  throw new NotConfigured("Sign in to OpenCode or set OPENCODE_API_KEY.");
};

export const qwencloud: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const settings = parseJson(await env.readText(`${env.home}/.qwen/settings.json`));
  const settingsEnv = obj(settings.env);
  const oauth = parseJson(await env.readText(`${env.home}/.qwen/oauth_creds.json`));
  const accounts = parseJson(await env.readText(`${env.home}/.qwen/google_accounts.json`));
  const key =
    firstEnv(env, ["DASHSCOPE_API_KEY", "ALIBABA_CODING_PLAN_API_KEY", "ALIBABA_QWEN_API_KEY"]) ??
    str(settingsEnv.DASHSCOPE_API_KEY) ??
    str(settingsEnv.ALIBABA_CODING_PLAN_API_KEY) ??
    str(settingsEnv.ALIBABA_QWEN_API_KEY);
  const hasOauth = Boolean(str(oauth.access_token) || str(oauth.refresh_token));
  if (!key && !hasOauth) throw new NotConfigured("Sign in to Qwen CLI or set DASHSCOPE_API_KEY.");
  const model = str(obj(settings.model).name);
  const account = str(accounts.active);
  const plan = key ? (model ? `DashScope · ${model}` : "DashScope") : "Qwen OAuth";
  return [
    {
      ...base(env, "qwencloud", "local"),
      instance: instance?.id ?? "qwencloud",
      ...(instance ? { label: instance.name } : {}),
      ...(account ? { account } : {}),
      plan,
      windows: [],
    },
  ];
};

export const gemini: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const accounts = parseJson(await env.readText(`${env.home}/.gemini/google_accounts.json`));
  const active = str(accounts.active);
  const oldList = Array.isArray(accounts.old)
    ? accounts.old.filter((v): v is string => typeof v === "string" && v.length > 0)
    : [];
  const oauth = parseJson(await env.readText(`${env.home}/.gemini/oauth_creds.json`));
  const apiKey = await firstEnvOrSettings(env, ["GEMINI_API_KEY", "GOOGLE_API_KEY"]);
  const hasHistory =
    (await env.listDir(`${env.home}/.gemini/tmp`)).length > 0 ||
    (await env.listDir(`${env.home}/.gemini/history`)).length > 0;
  const account = active ?? (hasHistory ? oldList[0] : undefined);
  if (!account && !str(oauth.access_token) && !apiKey) {
    throw new NotConfigured("Sign in to Gemini CLI or set GEMINI_API_KEY.");
  }
  return [
    {
      ...base(env, "gemini", "local"),
      instance: instance?.id ?? "gemini",
      ...(instance ? { label: instance.name } : {}),
      ...(account ? { account } : {}),
      plan: apiKey ? "API key" : active ? "Gemini CLI" : "Gemini CLI (local)",
      windows: [],
    },
  ];
};

export const mistral: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const vibeEnv = parseDotEnv(await env.readText(`${env.home}/.vibe/.env`));
  const key = firstEnv(env, ["MISTRAL_API_KEY"]) ?? vibeEnv.MISTRAL_API_KEY;
  const configToml = (await env.readText(`${env.home}/.vibe/config.toml`)) ?? "";
  const activeModel = /^\s*active_model\s*=\s*"([^"]+)"/m.exec(configToml)?.[1];
  const sessionFiles = await env.listFiles(`${env.home}/.vibe/logs/session`, ".json");
  if (!key && sessionFiles.length === 0) {
    throw new NotConfigured("Set MISTRAL_API_KEY or run Mistral Vibe.");
  }
  let spentUsd = 0;
  let hasCost = false;
  for (const file of sessionFiles.slice(-100)) {
    const parsed = parseJson(await env.readText(file));
    const cost = num(obj(obj(parsed.metadata).stats).session_cost);
    if (cost !== undefined && cost >= 0) {
      spentUsd += cost;
      hasCost = true;
    }
  }
  return [
    {
      ...base(env, "mistral", "local"),
      instance: instance?.id ?? "mistral",
      ...(instance ? { label: instance.name } : {}),
      plan: activeModel ? `Vibe · ${activeModel}` : "Mistral",
      windows: [],
      ...(hasCost ? { credits: { currency: "USD", spent: Math.round(spentUsd * 100) / 100 } } : {}),
    },
  ];
};

export const parseWindsurf = (
  body: Record<string, unknown>,
): Pick<ProviderSnapshot, "windows" | "plan"> => {
  const usage = obj(body.usage);
  const resetsAt = iso(body.endTimestamp);
  const windows: UsageWindow[] = [];
  for (const [totalKey, usedKey, label] of [
    ["messages", "usedMessages", "Messages"],
    ["flowActions", "usedFlowActions", "Flow Actions"],
  ] as const) {
    const total = num(usage[totalKey]);
    const used = num(usage[usedKey]);
    if (total === undefined || total <= 0 || used === undefined) continue;
    windows.push({
      kind: "monthly",
      label,
      usedPercent: clamp((used / total) * 100),
      ...(resetsAt ? { resetsAt } : {}),
    });
  }
  const plan = str(body.planName);
  return { windows, ...(plan ? { plan } : {}) };
};

export const windsurf: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const db =
    env.platform === "darwin"
      ? `${env.home}/Library/Application Support/Windsurf/User/globalStorage/state.vscdb`
      : `${env.env.XDG_CONFIG_HOME ?? `${env.home}/.config`}/Windsurf/User/globalStorage/state.vscdb`;
  const raw = await env.sqliteValue(
    db,
    "SELECT value FROM ItemTable WHERE key = ?",
    "windsurf.settings.cachedPlanInfo",
  );
  if (!raw) throw new NotConfigured("Sign in to Windsurf.");
  const parsed = parseWindsurf(parseJson(raw));
  if (!parsed.plan && parsed.windows.length === 0) throw new NotConfigured("Sign in to Windsurf.");
  return [
    {
      ...base(env, "windsurf", "local"),
      instance: instance?.id ?? "windsurf",
      ...(instance ? { label: instance.name } : {}),
      ...parsed,
    },
  ];
};

export const groq: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const desktopSettings =
    env.platform === "darwin"
      ? `${env.home}/Library/Application Support/groq-desktop-app/settings.json`
      : `${env.env.XDG_CONFIG_HOME ?? `${env.home}/.config`}/groq-desktop-app/settings.json`;
  const file = parseJson(await env.readText(desktopSettings));
  const key = firstEnv(env, ["GROQ_API_KEY"]) ?? str(file.GROQ_API_KEY);
  if (!key) throw new NotConfigured("Set GROQ_API_KEY or sign in to Groq Desktop.");
  const model = str(file.model);
  return [
    {
      ...base(env, "groq", "local"),
      instance: instance?.id ?? "groq",
      ...(instance ? { label: instance.name } : {}),
      plan: model ? `Groq · ${model}` : "API key",
      windows: [],
    },
  ];
};

export const ollama: Fetcher = async (env, instances) => {
  const instance = instances[0];
  const apiKey = firstEnv(env, ["OLLAMA_API_KEY", "OLLAMA_KEY"]);
  const manifests = await env.listFiles(`${env.home}/.ollama/models/manifests`, "");
  if (!apiKey && manifests.length === 0) {
    throw new NotConfigured("Install an Ollama model or set OLLAMA_API_KEY.");
  }
  const count = manifests.length;
  const plan =
    count > 0 ? `${count} local ${count === 1 ? "model" : "models"}` : "Ollama Cloud";
  return [
    {
      ...base(env, "ollama", "local"),
      instance: instance?.id ?? "ollama",
      ...(instance ? { label: instance.name } : {}),
      plan,
      windows: [],
    },
  ];
};
