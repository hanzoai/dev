import {
  buildHanzoIamAccountFromTokens,
  DEFAULT_HANZO_IAM_ISSUER,
  type HanzoIamAccount,
} from "./hanzoIamAuth.ts";

/** The machine's Hanzo identity: the credential vault the CLI, hanzod, and the desktop app all use. */
export const SHARED_IDENTITY_BRAND = "hanzo";

export interface SharedIdentitySnapshot {
  readonly activeAccountId: string | null;
  readonly accounts: ReadonlyArray<HanzoIamAccount>;
}

export const EMPTY_SHARED_IDENTITY: SharedIdentitySnapshot = {
  activeAccountId: null,
  accounts: [],
};

/** `<data_local_dir>/hanzo`, where `credentials` and `config.toml` live. */
export function sharedIdentityDirectory(input: {
  readonly platform: string;
  readonly home: string;
  readonly env: Readonly<Record<string, string | undefined>>;
}): string {
  const { platform, home, env } = input;
  if (platform === "darwin") return `${home}/Library/Application Support/hanzo`;
  if (platform === "win32") return `${env.LOCALAPPDATA ?? `${home}\\AppData\\Local`}\\hanzo`;
  return `${env.XDG_DATA_HOME || `${home}/.local/share`}/hanzo`;
}

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const parseJson = (raw: string | undefined): unknown => {
  if (!raw?.trim()) return null;
  try {
    return JSON.parse(raw);
  } catch {
    return null;
  }
};

/** `[auth.active]` → `hanzo = "admin/z"` in the CLI's config.toml. */
export function parseActiveIdentity(configToml: string | undefined): string | null {
  if (!configToml) return null;
  let inActive = false;
  for (const line of configToml.split(/\r?\n/u)) {
    const trimmed = line.trim();
    if (trimmed.startsWith("[")) {
      inActive = trimmed === "[auth.active]";
      continue;
    }
    if (!inActive) continue;
    const match = /^hanzo\s*=\s*"([^"]+)"/u.exec(trimmed);
    if (match) return match[1] ?? null;
  }
  return null;
}

/**
 * Reads the vault, `{ "<brand>/<owner>/<name>": "<TokenSet JSON>" }`, read-only. Never refreshes:
 * a refresh token is single-use and belongs to whoever holds the refresh lock.
 */
export function parseSharedIdentity(input: {
  readonly credentials: string | undefined;
  readonly config: string | undefined;
}): SharedIdentitySnapshot {
  const vault = parseJson(input.credentials);
  if (!isRecord(vault)) return EMPTY_SHARED_IDENTITY;
  const accounts: HanzoIamAccount[] = [];
  for (const [key, value] of Object.entries(vault)) {
    const [brand, owner, name, ...rest] = key.split("/");
    if (brand !== SHARED_IDENTITY_BRAND || !owner || !name || rest.length > 0) continue;
    const tokens = typeof value === "string" ? parseJson(value) : value;
    if (!isRecord(tokens) || typeof tokens.access_token !== "string" || !tokens.access_token) {
      continue;
    }
    const refreshToken =
      typeof tokens.refresh_token === "string" && tokens.refresh_token
        ? tokens.refresh_token
        : null;
    const account = buildHanzoIamAccountFromTokens({
      issuer: DEFAULT_HANZO_IAM_ISSUER,
      clientId: "hanzo-app",
      accessToken: tokens.access_token,
      refreshToken,
      idToken: typeof tokens.id_token === "string" ? tokens.id_token : null,
      userinfo: { sub: name, owner, name },
    });
    accounts.push({ ...account, id: `${owner}/${name}` });
  }
  const wanted = parseActiveIdentity(input.config);
  const active = accounts.find((account) => account.id === wanted) ?? accounts[0] ?? null;
  return { activeAccountId: active?.id ?? null, accounts };
}

/** A token Dev may send: present, and not within `skewMs` of expiry. */
export function isSharedTokenUsable(
  account: Pick<HanzoIamAccount, "accessToken" | "expiresAt"> | null | undefined,
  now: number,
  skewMs = 30_000,
): boolean {
  if (!account?.accessToken) return false;
  return account.expiresAt === null || account.expiresAt - skewMs > now;
}

/** SuperAdmin if and only if the identity's org is the reserved `admin`. */
export const isSharedSuperAdmin = (account: Pick<HanzoIamAccount, "owner">): boolean =>
  account.owner === "admin";
