// @effect-diagnostics globalDate:off -- OAuth token expiry and PKCE sessions compare wall-clock JWT exp epochs outside an Effect runtime.
import { sha256 } from "@noble/hashes/sha2.js";

export type HanzoIamBrandId = "hanzo" | "lux" | "zoo" | "pars";

export interface HanzoIamBrandConfig {
  readonly id: HanzoIamBrandId;
  readonly label: string;
  readonly issuer: string;
  readonly clientId: string;
  readonly defaultClientId: string;
}

export const HANZO_IAM_BRANDS: ReadonlyArray<HanzoIamBrandConfig> = [
  {
    id: "hanzo",
    label: "Hanzo",
    issuer: "https://hanzo.id",
    clientId: "hanzo-app",
    defaultClientId: "hanzo-app",
  },
  {
    id: "lux",
    label: "Lux",
    issuer: "https://lux.id",
    clientId: "lux-app",
    defaultClientId: "lux-app",
  },
  {
    id: "zoo",
    label: "Zoo",
    issuer: "https://zoolabs.id",
    clientId: "zoo-app",
    defaultClientId: "zoo-app",
  },
  {
    id: "pars",
    label: "Pars",
    issuer: "https://pars.id",
    clientId: "pars-app",
    defaultClientId: "pars-app",
  },
] as const;

export const HANZO_IAM_VAULT_STORAGE_KEY = "hanzo.dev.iam.vault.v1";
export const EMPTY_HANZO_IAM_VAULT: HanzoIamVaultState = {
  activeAccountId: null,
  accounts: [],
  activeOrgId: null,
  orgByAccountId: {},
};

export const DEFAULT_HANZO_IAM_BRAND: HanzoIamBrandConfig = HANZO_IAM_BRANDS[0]!;
export const DEFAULT_HANZO_IAM_ISSUER = DEFAULT_HANZO_IAM_BRAND.issuer;
export const DEFAULT_HANZO_IAM_CLIENT_ID = DEFAULT_HANZO_IAM_BRAND.clientId;
export const DEFAULT_HANZO_RELAY_URL = "https://relay.dev.hanzo.ai";
export const DEFAULT_HANZO_API_BASE_URL = "https://api.hanzo.ai";
export const HANZO_BILLING_PORTAL_URL = "https://billing.hanzo.ai";
export const HANZO_IAM_OAUTH_SCOPES = ["openid", "profile", "email", "offline_access"] as const;
export const HANZO_IAM_CALLBACK_SCHEMES = [
  "hanzo",
  "hanzodev",
  "hanzo-dev",
  "hanzo-preview",
] as const;

export interface HanzoIamOrganization {
  readonly id: string;
  readonly name: string;
  readonly displayName: string;
  readonly role?: string | null;
}

export interface HanzoIamAccount {
  /** Canonical multi-org account key (`${owner}/${subject}`). */
  readonly id: string;
  readonly brand: HanzoIamBrandId;
  readonly issuer: string;
  readonly clientId: string;
  readonly accessToken: string;
  readonly refreshToken: string | null;
  /** Epoch milliseconds when `accessToken` expires, or `null` for non-expiring API keys. */
  readonly expiresAt: number | null;
  readonly subject: string;
  readonly owner: string;
  readonly email: string | null;
  readonly displayName: string | null;
  readonly avatarUrl?: string | null;
  readonly organizations?: ReadonlyArray<HanzoIamOrganization>;
}

export interface HanzoIamVaultState {
  readonly activeAccountId: string | null;
  readonly accounts: ReadonlyArray<HanzoIamAccount>;
  readonly activeOrgId?: string | null;
  readonly orgByAccountId?: Readonly<Record<string, string>>;
}

export type HanzoSubscriptionPlanId = "free" | "go" | "pro" | "team" | "ultra";

export interface HanzoSubscriptionPlan {
  readonly id: HanzoSubscriptionPlanId;
  readonly name: string;
  readonly priceLabel: string;
  readonly monthlyAllowanceCents: number;
  readonly summary: string;
}

export const HANZO_SUBSCRIPTION_PLANS: ReadonlyArray<HanzoSubscriptionPlan> = [
  {
    id: "free",
    name: "Free",
    priceLabel: "$0/mo",
    monthlyAllowanceCents: 500,
    summary: "Local Zen Qwen3+ AI & starter cloud access",
  },
  {
    id: "go",
    name: "Go",
    priceLabel: "$8/mo",
    monthlyAllowanceCents: 1_000,
    summary: "Everyday cloud models & remote mobile pairing",
  },
  {
    id: "pro",
    name: "Pro",
    priceLabel: "$15/mo",
    monthlyAllowanceCents: 2_500,
    summary: "Frontier coding agents, Hanzo Connect tunnels & sandboxes",
  },
  {
    id: "team",
    name: "Team",
    priceLabel: "Team",
    monthlyAllowanceCents: 10_000,
    summary: "Shared organization workspaces, SSO & X-Org-Id governance",
  },
  {
    id: "ultra",
    name: "Ultra",
    priceLabel: "$195/mo",
    monthlyAllowanceCents: 25_000,
    summary: "Maximum parallel agents, priority GPUs & highest limits",
  },
] as const;

export interface HanzoBillingSummary {
  readonly planId: HanzoSubscriptionPlanId;
  readonly planName: string;
  readonly priceLabel: string;
  readonly creditBalanceCents: number;
  readonly monthlyUsedCents: number;
  readonly monthlyLimitCents: number;
  readonly billingPortalUrl: string;
}

export interface HanzoEcosystemSurface {
  readonly id: "hanzo-app" | "hanzo-bot" | "hanzo-team" | "hanzo-console" | "hanzo-billing";
  readonly label: string;
  readonly hostname: string;
  readonly url: string;
  readonly description: string;
}

export const HANZO_ECOSYSTEM_SURFACES: ReadonlyArray<HanzoEcosystemSurface> = [
  {
    id: "hanzo-app",
    label: "Hanzo Web",
    hostname: "hanzo.app",
    url: "https://hanzo.app",
    description: "Unified cloud workspace & chat",
  },
  {
    id: "hanzo-bot",
    label: "Hanzo Bot",
    hostname: "hanzo.bot",
    url: "https://hanzo.bot",
    description: "Remote coding agents & tunnels",
  },
  {
    id: "hanzo-team",
    label: "Hanzo Team",
    hostname: "hanzo.team",
    url: "https://hanzo.team",
    description: "Organization collaboration & members",
  },
  {
    id: "hanzo-console",
    label: "Hanzo Console",
    hostname: "platform.hanzo.ai",
    url: "https://platform.hanzo.ai",
    description: "API keys, deployments & observability",
  },
  {
    id: "hanzo-billing",
    label: "Billing Portal",
    hostname: "billing.hanzo.ai",
    url: HANZO_BILLING_PORTAL_URL,
    description: "Plans, invoices & credit top-ups",
  },
] as const;

export interface ParsedHanzoIamCallback {
  readonly code: string | null;
  readonly state: string | null;
  readonly error: string | null;
  readonly errorDescription: string | null;
  readonly brand: HanzoIamBrandId | null;
}

export function getHanzoIamBrand(brandId: string | null | undefined): HanzoIamBrandConfig {
  const normalized = brandId?.trim().toLowerCase();
  return HANZO_IAM_BRANDS.find((entry) => entry.id === normalized) ?? DEFAULT_HANZO_IAM_BRAND;
}

export function normalizeIamIssuer(raw: string | null | undefined): string {
  const trimmed = raw?.trim() ?? "";
  if (!trimmed) return DEFAULT_HANZO_IAM_ISSUER;
  try {
    const parsed = new URL(trimmed);
    if (parsed.protocol !== "https:" && parsed.protocol !== "http:") {
      return DEFAULT_HANZO_IAM_ISSUER;
    }
    return parsed.origin;
  } catch {
    return DEFAULT_HANZO_IAM_ISSUER;
  }
}

export const normalizeHanzoIssuer = normalizeIamIssuer;

export function parseHanzoIamVaultJson(raw: string | null | undefined): HanzoIamVaultState {
  if (!raw?.trim()) return EMPTY_HANZO_IAM_VAULT;
  try {
    const parsed = JSON.parse(raw) as Partial<HanzoIamVaultState>;
    const accounts = Array.isArray(parsed.accounts)
      ? parsed.accounts.filter(
          (entry): entry is HanzoIamAccount =>
            Boolean(entry && typeof entry.id === "string" && typeof entry.accessToken === "string"),
        )
      : [];
    const activeAccount =
      accounts.find((entry) => entry.id === parsed.activeAccountId) ?? accounts[0] ?? null;
    const orgByAccountId =
      parsed.orgByAccountId && typeof parsed.orgByAccountId === "object"
        ? (parsed.orgByAccountId as Readonly<Record<string, string>>)
        : {};
    const activeOrgId = activeAccount
      ? (orgByAccountId[activeAccount.id] ??
          (typeof parsed.activeOrgId === "string" && parsed.activeOrgId.trim()
            ? parsed.activeOrgId.trim()
            : activeAccount.owner))
      : null;
    return {
      activeAccountId: activeAccount?.id ?? null,
      accounts,
      activeOrgId,
      orgByAccountId,
    };
  } catch {
    return EMPTY_HANZO_IAM_VAULT;
  }
}

export function serializeHanzoIamVaultJson(vault: HanzoIamVaultState): string {
  return JSON.stringify(vault);
}

export function parseHanzoIamCallbackUrl(rawUrl: string): ParsedHanzoIamCallback | null {
  const trimmed = rawUrl.trim();
  if (!trimmed) return null;
  let parsed: URL;
  try {
    parsed = new URL(trimmed);
  } catch {
    return null;
  }

  const scheme = parsed.protocol.replace(/:$/u, "").toLowerCase();
  const isCustomScheme = (HANZO_IAM_CALLBACK_SCHEMES as ReadonlyArray<string>).includes(scheme);
  const isHttpScheme = scheme === "https" || scheme === "http";
  if (!isCustomScheme && !isHttpScheme) {
    return null;
  }

  // Custom schemes parse `hanzo://oauth/callback` as host="oauth", pathname="/callback",
  // and `hanzo:///oauth/callback` or Expo `hanzo-dev://--/oauth/callback` in pathname.
  const combinedPath = isCustomScheme
    ? `/${[parsed.host, ...parsed.pathname.split("/")].filter(Boolean).join("/")}`
    : parsed.pathname;
  const normalizedPath = combinedPath.replace(/\/+/gu, "/").replace(/\/$/u, "").toLowerCase();

  const oauthMatch = /(?:^|\/)(?:oauth|auth)\/(?:callback|(hanzo|lux|zoo|pars))$/u.exec(
    normalizedPath,
  );
  if (!oauthMatch) {
    return null;
  }

  const trimParam = (key: string): string | null => {
    const value = parsed.searchParams.get(key)?.trim();
    return value ? value : null;
  };

  const brandParam = (oauthMatch[1] ?? trimParam("brand"))?.toLowerCase();
  const matchedBrand =
    HANZO_IAM_BRANDS.find((entry) => entry.id === brandParam)?.id ?? null;

  const code = trimParam("code");
  const state = trimParam("state");
  const error = trimParam("error");
  const errorDescription = trimParam("error_description");
  if (!code && !error) {
    return null;
  }

  return {
    code,
    state,
    error,
    errorDescription,
    brand: matchedBrand,
  };
}

function toBase64Url(bytes: Uint8Array): string {
  let binary = "";
  for (let i = 0; i < bytes.length; i++) {
    binary += String.fromCharCode(bytes[i]!);
  }
  return globalThis
    .btoa(binary)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/g, "");
}

function fromBase64UrlToUtf8(segment: string): string | null {
  try {
    const padded = segment.replace(/-/g, "+").replace(/_/g, "/");
    const padLength = (4 - (padded.length % 4)) % 4;
    const binary = globalThis.atob(padded + "=".repeat(padLength));
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i);
    }
    return new TextDecoder().decode(bytes);
  } catch {
    return null;
  }
}

export function createPkceVerifier(randomBytes?: Uint8Array): string {
  const bytes =
    randomBytes ??
    (() => {
      const buf = new Uint8Array(32);
      globalThis.crypto.getRandomValues(buf);
      return buf;
    })();
  return toBase64Url(bytes);
}

export function createPkceChallengeS256(verifier: string): string {
  const digest = sha256(new TextEncoder().encode(verifier));
  return toBase64Url(digest);
}

export function createOAuthState(): string {
  const buf = new Uint8Array(18);
  globalThis.crypto.getRandomValues(buf);
  return toBase64Url(buf);
}

export function buildHanzoIamAuthorizeUrl(input: {
  readonly issuer: string;
  readonly clientId: string;
  readonly redirectUri: string;
  readonly state: string;
  readonly codeChallenge: string;
  readonly scopes?: ReadonlyArray<string>;
}): string {
  const origin = normalizeIamIssuer(input.issuer);
  const url = new URL("/oauth/authorize", origin);
  url.searchParams.set("client_id", input.clientId);
  url.searchParams.set("redirect_uri", input.redirectUri);
  url.searchParams.set("response_type", "code");
  url.searchParams.set("scope", (input.scopes ?? HANZO_IAM_OAUTH_SCOPES).join(" "));
  url.searchParams.set("state", input.state);
  url.searchParams.set("code_challenge", input.codeChallenge);
  url.searchParams.set("code_challenge_method", "S256");
  return url.toString();
}

export interface DecodedIamClaims {
  readonly subject: string | null;
  readonly owner: string | null;
  readonly name: string | null;
  readonly email: string | null;
  readonly avatarUrl: string | null;
  readonly organizations: ReadonlyArray<HanzoIamOrganization>;
  readonly expiresAt: number | null;
  readonly issuer: string | null;
}

function normalizeOrganizationEntries(
  rawOrgs: unknown,
  fallbackOwner: string | null,
): ReadonlyArray<HanzoIamOrganization> {
  const seen = new Set<string>();
  const result: HanzoIamOrganization[] = [];
  const push = (id: string, displayName?: string | null, role?: string | null) => {
    const normalizedId = id.trim();
    if (!normalizedId || seen.has(normalizedId)) return;
    seen.add(normalizedId);
    result.push({
      id: normalizedId,
      name: normalizedId,
      displayName: displayName?.trim() || normalizedId,
      ...(role ? { role } : {}),
    });
  };

  if (Array.isArray(rawOrgs)) {
    for (const item of rawOrgs) {
      if (typeof item === "string") {
        push(item);
      } else if (item && typeof item === "object") {
        const record = item as Record<string, unknown>;
        const id =
          (typeof record.name === "string" && record.name) ||
          (typeof record.id === "string" && record.id) ||
          (typeof record.slug === "string" && record.slug) ||
          "";
        const displayName =
          (typeof record.displayName === "string" && record.displayName) ||
          (typeof record.display_name === "string" && record.display_name) ||
          id;
        const role = typeof record.role === "string" ? record.role : null;
        if (id) push(id, displayName, role);
      }
    }
  }
  if (fallbackOwner) {
    push(fallbackOwner);
  }
  return result;
}

export function decodeHanzoJwtClaims(token: string): DecodedIamClaims {
  const parts = token.split(".");
  if (parts.length < 2) {
    return {
      subject: null,
      owner: null,
      name: null,
      email: null,
      avatarUrl: null,
      organizations: [],
      expiresAt: null,
      issuer: null,
    };
  }
  const json = fromBase64UrlToUtf8(parts[1]!);
  if (!json) {
    return {
      subject: null,
      owner: null,
      name: null,
      email: null,
      avatarUrl: null,
      organizations: [],
      expiresAt: null,
      issuer: null,
    };
  }
  try {
    const raw = JSON.parse(json) as Record<string, unknown>;
    const str = (key: string): string | null => {
      const value = raw[key];
      return typeof value === "string" && value.trim() ? value.trim() : null;
    };
    const exp = raw.exp;
    const expiresAt = typeof exp === "number" && Number.isFinite(exp) ? exp * 1000 : null;
    const owner = str("owner") ?? str("org") ?? str("organization");
    const name = str("name") ?? str("preferred_username");
    const subject = str("sub") ?? name;
    const avatarUrl = str("avatar") ?? str("picture");
    const organizations = normalizeOrganizationEntries(
      raw.organizations ?? raw.orgs,
      owner,
    );
    return {
      subject,
      owner,
      name,
      email: str("email"),
      avatarUrl,
      organizations,
      expiresAt,
      issuer: str("iss"),
    };
  } catch {
    return {
      subject: null,
      owner: null,
      name: null,
      email: null,
      avatarUrl: null,
      organizations: [],
      expiresAt: null,
      issuer: null,
    };
  }
}

export function buildHanzoIamAccountFromTokens(input: {
  readonly brand?: HanzoIamBrandId;
  readonly issuer: string;
  readonly clientId: string;
  readonly accessToken: string;
  readonly refreshToken?: string | null;
  readonly idToken?: string | null;
  readonly expiresInSeconds?: number | null;
  readonly organizations?: ReadonlyArray<HanzoIamOrganization>;
  readonly userinfo?: {
    readonly sub?: string | null;
    readonly owner?: string | null;
    readonly name?: string | null;
    readonly displayName?: string | null;
    readonly email?: string | null;
    readonly avatarUrl?: string | null;
    readonly organizations?: ReadonlyArray<HanzoIamOrganization>;
  } | null;
}): HanzoIamAccount {
  const accessClaims = decodeHanzoJwtClaims(input.accessToken);
  const idClaims = input.idToken ? decodeHanzoJwtClaims(input.idToken) : null;
  const brand =
    input.brand ??
    HANZO_IAM_BRANDS.find((entry) => normalizeIamIssuer(input.issuer) === entry.issuer)?.id ??
    "hanzo";
  const owner =
    input.userinfo?.owner?.trim() ||
    accessClaims.owner ||
    idClaims?.owner ||
    brand;
  const subject =
    input.userinfo?.sub?.trim() ||
    input.userinfo?.name?.trim() ||
    accessClaims.name ||
    idClaims?.name ||
    accessClaims.subject ||
    idClaims?.subject ||
    "user";
  const email = input.userinfo?.email?.trim() || accessClaims.email || idClaims?.email || null;
  const displayName =
    input.userinfo?.displayName?.trim() ||
    input.userinfo?.name?.trim() ||
    accessClaims.name ||
    idClaims?.name ||
    email ||
    `${owner}/${subject}`;
  const avatarUrl =
    input.userinfo?.avatarUrl?.trim() ||
    accessClaims.avatarUrl ||
    idClaims?.avatarUrl ||
    null;
  const expiresAt =
    typeof input.expiresInSeconds === "number" && input.expiresInSeconds > 0
      ? Date.now() + input.expiresInSeconds * 1000
      : (accessClaims.expiresAt ?? idClaims?.expiresAt ?? null);
  const mergedOrganizations = normalizeOrganizationEntries(
    [
      ...(input.organizations ?? []),
      ...(input.userinfo?.organizations ?? []),
      ...accessClaims.organizations,
      ...(idClaims?.organizations ?? []),
    ],
    owner,
  );

  return {
    id: `${owner}/${subject}`,
    brand,
    issuer: normalizeIamIssuer(input.issuer),
    clientId: input.clientId,
    accessToken: input.accessToken,
    refreshToken: input.refreshToken ?? null,
    expiresAt,
    subject,
    owner,
    email,
    displayName,
    avatarUrl,
    organizations: mergedOrganizations,
  };
}

export async function fetchHanzoIamUserinfo(
  issuer: string,
  accessToken: string,
  fetchImpl: typeof globalThis.fetch = globalThis.fetch,
): Promise<{
  readonly sub?: string | null;
  readonly owner?: string | null;
  readonly name?: string | null;
  readonly displayName?: string | null;
  readonly email?: string | null;
  readonly avatarUrl?: string | null;
  readonly organizations?: ReadonlyArray<HanzoIamOrganization>;
} | null> {
  const origin = normalizeIamIssuer(issuer);
  for (const endpoint of ["/api/userinfo", "/oauth/userinfo"]) {
    try {
      const res = await fetchImpl(new URL(endpoint, origin).toString(), {
        headers: {
          Authorization: `Bearer ${accessToken}`,
          Accept: "application/json",
        },
      });
      if (!res.ok) continue;
      const body = (await res.json()) as Record<string, unknown>;
      const str = (key: string) =>
        typeof body[key] === "string" && (body[key] as string).trim()
          ? (body[key] as string).trim()
          : null;
      const owner = str("owner") ?? str("org");
      return {
        sub: str("sub") ?? str("id"),
        owner,
        name: str("name") ?? str("preferred_username"),
        displayName: str("displayName") ?? str("display_name") ?? str("name"),
        email: str("email"),
        avatarUrl: str("avatar") ?? str("picture"),
        organizations: normalizeOrganizationEntries(
          body.organizations ?? body.orgs,
          owner,
        ),
      };
    } catch {
      // Fall through to next candidate userinfo path or token claims.
    }
  }
  return null;
}

export function buildHanzoApiHeaders(input: {
  readonly accessToken?: string | null | undefined;
  readonly orgId?: string | null | undefined;
  readonly extra?: Readonly<Record<string, string>> | undefined;
}): Record<string, string> {
  const headers: Record<string, string> = {
    Accept: "application/json",
    ...input.extra,
  };
  const token = input.accessToken?.trim();
  if (token) {
    headers.Authorization = `Bearer ${token}`;
  }
  const orgId = input.orgId?.trim();
  if (orgId) {
    headers["X-Org-Id"] = orgId;
  }
  return headers;
}

export async function fetchHanzoIamOrganizations(input: {
  readonly issuer?: string;
  readonly apiBaseUrl?: string;
  readonly accessToken: string;
  readonly orgId?: string | null;
  readonly fallbackOwner?: string | null;
  readonly fetchImpl?: typeof globalThis.fetch;
}): Promise<ReadonlyArray<HanzoIamOrganization>> {
  const fetchImpl = input.fetchImpl ?? globalThis.fetch;
  const issuerOrigin = normalizeIamIssuer(input.issuer);
  const apiOrigin = input.apiBaseUrl?.trim() || DEFAULT_HANZO_API_BASE_URL;
  const candidates = [
    new URL("/api/get-organizations", issuerOrigin).toString(),
    new URL("/v1/organizations", apiOrigin).toString(),
  ];

  for (const endpoint of candidates) {
    try {
      const response = await fetchImpl(endpoint, {
        headers: buildHanzoApiHeaders({
          accessToken: input.accessToken,
          orgId: input.orgId,
        }),
      });
      if (!response.ok) continue;
      const payload = (await response.json()) as unknown;
      const rawList = Array.isArray(payload)
        ? payload
        : payload && typeof payload === "object"
          ? ((payload as Record<string, unknown>).data ??
              (payload as Record<string, unknown>).organizations)
          : null;
      const normalized = normalizeOrganizationEntries(rawList, input.fallbackOwner ?? null);
      if (normalized.length > 0) return normalized;
    } catch {
      // Fall through to next endpoint or fallback owner.
    }
  }

  return normalizeOrganizationEntries([], input.fallbackOwner ?? null);
}

export function defaultHanzoBillingSummary(
  planId: HanzoSubscriptionPlanId = "pro",
): HanzoBillingSummary {
  const plan =
    HANZO_SUBSCRIPTION_PLANS.find((entry) => entry.id === planId) ?? HANZO_SUBSCRIPTION_PLANS[2]!;
  return {
    planId: plan.id,
    planName: plan.name,
    priceLabel: plan.priceLabel,
    creditBalanceCents: 0,
    monthlyUsedCents: 0,
    monthlyLimitCents: plan.monthlyAllowanceCents,
    billingPortalUrl: HANZO_BILLING_PORTAL_URL,
  };
}

export async function fetchHanzoBillingSummary(input: {
  readonly accessToken: string;
  readonly orgId?: string | null;
  readonly apiBaseUrl?: string;
  readonly fetchImpl?: typeof globalThis.fetch;
}): Promise<HanzoBillingSummary> {
  const fetchImpl = input.fetchImpl ?? globalThis.fetch;
  const apiOrigin = input.apiBaseUrl?.trim() || DEFAULT_HANZO_API_BASE_URL;
  try {
    const response = await fetchImpl(new URL("/v1/billing/summary", apiOrigin).toString(), {
      headers: buildHanzoApiHeaders({
        accessToken: input.accessToken,
        orgId: input.orgId,
      }),
    });
    if (response.ok) {
      const body = (await response.json()) as Record<string, unknown>;
      const rawPlan = typeof body.plan === "string" ? body.plan.trim().toLowerCase() : "";
      const matchedPlan =
        HANZO_SUBSCRIPTION_PLANS.find((entry) => entry.id === rawPlan) ??
        HANZO_SUBSCRIPTION_PLANS[0]!;
      const num = (key: string, fallback: number) => {
        const value = body[key];
        return typeof value === "number" && Number.isFinite(value) ? Math.max(0, value) : fallback;
      };
      return {
        planId: matchedPlan.id,
        planName: matchedPlan.name,
        priceLabel: matchedPlan.priceLabel,
        creditBalanceCents: num("credit_balance_cents", num("balance_cents", 0)),
        monthlyUsedCents: num("monthly_used_cents", num("used_cents", 0)),
        monthlyLimitCents: num(
          "monthly_limit_cents",
          matchedPlan.monthlyAllowanceCents,
        ),
        billingPortalUrl:
          typeof body.portal_url === "string" && body.portal_url.startsWith("https://")
            ? body.portal_url
            : buildHanzoSurfaceLaunchUrl(HANZO_BILLING_PORTAL_URL, { orgId: input.orgId }),
      };
    }
  } catch {
    // Offline or local-only session: return Free/starter summary with billing portal link.
  }
  return {
    ...defaultHanzoBillingSummary("free"),
    billingPortalUrl: buildHanzoSurfaceLaunchUrl(HANZO_BILLING_PORTAL_URL, { orgId: input.orgId }),
  };
}

export function buildHanzoSurfaceLaunchUrl(
  baseUrl: string,
  context?: {
    readonly orgId?: string | null | undefined;
    readonly accountId?: string | null | undefined;
  },
): string {
  try {
    const url = new URL(baseUrl);
    const orgId = context?.orgId?.trim();
    if (orgId) {
      url.searchParams.set("org", orgId);
    }
    const accountId = context?.accountId?.trim();
    if (accountId) {
      url.searchParams.set("account", accountId);
    }
    return url.toString();
  } catch {
    return baseUrl;
  }
}

export async function exchangeHanzoIamAuthorizationCode(input: {
  readonly issuer: string;
  readonly clientId: string;
  readonly code: string;
  readonly redirectUri: string;
  readonly codeVerifier: string;
  readonly brand?: HanzoIamBrandId;
  readonly fetchImpl?: typeof globalThis.fetch;
}): Promise<HanzoIamAccount> {
  const fetchImpl = input.fetchImpl ?? globalThis.fetch;
  const origin = normalizeIamIssuer(input.issuer);
  const body = new URLSearchParams({
    grant_type: "authorization_code",
    client_id: input.clientId,
    code: input.code,
    redirect_uri: input.redirectUri,
    code_verifier: input.codeVerifier,
  });
  const response = await fetchImpl(new URL("/oauth/token", origin).toString(), {
    method: "POST",
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
      Accept: "application/json",
    },
    body: body.toString(),
  });
  if (!response.ok) {
    throw new Error(`Hanzo IAM token exchange failed (${response.status}).`);
  }
  const tokenPayload = (await response.json()) as {
    readonly access_token?: string;
    readonly refresh_token?: string;
    readonly id_token?: string;
    readonly expires_in?: number;
  };
  const accessToken = tokenPayload.access_token?.trim();
  if (!accessToken) {
    throw new Error("Hanzo IAM token response did not contain an access_token.");
  }
  const userinfo = await fetchHanzoIamUserinfo(origin, accessToken, fetchImpl);
  return buildHanzoIamAccountFromTokens({
    ...(input.brand ? { brand: input.brand } : {}),
    issuer: origin,
    clientId: input.clientId,
    accessToken,
    refreshToken: tokenPayload.refresh_token ?? null,
    idToken: tokenPayload.id_token ?? null,
    expiresInSeconds: tokenPayload.expires_in ?? null,
    userinfo,
  });
}

export async function refreshHanzoIamAccount(
  account: HanzoIamAccount,
  fetchImpl: typeof globalThis.fetch = globalThis.fetch,
): Promise<HanzoIamAccount> {
  if (!account.refreshToken) {
    return account;
  }
  const body = new URLSearchParams({
    grant_type: "refresh_token",
    client_id: account.clientId,
    refresh_token: account.refreshToken,
  });
  const response = await fetchImpl(new URL("/oauth/token", account.issuer).toString(), {
    method: "POST",
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
      Accept: "application/json",
    },
    body: body.toString(),
  });
  if (!response.ok) {
    throw new Error(`Hanzo IAM token refresh failed (${response.status}).`);
  }
  const tokenPayload = (await response.json()) as {
    readonly access_token?: string;
    readonly refresh_token?: string;
    readonly id_token?: string;
    readonly expires_in?: number;
  };
  const accessToken = tokenPayload.access_token?.trim();
  if (!accessToken) {
    throw new Error("Hanzo IAM refresh response did not contain an access_token.");
  }
  const refreshed = buildHanzoIamAccountFromTokens({
    brand: account.brand,
    issuer: account.issuer,
    clientId: account.clientId,
    accessToken,
    refreshToken: tokenPayload.refresh_token ?? account.refreshToken,
    idToken: tokenPayload.id_token ?? null,
    expiresInSeconds: tokenPayload.expires_in ?? null,
    ...(account.organizations ? { organizations: account.organizations } : {}),
    userinfo: {
      sub: account.subject,
      owner: account.owner,
      displayName: account.displayName,
      email: account.email,
      avatarUrl: account.avatarUrl ?? null,
    },
  });
  return {
    ...refreshed,
    id: account.id,
  };
}

export function isHanzoIamTokenExpiringSoon(
  account: Pick<HanzoIamAccount, "expiresAt">,
  skewMs = 60_000,
): boolean {
  if (account.expiresAt === null) return false;
  return Date.now() + skewMs >= account.expiresAt;
}

export function resolveActiveHanzoAccount(vault: HanzoIamVaultState): HanzoIamAccount | null {
  if (vault.accounts.length === 0) return null;
  return (
    vault.accounts.find((entry) => entry.id === vault.activeAccountId) ?? vault.accounts[0] ?? null
  );
}

export function resolveAccountOrganizations(
  account: HanzoIamAccount | null,
): ReadonlyArray<HanzoIamOrganization> {
  if (!account) return [];
  return normalizeOrganizationEntries(account.organizations ?? [], account.owner);
}

export function resolveActiveHanzoOrgId(vault: HanzoIamVaultState): string | null {
  const active = resolveActiveHanzoAccount(vault);
  if (!active) return null;
  const savedForAccount = vault.orgByAccountId?.[active.id]?.trim();
  if (savedForAccount) return savedForAccount;
  const explicit = vault.activeOrgId?.trim();
  if (explicit) return explicit;
  return active.owner;
}

export function switchHanzoIamVaultOrg(
  vault: HanzoIamVaultState,
  orgId: string,
): HanzoIamVaultState {
  const trimmed = orgId.trim();
  if (!trimmed) return vault;
  const active = resolveActiveHanzoAccount(vault);
  return {
    ...vault,
    activeOrgId: trimmed,
    orgByAccountId: {
      ...vault.orgByAccountId,
      ...(active ? { [active.id]: trimmed } : {}),
    },
  };
}

export function upsertHanzoIamVaultAccount(
  vault: HanzoIamVaultState,
  account: HanzoIamAccount,
  makeActive = true,
): HanzoIamVaultState {
  const existingIndex = vault.accounts.findIndex((entry) => entry.id === account.id);
  const accounts =
    existingIndex === -1
      ? [...vault.accounts, account]
      : vault.accounts.map((entry, idx) => (idx === existingIndex ? account : entry));
  const nextActiveAccountId = makeActive ? account.id : (vault.activeAccountId ?? account.id);
  const nextActive = accounts.find((entry) => entry.id === nextActiveAccountId) ?? account;
  const orgByAccountId = {
    ...vault.orgByAccountId,
    [account.id]: vault.orgByAccountId?.[account.id] ?? account.owner,
  };
  return {
    activeAccountId: nextActiveAccountId,
    accounts,
    activeOrgId: orgByAccountId[nextActive.id] ?? nextActive.owner,
    orgByAccountId,
  };
}

export function removeHanzoIamVaultAccount(
  vault: HanzoIamVaultState,
  accountId: string,
): HanzoIamVaultState {
  const accounts = vault.accounts.filter((entry) => entry.id !== accountId);
  const activeAccountId =
    vault.activeAccountId === accountId ? (accounts[0]?.id ?? null) : vault.activeAccountId;
  const nextOrgMap: Record<string, string> = {};
  if (vault.orgByAccountId) {
    for (const [key, value] of Object.entries(vault.orgByAccountId)) {
      if (key !== accountId) nextOrgMap[key] = value;
    }
  }
  const nextActive = accounts.find((entry) => entry.id === activeAccountId) ?? null;
  return {
    activeAccountId,
    accounts,
    activeOrgId: nextActive ? (nextOrgMap[nextActive.id] ?? nextActive.owner) : null,
    orgByAccountId: nextOrgMap,
  };
}
