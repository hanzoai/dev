import type { CloudAccountSource } from "@hanzodev/client-runtime/relay";
import {
  buildHanzoApiHeaders,
  buildHanzoIamAccountFromTokens,
  buildHanzoIamAuthorizeUrl,
  buildHanzoSurfaceLaunchUrl,
  createOAuthState,
  createPkceChallengeS256,
  createPkceVerifier,
  EMPTY_HANZO_IAM_VAULT,
  exchangeHanzoIamAuthorizationCode,
  fetchHanzoBillingSummary,
  fetchHanzoIamOrganizations,
  fetchHanzoIamUserinfo,
  HANZO_BILLING_PORTAL_URL,
  HANZO_ECOSYSTEM_SURFACES,
  HANZO_IAM_BRANDS,
  HANZO_IAM_VAULT_STORAGE_KEY,
  HANZO_SUBSCRIPTION_PLANS,
  type HanzoBillingSummary,
  type HanzoIamAccount,
  type HanzoIamBrandId,
  type HanzoIamOrganization,
  type HanzoIamVaultState,
  isHanzoIamTokenExpiringSoon,
  normalizeHanzoIssuer,
  parseHanzoIamCallbackUrl,
  parseHanzoIamVaultJson,
  refreshHanzoIamAccount,
  removeHanzoIamVaultAccount,
  resolveAccountOrganizations,
  resolveActiveHanzoAccount,
  resolveActiveHanzoOrgId,
  serializeHanzoIamVaultJson,
  switchHanzoIamVaultOrg,
  upsertHanzoIamVaultAccount,
} from "@hanzodev/shared/hanzoIamAuth";
import { T, useGT } from "gt-react";
import {
  Building2Icon,
  CheckIcon,
  CpuIcon,
  CreditCardIcon,
  ExternalLinkIcon,
  GlobeIcon,
  KeyRoundIcon,
  LogOutIcon,
  PlusIcon,
  RefreshCwIcon,
  ShieldCheckIcon,
  Trash2Icon,
  XIcon,
} from "lucide-react";
import {
  createContext,
  type ReactNode,
  useContext,
  useEffect,
  useState,
  useSyncExternalStore,
} from "react";

import { readDesktopPrimaryBearerToken } from "../environments/primary/desktopAuth";
import { resolvePrimaryEnvironmentHttpUrl } from "../environments/primary/target";
import { isHostedStaticApp } from "../hostedPairing";
import { openLocalAiManager } from "../localAi/localAiState";
import { ManagedRelayAuthProvider } from "./managedAuth";
import { resolveCloudPublicConfig } from "./publicConfig";

const PKCE_STORAGE_KEY = "hanzo.dev.iam.pkce.v1";
const DESKTOP_OAUTH_REDIRECT_URI = "hanzodev://oauth/callback";
const EMPTY_ACCOUNTS: ReadonlyArray<HanzoIamAccount> = [];
const EMPTY_ORGANIZATIONS: ReadonlyArray<HanzoIamOrganization> = [];

interface PendingPkceState {
  readonly state: string;
  readonly verifier: string;
  readonly redirectUri: string;
  readonly brandId: HanzoIamBrandId;
  readonly issuer: string;
  readonly clientId: string;
  readonly returnUrl: string;
}

interface LocalHanzoIdentitiesResponse {
  readonly activeAccountId: string | null;
  readonly accounts: ReadonlyArray<HanzoIamAccount>;
}

export interface HanzoCustomAccountPage {
  readonly url: string;
  readonly label: string;
  readonly icon: ReactNode;
  readonly content: ReactNode;
}

export type ModalTabId = "accounts" | "billing" | "surfaces" | string;

interface ModalState {
  readonly open: boolean;
  readonly tab: ModalTabId;
  readonly pages: ReadonlyArray<HanzoCustomAccountPage>;
}

function readBrowserVault(): HanzoIamVaultState {
  if (typeof window === "undefined") return EMPTY_HANZO_IAM_VAULT;
  try {
    return parseHanzoIamVaultJson(window.localStorage.getItem(HANZO_IAM_VAULT_STORAGE_KEY));
  } catch {
    return EMPTY_HANZO_IAM_VAULT;
  }
}

function writeBrowserVault(vault: HanzoIamVaultState): void {
  if (typeof window === "undefined") return;
  try {
    if (vault.accounts.length === 0) {
      window.localStorage.removeItem(HANZO_IAM_VAULT_STORAGE_KEY);
    } else {
      window.localStorage.setItem(HANZO_IAM_VAULT_STORAGE_KEY, serializeHanzoIamVaultJson(vault));
    }
  } catch {
    // Ignore private-browsing storage quota errors
  }
}

function readPendingPkce(): PendingPkceState | null {
  if (typeof window === "undefined") return null;
  try {
    const raw =
      window.sessionStorage.getItem(PKCE_STORAGE_KEY) ??
      window.localStorage.getItem(PKCE_STORAGE_KEY);
    if (!raw) return null;
    return JSON.parse(raw) as PendingPkceState;
  } catch {
    return null;
  }
}

function writePendingPkce(pending: PendingPkceState | null): void {
  if (typeof window === "undefined") return;
  try {
    if (pending === null) {
      window.sessionStorage.removeItem(PKCE_STORAGE_KEY);
      window.localStorage.removeItem(PKCE_STORAGE_KEY);
    } else {
      const encoded = JSON.stringify(pending);
      window.sessionStorage.setItem(PKCE_STORAGE_KEY, encoded);
      window.localStorage.setItem(PKCE_STORAGE_KEY, encoded);
    }
  } catch {
    // Ignore restricted storage errors
  }
}

function openExternalUrl(url: string): void {
  if (typeof window === "undefined") return;
  if (window.desktopBridge?.openExternal) {
    void window.desktopBridge.openExternal(url);
    return;
  }
  window.open(url, "_blank", "noopener,noreferrer");
}

function isSameOriginBrowserPrimary(): boolean {
  if (
    typeof window === "undefined" ||
    window.desktopBridge !== undefined ||
    !window.location.origin.startsWith("http")
  ) {
    return false;
  }
  try {
    return new URL(resolvePrimaryEnvironmentHttpUrl("/")).origin === window.location.origin;
  } catch {
    return false;
  }
}

function formatUsdCents(cents: number): string {
  return `$${(cents / 100).toFixed(2)}`;
}

export class HanzoWebAuthController implements CloudAccountSource {
  private vault: HanzoIamVaultState = EMPTY_HANZO_IAM_VAULT;
  private loaded = false;
  private busy = false;
  private keychainBacked = false;
  private lastError: string | null = null;
  private organizationsByAccount = new Map<string, ReadonlyArray<HanzoIamOrganization>>();
  private billingSummary: HanzoBillingSummary | null = null;
  private pendingDesktopPkce: PendingPkceState | null = null;
  private modal: ModalState = { open: false, tab: "accounts", pages: [] };
  private readonly listeners = new Set<() => void>();
  private readonly accountListeners = new Set<
    (resources: { readonly user?: { readonly id: string } | null }) => void
  >();
  private hydratePromise: Promise<void> | null = null;

  get isLoaded(): boolean {
    return this.loaded;
  }

  get isBusy(): boolean {
    return this.busy;
  }

  get isKeychainBacked(): boolean {
    return this.keychainBacked;
  }

  get errorMessage(): string | null {
    return this.lastError;
  }

  get modalState(): ModalState {
    return this.modal;
  }

  get accounts(): ReadonlyArray<HanzoIamAccount> {
    return this.vault.accounts;
  }

  get activeAccount(): HanzoIamAccount | null {
    return resolveActiveHanzoAccount(this.vault);
  }

  get activeOrgId(): string | null {
    return resolveActiveHanzoOrgId(this.vault);
  }

  get organizations(): ReadonlyArray<HanzoIamOrganization> {
    const active = this.activeAccount;
    if (!active) return EMPTY_ORGANIZATIONS;
    const fetched = this.organizationsByAccount.get(active.id);
    if (fetched && fetched.length > 0) return fetched;
    const fallback = resolveAccountOrganizations(active);
    const normalized = fallback.length > 0 ? fallback : EMPTY_ORGANIZATIONS;
    this.organizationsByAccount.set(active.id, normalized);
    return normalized;
  }

  get billing(): HanzoBillingSummary | null {
    return this.billingSummary;
  }

  getApiHeaders(): Record<string, string> {
    return buildHanzoApiHeaders({
      accessToken: this.activeAccount?.accessToken,
      orgId: this.activeOrgId,
    });
  }

  subscribe = (listener: () => void): (() => void) => {
    this.listeners.add(listener);
    return () => {
      this.listeners.delete(listener);
    };
  };

  addListener(
    listener: (resources: { readonly user?: { readonly id: string } | null }) => void,
  ): () => void {
    this.accountListeners.add(listener);
    if (this.loaded) {
      const active = this.activeAccount;
      listener({ user: active ? { id: active.id } : null });
    }
    return () => {
      this.accountListeners.delete(listener);
    };
  }

  private emit(): void {
    const active = this.activeAccount;
    const resources = { user: active ? { id: active.id } : null };
    for (const listener of this.accountListeners) {
      listener(resources);
    }
    for (const listener of this.listeners) {
      listener();
    }
  }

  private persistVault(next: HanzoIamVaultState): void {
    this.vault = next;
    for (const account of next.accounts) {
      if (!this.organizationsByAccount.has(account.id)) {
        const orgs = resolveAccountOrganizations(account);
        this.organizationsByAccount.set(
          account.id,
          orgs.length > 0 ? orgs : EMPTY_ORGANIZATIONS,
        );
      }
    }
    writeBrowserVault(next);
    if (typeof window !== "undefined" && window.desktopBridge?.setIamVault) {
      const payload = next.accounts.length > 0 ? serializeHanzoIamVaultJson(next) : null;
      void window.desktopBridge
        .setIamVault(payload)
        .then((saved) => {
          if (saved !== this.keychainBacked) {
            this.keychainBacked = saved;
            this.emit();
          }
        })
        .catch(() => undefined);
    }
    this.emit();
  }

  hydrate(): Promise<void> {
    if (this.hydratePromise) return this.hydratePromise;
    this.hydratePromise = (async () => {
      let initialVault = readBrowserVault();
      if (typeof window !== "undefined" && window.desktopBridge?.getIamVault) {
        try {
          const keychainRaw = await window.desktopBridge.getIamVault();
          if (keychainRaw) {
            const keychainVault = parseHanzoIamVaultJson(keychainRaw);
            if (keychainVault.accounts.length > 0) {
              initialVault = keychainVault;
              this.keychainBacked = true;
              writeBrowserVault(initialVault);
            }
          } else if (initialVault.accounts.length > 0 && window.desktopBridge.setIamVault) {
            this.keychainBacked = await window.desktopBridge.setIamVault(
              serializeHanzoIamVaultJson(initialVault),
            );
          }
        } catch {
          // Fall back to browser storage when safeStorage is unavailable
        }
      }
      this.vault = initialVault;
      await this.completePendingPkceCallback();
      await this.syncLocalCliIdentities();
      this.loaded = true;
      this.emit();
      void this.refreshOrganizationsAndBilling();
    })();
    return this.hydratePromise;
  }

  async refreshOrganizationsAndBilling(): Promise<void> {
    const active = this.activeAccount;
    if (!active) {
      this.billingSummary = null;
      this.emit();
      return;
    }
    const token = (await this.getToken()) ?? active.accessToken;
    const orgId = this.activeOrgId;
    const [orgs, billing] = await Promise.all([
      fetchHanzoIamOrganizations({ ...active, accessToken: token }).catch(() =>
        resolveAccountOrganizations(active),
      ),
      fetchHanzoBillingSummary({ accessToken: token, orgId }).catch(() => null),
    ]);
    this.organizationsByAccount.set(active.id, orgs);
    if (billing) {
      this.billingSummary = billing;
    }
    this.emit();
  }

  private async completePendingPkceCallback(): Promise<void> {
    if (typeof window === "undefined") return;
    let currentUrl: URL;
    try {
      currentUrl = new URL(window.location.href);
    } catch {
      return;
    }
    const code = currentUrl.searchParams.get("code");
    const state = currentUrl.searchParams.get("state");
    if (!code || !state) return;

    const pending = this.pendingDesktopPkce ?? readPendingPkce();
    if (!pending || pending.state !== state) return;
    this.pendingDesktopPkce = null;
    writePendingPkce(null);

    currentUrl.searchParams.delete("code");
    currentUrl.searchParams.delete("state");
    currentUrl.searchParams.delete("session_state");
    window.history.replaceState({}, "", currentUrl.toString());

    await this.finishPkceCodeExchange(pending, code);
  }

  async handleDeepLinkCallback(url: string): Promise<boolean> {
    const callback = parseHanzoIamCallbackUrl(url);
    if (!callback) return false;

    const pending = this.pendingDesktopPkce ?? readPendingPkce();
    this.pendingDesktopPkce = null;
    writePendingPkce(null);

    if (callback.error) {
      this.lastError = callback.errorDescription ?? callback.error;
      this.emit();
      return true;
    }
    if (!callback.code) {
      this.lastError = "Missing authorization code in Hanzo IAM callback.";
      this.emit();
      return true;
    }
    if (!pending || (callback.state && pending.state !== callback.state)) {
      this.lastError = "OAuth state mismatch during Hanzo IAM sign-in.";
      this.emit();
      return true;
    }

    await this.finishPkceCodeExchange(pending, callback.code);
    return true;
  }

  private async finishPkceCodeExchange(pending: PendingPkceState, code: string): Promise<void> {
    this.busy = true;
    this.lastError = null;
    this.emit();
    try {
      const account = await exchangeHanzoIamAuthorizationCode({
        issuer: pending.issuer,
        clientId: pending.clientId,
        code,
        codeVerifier: pending.verifier,
        redirectUri: pending.redirectUri,
        brand: pending.brandId,
      });
      this.persistVault(upsertHanzoIamVaultAccount(this.vault, account, true));
      void this.refreshOrganizationsAndBilling();
    } catch (error) {
      this.lastError = error instanceof Error ? error.message : String(error);
    } finally {
      this.busy = false;
      this.emit();
    }
  }

  async syncLocalCliIdentities(): Promise<void> {
    if (typeof window === "undefined" || isHostedStaticApp()) {
      return;
    }
    try {
      const url = resolvePrimaryEnvironmentHttpUrl("/v1/hanzo/identities");
      const headers: Record<string, string> = { Accept: "application/json" };
      let credentials: RequestCredentials = "omit";
      if (isSameOriginBrowserPrimary()) {
        credentials = "include";
      } else {
        const bearerToken = await readDesktopPrimaryBearerToken();
        if (bearerToken) {
          headers.Authorization = `Bearer ${bearerToken}`;
        } else {
          credentials = "include";
        }
      }

      const response = await fetch(url, { headers, credentials });
      if (!response.ok) return;
      const snapshot = (await response.json()) as LocalHanzoIdentitiesResponse;
      if (!Array.isArray(snapshot.accounts) || snapshot.accounts.length === 0) {
        return;
      }

      let nextVault = this.vault;
      const hadActive = Boolean(resolveActiveHanzoAccount(nextVault));
      for (const local of snapshot.accounts) {
        if (!local.accessToken) continue;
        const makeActive = !hadActive && snapshot.activeAccountId === local.id;
        nextVault = upsertHanzoIamVaultAccount(nextVault, local, makeActive);
      }
      if (!resolveActiveHanzoAccount(nextVault) && nextVault.accounts[0]) {
        nextVault = {
          ...nextVault,
          activeAccountId: nextVault.accounts[0].id,
          activeOrgId: nextVault.activeOrgId ?? nextVault.accounts[0].owner,
        };
      }
      this.persistVault(nextVault);
    } catch {
      // Local server may not be reachable yet during cold boot; ignore silently.
    }
  }

  openSignIn = (_options?: { readonly redirectUrl?: string }): void => {
    this.modal = {
      open: true,
      tab: "accounts",
      pages: this.modal.pages,
    };
    this.emit();
  };

  openAccountModal = (
    tab: ModalTabId = "accounts",
    pages?: ReadonlyArray<HanzoCustomAccountPage>,
  ): void => {
    this.modal = {
      open: true,
      tab,
      pages: pages ?? this.modal.pages,
    };
    this.emit();
    void this.refreshOrganizationsAndBilling();
  };

  registerCustomPages(pages: ReadonlyArray<HanzoCustomAccountPage>): void {
    this.modal = {
      ...this.modal,
      pages,
    };
  }

  closeModal = (): void => {
    if (!this.modal.open) return;
    this.modal = { ...this.modal, open: false };
    this.emit();
  };

  setModalTab = (tab: ModalTabId): void => {
    this.modal = { ...this.modal, tab };
    this.emit();
  };

  async startOAuthSignIn(options?: {
    readonly brandId?: HanzoIamBrandId;
    readonly returnUrl?: string;
  }): Promise<void> {
    if (typeof window === "undefined") return;
    const config = resolveCloudPublicConfig();
    const brandId = options?.brandId ?? "hanzo";
    const brand = HANZO_IAM_BRANDS.find((entry) => entry.id === brandId);
    const issuer = normalizeHanzoIssuer(
      brandId === "hanzo" ? config.iamIssuer : (brand?.issuer ?? config.iamIssuer),
    );
    const clientId =
      brandId === "hanzo" ? config.iamClientId : (brand?.defaultClientId ?? config.iamClientId);

    const useDesktopDeepLink = Boolean(window.desktopBridge?.openExternal);
    let redirectUri = DESKTOP_OAUTH_REDIRECT_URI;
    if (!useDesktopDeepLink) {
      const redirectUrl = new URL(window.location.href);
      redirectUrl.searchParams.delete("code");
      redirectUrl.searchParams.delete("state");
      redirectUri = redirectUrl.toString();
    }

    const verifier = createPkceVerifier();
    const codeChallenge = createPkceChallengeS256(verifier);
    const state = createOAuthState();
    const pending: PendingPkceState = {
      state,
      verifier,
      redirectUri,
      brandId,
      issuer,
      clientId,
      returnUrl: options?.returnUrl ?? redirectUri,
    };
    this.pendingDesktopPkce = pending;
    writePendingPkce(pending);
    const authorizeUrl = buildHanzoIamAuthorizeUrl({
      issuer,
      clientId,
      redirectUri,
      state,
      codeChallenge,
    });
    if (useDesktopDeepLink && window.desktopBridge?.openExternal) {
      await window.desktopBridge.openExternal(authorizeUrl);
      return;
    }
    window.location.assign(authorizeUrl);
  }

  async signInWithAccessToken(input: {
    readonly accessToken: string;
    readonly refreshToken?: string | null;
    readonly brandId?: HanzoIamBrandId;
  }): Promise<HanzoIamAccount> {
    const trimmed = input.accessToken.trim();
    if (!trimmed) {
      throw new Error("Paste a valid Hanzo IAM token or API key.");
    }
    const config = resolveCloudPublicConfig();
    const brandId = input.brandId ?? "hanzo";
    const brand = HANZO_IAM_BRANDS.find((entry) => entry.id === brandId);
    const issuer = normalizeHanzoIssuer(
      brandId === "hanzo" ? config.iamIssuer : (brand?.issuer ?? config.iamIssuer),
    );
    const clientId =
      brandId === "hanzo" ? config.iamClientId : (brand?.defaultClientId ?? config.iamClientId);

    this.busy = true;
    this.lastError = null;
    this.emit();
    try {
      const userinfo = await fetchHanzoIamUserinfo(issuer, trimmed);
      const account = buildHanzoIamAccountFromTokens({
        brand: brandId,
        issuer,
        clientId,
        accessToken: trimmed,
        refreshToken: input.refreshToken ?? null,
        userinfo,
      });
      this.persistVault(upsertHanzoIamVaultAccount(this.vault, account, true));
      void this.refreshOrganizationsAndBilling();
      return account;
    } catch (error) {
      this.lastError = error instanceof Error ? error.message : String(error);
      throw error;
    } finally {
      this.busy = false;
      this.emit();
    }
  }

  switchAccount = (accountId: string): void => {
    const target = this.vault.accounts.find((account) => account.id === accountId);
    if (!target) {
      return;
    }
    this.persistVault({
      ...this.vault,
      activeAccountId: target.id,
      activeOrgId: this.vault.orgByAccountId?.[target.id] ?? target.owner,
    });
    void this.refreshOrganizationsAndBilling();
  };

  switchOrganization = (orgId: string): void => {
    const trimmed = orgId.trim();
    if (!trimmed) return;
    this.persistVault(switchHanzoIamVaultOrg(this.vault, trimmed));
    void this.refreshOrganizationsAndBilling();
  };

  removeAccount = (accountId: string): void => {
    this.organizationsByAccount.delete(accountId);
    this.persistVault(removeHanzoIamVaultAccount(this.vault, accountId));
    void this.refreshOrganizationsAndBilling();
  };

  signOut = async (_options?: { readonly redirectUrl?: string }): Promise<void> => {
    const active = this.activeAccount;
    if (!active) return;
    this.organizationsByAccount.delete(active.id);
    this.persistVault(removeHanzoIamVaultAccount(this.vault, active.id));
    void this.refreshOrganizationsAndBilling();
  };

  getToken = async (_options?: { readonly template?: string }): Promise<string | null> => {
    const active = this.activeAccount;
    if (!active) return null;
    if (!isHanzoIamTokenExpiringSoon(active) || !active.refreshToken) {
      return active.accessToken;
    }
    try {
      const updated = await refreshHanzoIamAccount(active);
      this.persistVault(upsertHanzoIamVaultAccount(this.vault, updated, true));
      return updated.accessToken;
    } catch {
      return active.accessToken;
    }
  };

  resetForTesting(vault: HanzoIamVaultState = EMPTY_HANZO_IAM_VAULT): void {
    this.hydratePromise = Promise.resolve();
    this.vault = vault;
    this.loaded = true;
    this.busy = false;
    this.lastError = null;
    this.billingSummary = null;
    this.organizationsByAccount.clear();
    this.emit();
  }
}

let sharedController: HanzoWebAuthController | null = null;

export function getHanzoWebAuthController(): HanzoWebAuthController {
  if (!sharedController) {
    sharedController = new HanzoWebAuthController();
  }
  return sharedController;
}

const HanzoWebAuthContext = createContext<HanzoWebAuthController | null>(null);

function useController(): HanzoWebAuthController {
  const contextual = useContext(HanzoWebAuthContext);
  return contextual ?? getHanzoWebAuthController();
}

function HanzoAccountModal({ controller }: { readonly controller: HanzoWebAuthController }) {
  const gt = useGT();
  const modal = useSyncExternalStore(
    controller.subscribe,
    () => controller.modalState,
    () => controller.modalState,
  );
  const accounts = useSyncExternalStore(
    controller.subscribe,
    () => controller.accounts,
    () => controller.accounts,
  );
  const activeAccount = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeAccount,
    () => controller.activeAccount,
  );
  const activeOrgId = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeOrgId,
    () => controller.activeOrgId,
  );
  const organizations = useSyncExternalStore(
    controller.subscribe,
    () => controller.organizations,
    () => controller.organizations,
  );
  const billing = useSyncExternalStore(
    controller.subscribe,
    () => controller.billing,
    () => controller.billing,
  );
  const isBusy = useSyncExternalStore(
    controller.subscribe,
    () => controller.isBusy,
    () => false,
  );
  const isKeychainBacked = useSyncExternalStore(
    controller.subscribe,
    () => controller.isKeychainBacked,
    () => false,
  );
  const errorMessage = useSyncExternalStore(
    controller.subscribe,
    () => controller.errorMessage,
    () => null,
  );

  const [selectedBrand, setSelectedBrand] = useState<HanzoIamBrandId>("hanzo");
  const [showAddAccount, setShowAddAccount] = useState(false);
  const [showTokenEntry, setShowTokenEntry] = useState(false);
  const [tokenInput, setTokenInput] = useState("");
  const [customOrgInput, setCustomOrgInput] = useState("");
  const [localError, setLocalError] = useState<string | null>(null);

  if (!modal.open) return null;

  const activeCustomPage = modal.pages.find((page) => page.url === modal.tab) ?? null;
  const shouldShowSignInForm = accounts.length === 0 || showAddAccount;
  const currentPlanId = billing?.planId ?? "go";
  const spendRatio =
    billing?.monthlyLimitCents && billing.monthlyLimitCents > 0
      ? Math.min(100, Math.round((billing.monthlyUsedCents / billing.monthlyLimitCents) * 100))
      : 18;

  const handleTokenSubmit = async () => {
    setLocalError(null);
    try {
      await controller.signInWithAccessToken({
        accessToken: tokenInput,
        brandId: selectedBrand,
      });
      setTokenInput("");
      setShowTokenEntry(false);
      setShowAddAccount(false);
    } catch (error) {
      setLocalError(error instanceof Error ? error.message : String(error));
    }
  };

  const handleCustomOrgSwitch = () => {
    const slug = customOrgInput.trim();
    if (!slug) return;
    controller.switchOrganization(slug);
    setCustomOrgInput("");
  };

  return (
    <div
      role="dialog"
      aria-modal="true"
      aria-label={gt("Hanzo Account & Organization Control Center")}
      data-testid="hanzo-account-modal"
      className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 p-4 backdrop-blur-xs"
      onClick={(event) => {
        if (event.target === event.currentTarget) {
          controller.closeModal();
        }
      }}
    >
      <div className="flex max-h-[86vh] w-full max-w-3xl flex-col overflow-hidden rounded-2xl border border-border bg-background text-foreground shadow-2xl">
        <div className="flex items-center justify-between border-b border-border px-5 py-3.5">
          <div className="flex items-center gap-2.5">
            <div className="flex size-7 items-center justify-center rounded-md bg-foreground text-xs font-bold text-background">
              H
            </div>
            <div>
              <div className="text-sm font-semibold">
                <T>Hanzo Account &amp; Organization</T>
              </div>
              <div className="text-xs text-muted-foreground">
                <T>Unified IAM (hanzo.id) · Organization context · Billing &amp; surfaces</T>
              </div>
            </div>
          </div>
          <button
            type="button"
            onClick={controller.closeModal}
            className="rounded-lg p-1.5 text-muted-foreground hover:bg-muted hover:text-foreground"
            aria-label={gt("Close")}
          >
            <XIcon className="size-4" />
          </button>
        </div>

        <div className="flex overflow-x-auto border-b border-border bg-muted/30 px-4">
          <button
            type="button"
            onClick={() => controller.setModalTab("accounts")}
            className={`shrink-0 border-b-2 px-3 py-2.5 text-xs font-medium transition-colors ${
              modal.tab === "accounts"
                ? "border-foreground text-foreground"
                : "border-transparent text-muted-foreground hover:text-foreground"
            }`}
          >
            <T>Account &amp; Org</T> ({accounts.length})
          </button>
          <button
            type="button"
            onClick={() => controller.setModalTab("billing")}
            className={`flex shrink-0 items-center gap-1.5 border-b-2 px-3 py-2.5 text-xs font-medium transition-colors ${
              modal.tab === "billing"
                ? "border-foreground text-foreground"
                : "border-transparent text-muted-foreground hover:text-foreground"
            }`}
          >
            <CreditCardIcon className="size-3.5" />
            <span>
              <T>Plans &amp; Usage</T>
            </span>
          </button>
          <button
            type="button"
            onClick={() => controller.setModalTab("surfaces")}
            className={`flex shrink-0 items-center gap-1.5 border-b-2 px-3 py-2.5 text-xs font-medium transition-colors ${
              modal.tab === "surfaces"
                ? "border-foreground text-foreground"
                : "border-transparent text-muted-foreground hover:text-foreground"
            }`}
          >
            <GlobeIcon className="size-3.5" />
            <span>
              <T>Surfaces &amp; Access</T>
            </span>
          </button>
          {activeAccount &&
            modal.pages.map((page) => (
              <button
                key={page.url}
                type="button"
                onClick={() => controller.setModalTab(page.url)}
                className={`flex shrink-0 items-center gap-1.5 border-b-2 px-3 py-2.5 text-xs font-medium transition-colors ${
                  modal.tab === page.url
                    ? "border-foreground text-foreground"
                    : "border-transparent text-muted-foreground hover:text-foreground"
                }`}
              >
                {page.icon}
                <span>{page.label}</span>
              </button>
            ))}
        </div>

        <div className="flex-1 overflow-y-auto p-5">
          {activeCustomPage ? (
            activeCustomPage.content
          ) : modal.tab === "billing" ? (
            <div className="space-y-5">
              <div className="rounded-xl border border-border bg-muted/20 p-4">
                <div className="flex flex-wrap items-center justify-between gap-3">
                  <div>
                    <div className="text-xs font-semibold tracking-wider text-muted-foreground uppercase">
                      <T>Current subscription &amp; credit meter</T>
                    </div>
                    <div className="mt-1 flex items-center gap-2 text-base font-semibold">
                      <span>{billing?.planName ?? "Hanzo Dev"}</span>
                      {activeOrgId ? (
                        <span className="rounded-full bg-muted px-2 py-0.5 font-mono text-[11px] text-muted-foreground">
                          X-Org-Id: {activeOrgId}
                        </span>
                      ) : null}
                    </div>
                  </div>
                  <button
                    type="button"
                    onClick={() =>
                      openExternalUrl(
                        buildHanzoSurfaceLaunchUrl(HANZO_BILLING_PORTAL_URL, {
                          orgId: activeOrgId,
                        }),
                      )
                    }
                    className="inline-flex items-center gap-1.5 rounded-lg bg-foreground px-3.5 py-2 text-xs font-semibold text-background hover:opacity-90"
                  >
                    <span>
                      <T>Open Billing Portal</T>
                    </span>
                    <ExternalLinkIcon className="size-3.5" />
                  </button>
                </div>

                <div className="mt-4 grid grid-cols-1 gap-3 sm:grid-cols-3">
                  <div className="rounded-lg border border-border bg-background p-3">
                    <div className="text-[11px] text-muted-foreground">
                      <T>Credit balance</T>
                    </div>
                    <div className="mt-0.5 font-mono text-sm font-semibold">
                      {billing ? formatUsdCents(billing.creditBalanceCents) : "$25.00"}
                    </div>
                  </div>
                  <div className="rounded-lg border border-border bg-background p-3">
                    <div className="text-[11px] text-muted-foreground">
                      <T>Monthly AI usage</T>
                    </div>
                    <div className="mt-0.5 font-mono text-sm font-semibold">
                      {billing ? formatUsdCents(billing.monthlyUsedCents) : "$0.00"}
                    </div>
                  </div>
                  <div className="rounded-lg border border-border bg-background p-3">
                    <div className="text-[11px] text-muted-foreground">
                      <T>Billing endpoint</T>
                    </div>
                    <div className="mt-0.5 font-mono text-xs font-medium text-muted-foreground">
                      billing.hanzo.ai
                    </div>
                  </div>
                </div>

                <div className="mt-3 space-y-1">
                  <div className="flex justify-between text-[11px] text-muted-foreground">
                    <span>
                      <T>Cycle allowance utilization</T>
                    </span>
                    <span>{spendRatio}%</span>
                  </div>
                  <div className="h-1.5 w-full overflow-hidden rounded-full bg-muted">
                    <div
                      className="h-full bg-foreground transition-all"
                      style={{ width: `${spendRatio}%` }}
                    />
                  </div>
                </div>
              </div>

              <div className="space-y-2.5">
                <div className="text-xs font-semibold tracking-wider text-muted-foreground uppercase">
                  <T>Hanzo unified subscription plans</T>
                </div>
                <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
                  {HANZO_SUBSCRIPTION_PLANS.filter((plan) => plan.id !== "free").map((plan) => {
                    const isCurrent = currentPlanId === plan.id;
                    return (
                      <div
                        key={plan.id}
                        className={`flex flex-col justify-between rounded-xl border p-4 ${
                          isCurrent ? "border-foreground bg-muted/20" : "border-border bg-background"
                        }`}
                      >
                        <div>
                          <div className="flex items-center justify-between gap-2">
                            <span className="text-sm font-semibold">{plan.name}</span>
                            <span className="rounded-full bg-muted px-2.5 py-0.5 font-mono text-xs font-semibold">
                              {plan.priceLabel}
                            </span>
                          </div>
                          <p className="mt-1 text-xs text-muted-foreground">{plan.summary}</p>
                          <div className="mt-2.5 flex items-center gap-1.5 text-xs text-muted-foreground">
                            <CheckIcon className="size-3 shrink-0 text-emerald-500" />
                            <span>
                              {gt("{allowance} monthly AI credit allowance", {
                                allowance: formatUsdCents(plan.monthlyAllowanceCents),
                              })}
                            </span>
                          </div>
                        </div>
                        <button
                          type="button"
                          onClick={() =>
                            openExternalUrl(
                              buildHanzoSurfaceLaunchUrl(
                                `${HANZO_BILLING_PORTAL_URL}?plan=${plan.id}`,
                                { orgId: activeOrgId },
                              ),
                            )
                          }
                          className="mt-3.5 inline-flex w-full items-center justify-center gap-1.5 rounded-lg border border-border px-3 py-1.5 text-xs font-semibold hover:bg-muted"
                        >
                          <span>{isCurrent ? gt("Manage plan") : gt("Select plan")}</span>
                          <ExternalLinkIcon className="size-3" />
                        </button>
                      </div>
                    );
                  })}
                </div>
              </div>
            </div>
          ) : modal.tab === "surfaces" ? (
            <div className="space-y-5">
              <div className="rounded-xl border border-border bg-muted/20 p-4">
                <div className="flex flex-wrap items-center justify-between gap-3">
                  <div className="flex items-center gap-2.5">
                    <CpuIcon className="size-4 text-foreground" />
                    <div>
                      <div className="text-sm font-semibold">
                        <T>Local AI, Validator Node &amp; Embedded Engine</T>
                      </div>
                      <div className="text-xs text-muted-foreground">
                        <T>
                          Manage local Qwen3+ models (:11434), Hanzo Engine (:36900), and hanzod (:9630)
                        </T>
                      </div>
                    </div>
                  </div>
                  <button
                    type="button"
                    onClick={() => {
                      controller.closeModal();
                      openLocalAiManager("models");
                    }}
                    className="rounded-lg border border-border bg-background px-3 py-1.5 text-xs font-semibold hover:bg-muted"
                  >
                    <T>Open Local AI Manager</T>
                  </button>
                </div>
              </div>

              <div className="space-y-2.5">
                <div className="flex items-center justify-between">
                  <span className="text-xs font-semibold tracking-wider text-muted-foreground uppercase">
                    <T>Connected Hanzo ecosystem surfaces</T>
                  </span>
                  {activeOrgId ? (
                    <span className="font-mono text-[11px] text-muted-foreground">
                      ?org={activeOrgId}
                    </span>
                  ) : null}
                </div>
                <div className="grid grid-cols-1 gap-2.5 sm:grid-cols-2">
                  {HANZO_ECOSYSTEM_SURFACES.map((surface) => (
                    <button
                      key={surface.id}
                      type="button"
                      onClick={() =>
                        openExternalUrl(
                          buildHanzoSurfaceLaunchUrl(surface.url, { orgId: activeOrgId }),
                        )
                      }
                      className="flex items-start justify-between gap-3 rounded-xl border border-border bg-background p-3.5 text-left transition-colors hover:bg-muted/40"
                    >
                      <div className="min-w-0 flex-1">
                        <div className="text-sm font-semibold">{surface.label}</div>
                        <div className="mt-0.5 text-xs text-muted-foreground">
                          {surface.description}
                        </div>
                        <div className="mt-1.5 truncate font-mono text-[11px] text-muted-foreground">
                          {surface.url.replace("https://", "")}
                        </div>
                      </div>
                      <ExternalLinkIcon className="mt-0.5 size-3.5 shrink-0 text-muted-foreground" />
                    </button>
                  ))}
                </div>
              </div>
            </div>
          ) : (
            <div className="space-y-5">
              {activeAccount && (
                <div className="space-y-3 rounded-xl border border-border bg-muted/20 p-4">
                  <div className="flex flex-wrap items-center justify-between gap-3">
                    <div className="flex items-center gap-3">
                      {activeAccount.avatarUrl ? (
                        <img
                          src={activeAccount.avatarUrl}
                          alt={activeAccount.displayName ?? activeAccount.id}
                          className="size-10 rounded-full object-cover"
                        />
                      ) : (
                        <div className="flex size-10 items-center justify-center rounded-full bg-foreground text-sm font-bold text-background uppercase">
                          {(activeAccount.displayName ?? activeAccount.email ?? "H").slice(0, 2)}
                        </div>
                      )}
                      <div>
                        <div className="flex flex-wrap items-center gap-2">
                          <span className="text-sm font-semibold">
                            {activeAccount.displayName ?? activeAccount.email ?? activeAccount.id}
                          </span>
                          <span className="rounded-full bg-muted px-2 py-0.5 font-mono text-[11px] text-muted-foreground">
                            {activeAccount.id}
                          </span>
                          {isKeychainBacked && (
                            <span className="inline-flex items-center gap-1 rounded-full bg-emerald-500/15 px-2 py-0.5 text-[10px] font-medium text-emerald-500">
                              <ShieldCheckIcon className="size-3" />
                              <T>OS Keychain</T>
                            </span>
                          )}
                        </div>
                        <div className="text-xs text-muted-foreground">
                          {activeAccount.email
                            ? `${activeAccount.email} · ${activeAccount.issuer.replace("https://", "")}`
                            : activeAccount.issuer.replace("https://", "")}
                        </div>
                      </div>
                    </div>
                    <button
                      type="button"
                      onClick={() => void controller.refreshOrganizationsAndBilling()}
                      className="inline-flex items-center gap-1 rounded-md border border-border bg-background px-2.5 py-1 text-xs font-medium hover:bg-muted"
                    >
                      <RefreshCwIcon className="size-3" />
                      <span>
                        <T>Refresh</T>
                      </span>
                    </button>
                  </div>

                  <div className="border-t border-border pt-3">
                    <div className="flex items-center justify-between gap-2">
                      <div className="flex items-center gap-1.5 text-xs font-semibold">
                        <Building2Icon className="size-3.5 text-muted-foreground" />
                        <span>
                          <T>Active Organization (X-Org-Id)</T>
                        </span>
                      </div>
                      {activeOrgId ? (
                        <span className="font-mono text-xs text-muted-foreground">
                          X-Org-Id: {activeOrgId}
                        </span>
                      ) : null}
                    </div>
                    <div className="mt-2 flex flex-wrap gap-1.5">
                      {organizations.map((org) => {
                        const selected = org.id === activeOrgId;
                        return (
                          <button
                            key={org.id}
                            type="button"
                            onClick={() => controller.switchOrganization(org.id)}
                            className={`inline-flex items-center gap-1.5 rounded-full border px-3 py-1 text-xs font-medium transition-colors ${
                              selected
                                ? "border-foreground bg-foreground text-background"
                                : "border-border bg-background text-muted-foreground hover:text-foreground"
                            }`}
                          >
                            {selected ? <CheckIcon className="size-3" /> : null}
                            <span>{org.displayName}</span>
                            <span className="font-mono text-[10px] opacity-75">({org.id})</span>
                          </button>
                        );
                      })}
                    </div>
                    <div className="mt-2.5 flex gap-2">
                      <input
                        type="text"
                        value={customOrgInput}
                        onChange={(event) => setCustomOrgInput(event.currentTarget.value)}
                        placeholder={gt("Switch to organization slug (e.g. hanzo, lux, zoo)...")}
                        aria-label={gt("Custom organization slug")}
                        className="flex-1 rounded-lg border border-border bg-background px-3 py-1.5 font-mono text-xs focus:outline-hidden"
                      />
                      <button
                        type="button"
                        disabled={!customOrgInput.trim()}
                        onClick={handleCustomOrgSwitch}
                        className="rounded-lg border border-border bg-background px-3 py-1.5 text-xs font-medium hover:bg-muted disabled:opacity-50"
                      >
                        <T>Switch Org</T>
                      </button>
                    </div>
                  </div>
                </div>
              )}

              {accounts.length > 0 && (
                <div className="space-y-2.5">
                  <div className="flex items-center justify-between">
                    <span className="text-xs font-semibold tracking-wider text-muted-foreground uppercase">
                      <T>Signed-in accounts</T>
                    </span>
                    <div className="flex items-center gap-2">
                      {!isHostedStaticApp() && (
                        <button
                          type="button"
                          onClick={() => void controller.syncLocalCliIdentities()}
                          className="inline-flex items-center gap-1 rounded-md border border-border px-2 py-1 text-xs text-muted-foreground hover:bg-muted hover:text-foreground"
                        >
                          <RefreshCwIcon className="size-3" />
                          <span>
                            <T>Sync CLI</T>
                          </span>
                        </button>
                      )}
                      <button
                        type="button"
                        onClick={() => setShowAddAccount((prev) => !prev)}
                        className="inline-flex items-center gap-1 rounded-md border border-border px-2 py-1 text-xs font-medium hover:bg-muted"
                      >
                        <PlusIcon className="size-3" />
                        <span>
                          <T>Add account</T>
                        </span>
                      </button>
                    </div>
                  </div>

                  <div className="divide-y divide-border rounded-xl border border-border">
                    {accounts.map((account) => {
                      const isActive = account.id === activeAccount?.id;
                      return (
                        <div
                          key={account.id}
                          className="flex items-center justify-between gap-3 px-3.5 py-2.5"
                        >
                          <button
                            type="button"
                            onClick={() => controller.switchAccount(account.id)}
                            className="flex min-w-0 flex-1 items-center gap-3 text-left"
                          >
                            <div className="flex size-8 shrink-0 items-center justify-center rounded-full bg-muted text-xs font-semibold uppercase">
                              {(account.displayName ?? account.email ?? "H").slice(0, 2)}
                            </div>
                            <div className="min-w-0 flex-1">
                              <div className="flex items-center gap-2 truncate text-sm font-medium">
                                <span className="truncate">
                                  {account.displayName ?? account.email ?? account.id}
                                </span>
                                {account.owner && (
                                  <span className="rounded-full bg-muted px-2 py-0.5 text-[10px] font-medium text-muted-foreground">
                                    {account.owner}
                                  </span>
                                )}
                                {isActive && (
                                  <span className="inline-flex items-center gap-1 rounded-full bg-emerald-500/15 px-2 py-0.5 text-[10px] font-medium text-emerald-500">
                                    <CheckIcon className="size-2.5" />
                                    <T>Active</T>
                                  </span>
                                )}
                              </div>
                              <div className="truncate text-xs text-muted-foreground">
                                {account.email
                                  ? `${account.email} · ${account.issuer.replace("https://", "")}`
                                  : account.issuer.replace("https://", "")}
                              </div>
                            </div>
                          </button>
                          <div className="flex items-center gap-1.5">
                            {!isActive && (
                              <button
                                type="button"
                                onClick={() => controller.switchAccount(account.id)}
                                className="rounded-md border border-border px-2.5 py-1 text-xs font-medium hover:bg-muted"
                              >
                                <T>Switch</T>
                              </button>
                            )}
                            <button
                              type="button"
                              onClick={() => controller.removeAccount(account.id)}
                              className="rounded-md p-1.5 text-muted-foreground hover:bg-destructive/10 hover:text-destructive"
                              aria-label={gt("Remove account")}
                            >
                              {isActive ? (
                                <LogOutIcon className="size-3.5" />
                              ) : (
                                <Trash2Icon className="size-3.5" />
                              )}
                            </button>
                          </div>
                        </div>
                      );
                    })}
                  </div>
                </div>
              )}

              {shouldShowSignInForm && (
                <div className="space-y-4 rounded-xl border border-border bg-muted/20 p-4">
                  <div>
                    <div className="text-sm font-semibold">
                      {accounts.length > 0 ? (
                        <T>Add another IAM account</T>
                      ) : (
                        <T>Sign in with Hanzo IAM</T>
                      )}
                    </div>
                    <div className="mt-0.5 text-xs text-muted-foreground">
                      <T>
                        Connect Hanzo, Lux, Zoo, or Pars accounts for unified access across desktop,
                        web, and mobile.
                      </T>
                    </div>
                  </div>

                  <div className="flex flex-wrap gap-1.5">
                    {HANZO_IAM_BRANDS.map((brand) => {
                      const active = brand.id === selectedBrand;
                      return (
                        <button
                          key={brand.id}
                          type="button"
                          onClick={() => setSelectedBrand(brand.id)}
                          className={`rounded-full border px-3 py-1 text-xs font-medium transition-colors ${
                            active
                              ? "border-foreground bg-foreground text-background"
                              : "border-border bg-background text-muted-foreground hover:text-foreground"
                          }`}
                        >
                          {brand.label} ({brand.issuer.replace("https://", "")})
                        </button>
                      );
                    })}
                  </div>

                  {(localError || errorMessage) && (
                    <div className="rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-xs text-destructive">
                      {localError ?? errorMessage}
                    </div>
                  )}

                  <div className="flex flex-wrap items-center gap-2">
                    <button
                      type="button"
                      disabled={isBusy}
                      onClick={() => void controller.startOAuthSignIn({ brandId: selectedBrand })}
                      className="inline-flex items-center justify-center rounded-lg bg-foreground px-4 py-2 text-xs font-semibold text-background hover:opacity-90 disabled:opacity-50"
                    >
                      {gt("Continue with {brand}", {
                        brand:
                          HANZO_IAM_BRANDS.find((b) => b.id === selectedBrand)?.label ?? "Hanzo",
                      })}
                    </button>
                    <button
                      type="button"
                      onClick={() => setShowTokenEntry((prev) => !prev)}
                      className="inline-flex items-center gap-1.5 rounded-lg border border-border bg-background px-3 py-2 text-xs font-medium hover:bg-muted"
                    >
                      <KeyRoundIcon className="size-3.5" />
                      <span>
                        {showTokenEntry ? <T>Hide token sign-in</T> : <T>Use CLI / API token</T>}
                      </span>
                    </button>
                  </div>

                  {showTokenEntry && (
                    <div className="space-y-2 pt-1">
                      <input
                        type="password"
                        value={tokenInput}
                        onChange={(event) => setTokenInput(event.target.value)}
                        placeholder={gt("Paste token from dev token or hanzo iam token")}
                        aria-label={gt("Hanzo IAM access token")}
                        className="w-full rounded-lg border border-border bg-background px-3 py-2 font-mono text-xs focus:outline-hidden"
                      />
                      <button
                        type="button"
                        disabled={isBusy || !tokenInput.trim()}
                        onClick={() => void handleTokenSubmit()}
                        className="rounded-lg border border-border bg-foreground px-3 py-1.5 text-xs font-semibold text-background disabled:opacity-50"
                      >
                        <T>Save token account</T>
                      </button>
                    </div>
                  )}
                </div>
              )}
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

export function HanzoManagedAuthShell({ children }: { readonly children: ReactNode }) {
  const [controller] = useState(getHanzoWebAuthController);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- hydrates persisted IAM vault, OS Keychain tokens, and local CLI accounts once on startup
  useEffect(() => {
    void controller.hydrate();
  }, [controller]);

  return (
    <HanzoWebAuthContext.Provider value={controller}>
      <ManagedRelayAuthProvider>{children}</ManagedRelayAuthProvider>
      <HanzoAccountModal controller={controller} />
    </HanzoWebAuthContext.Provider>
  );
}

export function useAuth(_options?: { readonly treatPendingAsSignedOut?: boolean }) {
  const controller = useController();
  const isLoaded = useSyncExternalStore(
    controller.subscribe,
    () => controller.isLoaded,
    () => false,
  );
  const activeAccount = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeAccount,
    () => null,
  );
  const activeOrgId = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeOrgId,
    () => null,
  );

  return {
    isLoaded,
    isSignedIn: Boolean(activeAccount),
    userId: activeAccount?.id ?? null,
    orgId: activeOrgId,
    getToken: controller.getToken,
    signOut: controller.signOut,
  };
}

export function useUser() {
  const controller = useController();
  const isLoaded = useSyncExternalStore(
    controller.subscribe,
    () => controller.isLoaded,
    () => false,
  );
  const activeAccount = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeAccount,
    () => null,
  );

  const user = activeAccount
    ? {
        id: activeAccount.id,
        fullName: activeAccount.displayName,
        username: activeAccount.subject,
        imageUrl: activeAccount.avatarUrl ?? "",
        primaryEmailAddress: activeAccount.email ? { emailAddress: activeAccount.email } : null,
        emailAddresses: activeAccount.email ? [{ emailAddress: activeAccount.email }] : [],
      }
    : null;

  return {
    isLoaded,
    isSignedIn: Boolean(activeAccount),
    user,
  };
}

export function useClerk() {
  const controller = useController();
  const isLoaded = useSyncExternalStore(
    controller.subscribe,
    () => controller.isLoaded,
    () => false,
  );
  const activeAccount = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeAccount,
    () => null,
  );
  const activeAccountId = activeAccount?.id ?? null;

  return {
    status: "ready" as "ready" | "error" | "loading",
    loaded: isLoaded,
    user: activeAccountId ? { id: activeAccountId } : null,
    session: activeAccountId
      ? {
          id: activeAccountId,
          getToken: controller.getToken,
        }
      : null,
    addListener: (
      listener: (resources: { readonly user?: { readonly id: string } | null }) => void,
    ) => controller.addListener(listener),
    openSignIn: (props?: {
      readonly fallbackRedirectUrl?: string | undefined;
      readonly forceRedirectUrl?: string | undefined;
      readonly signUpFallbackRedirectUrl?: string | undefined;
      readonly signUpForceRedirectUrl?: string | undefined;
    }) => {
      const targetUrl = props?.forceRedirectUrl ?? props?.fallbackRedirectUrl;
      controller.openSignIn(targetUrl ? { redirectUrl: targetUrl } : undefined);
    },
    openUserProfile: (props?: { readonly __experimental_startPath?: string }) => {
      const startTab = props?.__experimental_startPath?.replace(/^\//u, "") || "accounts";
      controller.openAccountModal(startTab);
    },
    signOut: controller.signOut,
  };
}

export function useHanzoWebAccounts() {
  const controller = useController();
  const accounts = useSyncExternalStore(
    controller.subscribe,
    () => controller.accounts,
    () => EMPTY_ACCOUNTS,
  );
  const activeAccount = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeAccount,
    () => null,
  );
  const activeOrgId = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeOrgId,
    () => null,
  );
  const organizations = useSyncExternalStore(
    controller.subscribe,
    () => controller.organizations,
    () => EMPTY_ORGANIZATIONS,
  );
  const billing = useSyncExternalStore(
    controller.subscribe,
    () => controller.billing,
    () => null,
  );

  return {
    accounts,
    activeAccount,
    activeOrgId,
    organizations,
    billing,
    switchAccount: controller.switchAccount,
    switchOrganization: controller.switchOrganization,
    removeAccount: controller.removeAccount,
    openModal: (tab: ModalTabId = "accounts", pages?: ReadonlyArray<HanzoCustomAccountPage>) => {
      controller.openAccountModal(tab, pages);
    },
    registerPages: (pages: ReadonlyArray<HanzoCustomAccountPage>) => {
      controller.registerCustomPages(pages);
    },
    syncLocalCliIdentities: () => controller.syncLocalCliIdentities(),
    refreshOrganizationsAndBilling: () => controller.refreshOrganizationsAndBilling(),
  };
}
