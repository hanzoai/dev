import * as SecureStore from "expo-secure-store";
import * as WebBrowser from "expo-web-browser";
import type { CloudAccountSource } from "@hanzodev/client-runtime/relay";
import {
  buildHanzoApiHeaders,
  buildHanzoIamAccountFromTokens,
  buildHanzoIamAuthorizeUrl,
  resolveIamEndpoints,
  createOAuthState,
  createPkceChallengeS256,
  createPkceVerifier,
  EMPTY_HANZO_IAM_VAULT,
  exchangeHanzoIamAuthorizationCode,
  fetchHanzoBillingSummary,
  fetchHanzoIamOrganizations,
  fetchHanzoIamUserinfo,
  getHanzoIamBrand,
  HANZO_IAM_BRANDS,
  type HanzoBillingSummary,
  type HanzoIamAccount,
  type HanzoIamBrandId,
  type HanzoIamOrganization,
  type HanzoIamVaultState,
  isHanzoIamTokenExpiringSoon,
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
import { useSyncExternalStore } from "react";

WebBrowser.maybeCompleteAuthSession();

const HANZO_IAM_REDIRECT_URI = "hanzo://oauth/callback";
const VAULT_STORAGE_KEY = "hanzo.dev.iam.vault.v1";

interface PendingMobilePkce {
  readonly state: string;
  readonly verifier: string;
  readonly redirectUri: string;
  readonly brandId: HanzoIamBrandId;
  readonly issuer: string;
  readonly clientId: string;
}

export interface HanzoMobileAuthSnapshot {
  readonly isLoaded: boolean;
  readonly activeAccount: HanzoIamAccount | null;
  readonly accounts: ReadonlyArray<HanzoIamAccount>;
  readonly activeOrgId: string | null;
  readonly organizations: ReadonlyArray<HanzoIamOrganization>;
  readonly billing: HanzoBillingSummary | null;
}

type AuthListener = () => void;
type CloudSourceListener = Parameters<CloudAccountSource["addListener"]>[0];

class HanzoMobileAuthController implements CloudAccountSource {
  private vault: HanzoIamVaultState = EMPTY_HANZO_IAM_VAULT;
  private organizationsByAccount = new Map<string, ReadonlyArray<HanzoIamOrganization>>();
  private billingSummary: HanzoBillingSummary | null = null;
  private pendingPkce: PendingMobilePkce | null = null;
  private snapshot: HanzoMobileAuthSnapshot = {
    isLoaded: false,
    activeAccount: null,
    accounts: [],
    activeOrgId: null,
    organizations: [],
    billing: null,
  };
  private readonly listeners = new Set<AuthListener>();
  private readonly cloudListeners = new Set<CloudSourceListener>();
  private hydratePromise: Promise<void> | null = null;
  private refreshInFlight = new Map<string, Promise<HanzoIamAccount>>();

  readonly getSnapshot = (): HanzoMobileAuthSnapshot => this.snapshot;

  readonly subscribe = (listener: AuthListener): (() => void) => {
    this.listeners.add(listener);
    return () => {
      this.listeners.delete(listener);
    };
  };

  readonly addListener: CloudAccountSource["addListener"] = (listener) => {
    this.cloudListeners.add(listener);
    if (this.snapshot.isLoaded) {
      listener({
        user: this.snapshot.activeAccount ? { id: this.snapshot.activeAccount.id } : null,
      });
    }
    return () => {
      this.cloudListeners.delete(listener);
    };
  };

  private buildSnapshot(isLoaded: boolean): HanzoMobileAuthSnapshot {
    const activeAccount = resolveActiveHanzoAccount(this.vault);
    const activeOrgId = resolveActiveHanzoOrgId(this.vault);
    const organizations = activeAccount
      ? (this.organizationsByAccount.get(activeAccount.id) ??
        resolveAccountOrganizations(activeAccount))
      : [];
    return {
      isLoaded,
      activeAccount,
      accounts: this.vault.accounts,
      activeOrgId,
      organizations,
      billing: activeAccount ? this.billingSummary : null,
    };
  }

  private emit(isLoaded = true): void {
    const next = this.buildSnapshot(isLoaded);
    this.snapshot = next;
    for (const listener of this.listeners) {
      listener();
    }
    if (next.isLoaded) {
      const resource = {
        user: next.activeAccount ? { id: next.activeAccount.id } : null,
      };
      for (const cloudListener of this.cloudListeners) {
        cloudListener(resource);
      }
    }
  }

  private async persistVault(vault: HanzoIamVaultState): Promise<void> {
    const activeAccount = resolveActiveHanzoAccount(vault);
    const normalized: HanzoIamVaultState = {
      activeAccountId: activeAccount?.id ?? null,
      accounts: vault.accounts,
      activeOrgId: resolveActiveHanzoOrgId(vault),
      orgByAccountId: vault.orgByAccountId ?? {},
    };
    this.vault = normalized;
    try {
      if (normalized.accounts.length === 0) {
        await SecureStore.deleteItemAsync(VAULT_STORAGE_KEY);
      } else {
        await SecureStore.setItemAsync(VAULT_STORAGE_KEY, serializeHanzoIamVaultJson(normalized));
      }
    } catch {
      // In-memory state still updates in simulator/test environments without keychain entitlements.
    }
    this.emit(true);
  }

  readonly hydrate = (): Promise<void> => {
    if (this.hydratePromise) return this.hydratePromise;
    this.hydratePromise = (async () => {
      try {
        const raw = await SecureStore.getItemAsync(VAULT_STORAGE_KEY);
        this.vault = parseHanzoIamVaultJson(raw);
        this.emit(true);
        if (this.vault.accounts.length > 0) {
          void this.refreshOrganizationsAndBilling();
        }
      } catch {
        this.vault = EMPTY_HANZO_IAM_VAULT;
        this.emit(true);
      }
    })();
    return this.hydratePromise;
  };

  readonly getApiHeaders = (): Record<string, string> =>
    buildHanzoApiHeaders({
      accessToken: this.snapshot.activeAccount?.accessToken,
      orgId: this.snapshot.activeOrgId,
    });

  readonly refreshOrganizationsAndBilling = async (): Promise<void> => {
    const active = this.snapshot.activeAccount;
    if (!active) {
      this.billingSummary = null;
      this.emit(true);
      return;
    }
    const token = (await this.getToken(active.id)) ?? active.accessToken;
    const orgId = this.snapshot.activeOrgId;
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
    this.emit(true);
  };

  readonly getToken = async (accountId?: string): Promise<string | null> => {
    await this.hydrate();
    const target = accountId
      ? (this.snapshot.accounts.find((entry) => entry.id === accountId) ?? null)
      : this.snapshot.activeAccount;
    if (!target) return null;
    if (!isHanzoIamTokenExpiringSoon(target) || !target.refreshToken) {
      return target.accessToken;
    }
    let pending = this.refreshInFlight.get(target.id);
    if (!pending) {
      pending = refreshHanzoIamAccount(target).finally(() => {
        this.refreshInFlight.delete(target.id);
      });
      this.refreshInFlight.set(target.id, pending);
    }
    try {
      const refreshed = await pending;
      const nextVault = upsertHanzoIamVaultAccount(
        this.vault,
        refreshed,
        this.snapshot.activeAccount?.id === target.id,
      );
      await this.persistVault(nextVault);
      return refreshed.accessToken;
    } catch {
      return target.accessToken;
    }
  };

  readonly handleDeepLinkCallback = async (url: string): Promise<boolean> => {
    const callback = parseHanzoIamCallbackUrl(url);
    if (!callback) return false;
    const pending = this.pendingPkce;
    this.pendingPkce = null;
    if (!pending || callback.error || !callback.code) {
      return true;
    }
    if (callback.state && callback.state !== pending.state) {
      return true;
    }
    const account = await exchangeHanzoIamAuthorizationCode({
      brand: pending.brandId,
      issuer: pending.issuer,
      clientId: pending.clientId,
      code: callback.code,
      redirectUri: pending.redirectUri,
      codeVerifier: pending.verifier,
    });
    await this.persistVault(upsertHanzoIamVaultAccount(this.vault, account, true));
    void this.refreshOrganizationsAndBilling();
    return true;
  };

  readonly signInWithBrand = async (
    brandId: HanzoIamBrandId = "hanzo",
  ): Promise<HanzoIamAccount> => {
    await this.hydrate();
    const brand = getHanzoIamBrand(brandId);
    // The hanzo-app IAM client registers `hanzo://oauth/callback`; the auth session intercepts that scheme.
    const redirectUri = HANZO_IAM_REDIRECT_URI;
    const verifier = createPkceVerifier();
    const challenge = createPkceChallengeS256(verifier);
    const state = createOAuthState();
    this.pendingPkce = {
      state,
      verifier,
      redirectUri,
      brandId: brand.id,
      issuer: brand.issuer,
      clientId: brand.clientId,
    };
    const authUrl = buildHanzoIamAuthorizeUrl({
      authorizationEndpoint: (await resolveIamEndpoints(brand.issuer)).authorize,
      issuer: brand.issuer,
      clientId: brand.clientId,
      redirectUri,
      state,
      codeChallenge: challenge,
    });

    const result = await WebBrowser.openAuthSessionAsync(authUrl, redirectUri);
    this.pendingPkce = null;
    if (result.type !== "success" || !result.url) {
      throw new Error("Sign-in was cancelled.");
    }
    const callback = parseHanzoIamCallbackUrl(result.url);
    if (!callback) {
      throw new Error("Unrecognized OAuth callback URL during Hanzo ID sign-in.");
    }
    if (callback.state !== state) {
      throw new Error("OAuth state mismatch during Hanzo ID sign-in.");
    }
    if (!callback.code) {
      throw new Error(callback.errorDescription ?? callback.error ?? "Missing authorization code.");
    }

    const account = await exchangeHanzoIamAuthorizationCode({
      brand: brand.id,
      issuer: brand.issuer,
      clientId: brand.clientId,
      code: callback.code,
      redirectUri,
      codeVerifier: verifier,
    });

    await this.persistVault(upsertHanzoIamVaultAccount(this.vault, account, true));
    void this.refreshOrganizationsAndBilling();
    return account;
  };

  readonly signInWithToken = async (input: {
    readonly token: string;
    readonly brand?: HanzoIamBrandId;
    readonly label?: string;
  }): Promise<HanzoIamAccount> => {
    await this.hydrate();
    const trimmed = input.token.trim();
    if (!trimmed) {
      throw new Error("Enter a valid Hanzo IAM token or API key.");
    }
    const brand = getHanzoIamBrand(input.brand ?? "hanzo");
    const userinfo = await fetchHanzoIamUserinfo(brand.issuer, trimmed);
    const account = buildHanzoIamAccountFromTokens({
      brand: brand.id,
      issuer: brand.issuer,
      clientId: brand.clientId,
      accessToken: trimmed,
      userinfo: {
        ...userinfo,
        ...(input.label?.trim() ? { displayName: input.label.trim() } : {}),
      },
    });
    await this.persistVault(upsertHanzoIamVaultAccount(this.vault, account, true));
    void this.refreshOrganizationsAndBilling();
    return account;
  };

  readonly switchAccount = async (accountId: string): Promise<void> => {
    await this.hydrate();
    const target = this.vault.accounts.find((entry) => entry.id === accountId);
    if (!target) return;
    await this.persistVault({
      ...this.vault,
      activeAccountId: target.id,
      activeOrgId: this.vault.orgByAccountId?.[target.id] ?? target.owner,
    });
    void this.refreshOrganizationsAndBilling();
  };

  readonly switchOrganization = async (orgId: string): Promise<void> => {
    await this.hydrate();
    const trimmed = orgId.trim();
    if (!trimmed) return;
    await this.persistVault(switchHanzoIamVaultOrg(this.vault, trimmed));
    void this.refreshOrganizationsAndBilling();
  };

  readonly removeAccount = async (accountId: string): Promise<void> => {
    await this.hydrate();
    this.organizationsByAccount.delete(accountId);
    await this.persistVault(removeHanzoIamVaultAccount(this.vault, accountId));
    void this.refreshOrganizationsAndBilling();
  };

  readonly signOut = async (): Promise<void> => {
    await this.hydrate();
    if (this.snapshot.activeAccount) {
      await this.removeAccount(this.snapshot.activeAccount.id);
    } else {
      await this.persistVault(EMPTY_HANZO_IAM_VAULT);
    }
  };

  readonly signOutAll = async (): Promise<void> => {
    this.organizationsByAccount.clear();
    this.billingSummary = null;
    await this.persistVault(EMPTY_HANZO_IAM_VAULT);
  };

  /** Test helper to reset in-memory state deterministically. */
  readonly resetForTesting = (
    initial?: Partial<HanzoMobileAuthSnapshot> & {
      readonly pendingPkce?: PendingMobilePkce | null;
    },
  ): void => {
    this.hydratePromise = Promise.resolve();
    this.organizationsByAccount.clear();
    this.billingSummary = initial?.billing ?? null;
    this.pendingPkce = initial?.pendingPkce ?? null;
    const accounts = initial?.accounts ?? (initial?.activeAccount ? [initial.activeAccount] : []);
    const activeAccount = initial?.activeAccount ?? accounts[0] ?? null;
    const activeOrgId = initial?.activeOrgId ?? activeAccount?.owner ?? null;
    this.vault = {
      activeAccountId: activeAccount?.id ?? null,
      accounts,
      activeOrgId,
      orgByAccountId: activeAccount && activeOrgId ? { [activeAccount.id]: activeOrgId } : {},
    };
    if (activeAccount && initial?.organizations) {
      this.organizationsByAccount.set(activeAccount.id, initial.organizations);
    }
    this.snapshot = {
      isLoaded: initial?.isLoaded ?? true,
      activeAccount,
      accounts,
      activeOrgId,
      organizations:
        initial?.organizations ?? (activeAccount ? resolveAccountOrganizations(activeAccount) : []),
      billing: activeAccount ? (initial?.billing ?? null) : null,
    };
    for (const listener of this.listeners) listener();
  };
}

export const hanzoMobileAuthStore = new HanzoMobileAuthController();
export type { HanzoMobileAuthController };
export { HANZO_IAM_BRANDS, type HanzoIamAccount, type HanzoIamBrandId };

const getAuthToken = async (_opts?: unknown) => await hanzoMobileAuthStore.getToken();
const signOutActiveAccount = async () => {
  await hanzoMobileAuthStore.signOut();
};

export function useAuth(_options?: { readonly treatPendingAsSignedOut?: boolean }) {
  const state = useSyncExternalStore(
    hanzoMobileAuthStore.subscribe,
    hanzoMobileAuthStore.getSnapshot,
    hanzoMobileAuthStore.getSnapshot,
  );

  return {
    isLoaded: state.isLoaded,
    isSignedIn: state.isLoaded && state.activeAccount !== null,
    userId: state.activeAccount?.id ?? null,
    orgId: state.activeOrgId,
    getToken: getAuthToken,
    signOut: signOutActiveAccount,
  };
}

export function useUser() {
  const state = useSyncExternalStore(
    hanzoMobileAuthStore.subscribe,
    hanzoMobileAuthStore.getSnapshot,
    hanzoMobileAuthStore.getSnapshot,
  );
  const active = state.activeAccount;
  return {
    isLoaded: state.isLoaded,
    isSignedIn: state.isLoaded && active !== null,
    user: active
      ? {
          id: active.id,
          username: active.subject,
          fullName: active.displayName ?? active.id,
          primaryEmailAddress: {
            emailAddress: active.email ?? active.id,
          },
        }
      : null,
  };
}

export function useHanzoMobileAccounts() {
  const state = useSyncExternalStore(
    hanzoMobileAuthStore.subscribe,
    hanzoMobileAuthStore.getSnapshot,
    hanzoMobileAuthStore.getSnapshot,
  );
  return {
    ...state,
    brands: HANZO_IAM_BRANDS,
    signInWithBrand: hanzoMobileAuthStore.signInWithBrand,
    signInWithToken: hanzoMobileAuthStore.signInWithToken,
    switchAccount: hanzoMobileAuthStore.switchAccount,
    switchOrganization: hanzoMobileAuthStore.switchOrganization,
    refreshOrganizationsAndBilling: hanzoMobileAuthStore.refreshOrganizationsAndBilling,
    removeAccount: hanzoMobileAuthStore.removeAccount,
    signOut: hanzoMobileAuthStore.signOut,
    signOutAll: hanzoMobileAuthStore.signOutAll,
  };
}
