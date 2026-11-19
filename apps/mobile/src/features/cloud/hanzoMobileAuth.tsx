import * as Linking from "expo-linking";
import * as SecureStore from "expo-secure-store";
import * as WebBrowser from "expo-web-browser";
import type { CloudAccountSource } from "@hanzodev/client-runtime/relay";
import {
  buildHanzoIamAccountFromTokens,
  buildHanzoIamAuthorizeUrl,
  createOAuthState,
  createPkceChallengeS256,
  createPkceVerifier,
  exchangeHanzoIamAuthorizationCode,
  fetchHanzoIamUserinfo,
  getHanzoIamBrand,
  HANZO_IAM_BRANDS,
  type HanzoIamAccount,
  type HanzoIamBrandId,
  type HanzoIamVaultState,
  isHanzoIamTokenExpiringSoon,
  refreshHanzoIamAccount,
  removeHanzoIamVaultAccount,
  upsertHanzoIamVaultAccount,
} from "@hanzodev/shared/hanzoIamAuth";
import { useCallback, useSyncExternalStore } from "react";

WebBrowser.maybeCompleteAuthSession();

const VAULT_STORAGE_KEY = "hanzo.dev.iam.vault.v1";

export interface HanzoMobileAuthSnapshot {
  readonly isLoaded: boolean;
  readonly activeAccount: HanzoIamAccount | null;
  readonly accounts: ReadonlyArray<HanzoIamAccount>;
}

type AuthListener = () => void;
type CloudSourceListener = Parameters<CloudAccountSource["addListener"]>[0];

class HanzoMobileAuthController implements CloudAccountSource {
  private snapshot: HanzoMobileAuthSnapshot = {
    isLoaded: false,
    activeAccount: null,
    accounts: [],
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

  private emit(next: HanzoMobileAuthSnapshot): void {
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
    const activeAccount =
      vault.accounts.find((entry) => entry.id === vault.activeAccountId) ??
      vault.accounts[0] ??
      null;
    const normalized: HanzoIamVaultState = {
      activeAccountId: activeAccount?.id ?? null,
      accounts: vault.accounts,
    };
    try {
      if (normalized.accounts.length === 0) {
        await SecureStore.deleteItemAsync(VAULT_STORAGE_KEY);
      } else {
        await SecureStore.setItemAsync(VAULT_STORAGE_KEY, JSON.stringify(normalized));
      }
    } catch {
      // In-memory state still updates in simulator/test environments without keychain entitlements.
    }
    this.emit({
      isLoaded: true,
      activeAccount,
      accounts: normalized.accounts,
    });
  }

  readonly hydrate = (): Promise<void> => {
    if (this.hydratePromise) return this.hydratePromise;
    this.hydratePromise = (async () => {
      try {
        const raw = await SecureStore.getItemAsync(VAULT_STORAGE_KEY);
        if (!raw) {
          this.emit({ isLoaded: true, activeAccount: null, accounts: [] });
          return;
        }
        const parsed = JSON.parse(raw) as Partial<HanzoIamVaultState>;
        const accounts = Array.isArray(parsed.accounts)
          ? parsed.accounts.filter(
              (entry): entry is HanzoIamAccount =>
                Boolean(entry && typeof entry.id === "string" && typeof entry.accessToken === "string"),
            )
          : [];
        const activeAccount =
          accounts.find((entry) => entry.id === parsed.activeAccountId) ?? accounts[0] ?? null;
        this.emit({
          isLoaded: true,
          activeAccount,
          accounts,
        });
      } catch {
        this.emit({ isLoaded: true, activeAccount: null, accounts: [] });
      }
    })();
    return this.hydratePromise;
  };

  readonly getToken = async (accountId?: string): Promise<string | null> => {
    await this.hydrate();
    const target = accountId
      ? this.snapshot.accounts.find((entry) => entry.id === accountId) ?? null
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
        {
          activeAccountId: this.snapshot.activeAccount?.id ?? refreshed.id,
          accounts: this.snapshot.accounts,
        },
        refreshed,
        this.snapshot.activeAccount?.id === target.id,
      );
      await this.persistVault(nextVault);
      return refreshed.accessToken;
    } catch {
      return target.accessToken;
    }
  };

  readonly signInWithBrand = async (brandId: HanzoIamBrandId = "hanzo"): Promise<HanzoIamAccount> => {
    await this.hydrate();
    const brand = getHanzoIamBrand(brandId);
    const redirectUri = Linking.createURL("oauth/callback");
    const verifier = createPkceVerifier();
    const challenge = createPkceChallengeS256(verifier);
    const state = createOAuthState();
    const authUrl = buildHanzoIamAuthorizeUrl({
      issuer: brand.issuer,
      clientId: brand.clientId,
      redirectUri,
      state,
      codeChallenge: challenge,
    });

    const result = await WebBrowser.openAuthSessionAsync(authUrl, redirectUri);
    if (result.type !== "success" || !result.url) {
      throw new Error("Sign-in was cancelled.");
    }
    const callbackUrl = new URL(result.url);
    const returnedState = callbackUrl.searchParams.get("state");
    if (returnedState !== state) {
      throw new Error("OAuth state mismatch during Hanzo ID sign-in.");
    }
    const code = callbackUrl.searchParams.get("code")?.trim();
    if (!code) {
      const errorDesc =
        callbackUrl.searchParams.get("error_description") ??
        callbackUrl.searchParams.get("error") ??
        "Missing authorization code.";
      throw new Error(errorDesc);
    }

    const account = await exchangeHanzoIamAuthorizationCode({
      brand: brand.id,
      issuer: brand.issuer,
      clientId: brand.clientId,
      code,
      redirectUri,
      codeVerifier: verifier,
    });

    const nextVault = upsertHanzoIamVaultAccount(
      {
        activeAccountId: this.snapshot.activeAccount?.id ?? null,
        accounts: this.snapshot.accounts,
      },
      account,
      true,
    );
    await this.persistVault(nextVault);
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
    const nextVault = upsertHanzoIamVaultAccount(
      {
        activeAccountId: this.snapshot.activeAccount?.id ?? null,
        accounts: this.snapshot.accounts,
      },
      account,
      true,
    );
    await this.persistVault(nextVault);
    return account;
  };

  readonly switchAccount = async (accountId: string): Promise<void> => {
    await this.hydrate();
    const exists = this.snapshot.accounts.some((entry) => entry.id === accountId);
    if (!exists) return;
    await this.persistVault({
      activeAccountId: accountId,
      accounts: this.snapshot.accounts,
    });
  };

  readonly removeAccount = async (accountId: string): Promise<void> => {
    await this.hydrate();
    const nextVault = removeHanzoIamVaultAccount(
      {
        activeAccountId: this.snapshot.activeAccount?.id ?? null,
        accounts: this.snapshot.accounts,
      },
      accountId,
    );
    await this.persistVault(nextVault);
  };

  readonly signOut = async (): Promise<void> => {
    await this.hydrate();
    if (this.snapshot.activeAccount) {
      await this.removeAccount(this.snapshot.activeAccount.id);
    } else {
      await this.persistVault({ activeAccountId: null, accounts: [] });
    }
  };

  readonly signOutAll = async (): Promise<void> => {
    await this.persistVault({ activeAccountId: null, accounts: [] });
  };

  /** Test helper to reset in-memory state deterministically. */
  readonly resetForTesting = (initial?: Partial<HanzoMobileAuthSnapshot>): void => {
    this.hydratePromise = Promise.resolve();
    this.emit({
      isLoaded: initial?.isLoaded ?? true,
      activeAccount: initial?.activeAccount ?? null,
      accounts: initial?.accounts ?? (initial?.activeAccount ? [initial.activeAccount] : []),
    });
  };
}

export const hanzoMobileAuthStore = new HanzoMobileAuthController();
export type { HanzoMobileAuthController };
export { HANZO_IAM_BRANDS, type HanzoIamAccount, type HanzoIamBrandId };

export function useAuth(_options?: { readonly treatPendingAsSignedOut?: boolean }) {
  const state = useSyncExternalStore(
    hanzoMobileAuthStore.subscribe,
    hanzoMobileAuthStore.getSnapshot,
    hanzoMobileAuthStore.getSnapshot,
  );
  const getToken = useCallback(
    async (_opts?: unknown) => await hanzoMobileAuthStore.getToken(),
    [],
  );
  const signOut = useCallback(async () => {
    await hanzoMobileAuthStore.signOut();
  }, []);

  return {
    isLoaded: state.isLoaded,
    isSignedIn: state.isLoaded && state.activeAccount !== null,
    userId: state.activeAccount?.id ?? null,
    orgId: state.activeAccount?.owner ?? null,
    getToken,
    signOut,
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
    removeAccount: hanzoMobileAuthStore.removeAccount,
    signOut: hanzoMobileAuthStore.signOut,
    signOutAll: hanzoMobileAuthStore.signOutAll,
  };
}
