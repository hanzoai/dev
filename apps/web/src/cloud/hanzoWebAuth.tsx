import type { CloudAccountSource } from "@hanzodev/client-runtime/relay";
import {
  buildHanzoIamAccountFromTokens,
  buildHanzoIamAuthorizeUrl,
  createOAuthState,
  createPkceChallengeS256,
  createPkceVerifier,
  EMPTY_HANZO_IAM_VAULT,
  exchangeHanzoIamAuthorizationCode,
  fetchHanzoIamUserinfo,
  HANZO_IAM_BRANDS,
  HANZO_IAM_VAULT_STORAGE_KEY,
  type HanzoIamAccount,
  type HanzoIamBrandId,
  type HanzoIamVaultState,
  isHanzoIamTokenExpiringSoon,
  normalizeHanzoIssuer,
  parseHanzoIamVaultJson,
  refreshHanzoIamAccount,
  removeHanzoIamVaultAccount,
  resolveActiveHanzoAccount,
  serializeHanzoIamVaultJson,
  upsertHanzoIamVaultAccount,
} from "@hanzodev/shared/hanzoIamAuth";
import {
  CheckIcon,
  KeyRoundIcon,
  LogOutIcon,
  PlusIcon,
  RefreshCwIcon,
  Trash2Icon,
  XIcon,
} from "lucide-react";
import {
  createContext,
  type ReactNode,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
  useSyncExternalStore,
} from "react";

import { readDesktopPrimaryBearerToken } from "../environments/primary/desktopAuth";
import { resolvePrimaryEnvironmentHttpUrl } from "../environments/primary/target";
import { isHostedStaticApp } from "../hostedPairing";
import { ManagedRelayAuthProvider } from "./managedAuth";
import { resolveCloudPublicConfig } from "./publicConfig";

const PKCE_SESSION_STORAGE_KEY = "hanzo.dev.iam.pkce.v1";

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

type ModalTabId = "accounts" | string;

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
    window.localStorage.setItem(HANZO_IAM_VAULT_STORAGE_KEY, serializeHanzoIamVaultJson(vault));
  } catch {
    // Ignore private-browsing storage quota errors
  }
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

export class HanzoWebAuthController implements CloudAccountSource {
  private vault: HanzoIamVaultState = EMPTY_HANZO_IAM_VAULT;
  private loaded = false;
  private busy = false;
  private lastError: string | null = null;
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
    writeBrowserVault(next);
    this.emit();
  }

  hydrate(): Promise<void> {
    if (this.hydratePromise) return this.hydratePromise;
    this.hydratePromise = (async () => {
      this.vault = readBrowserVault();
      await this.completePendingPkceCallback();
      await this.syncLocalCliIdentities();
      this.loaded = true;
      this.emit();
    })();
    return this.hydratePromise;
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

    const rawPending = window.sessionStorage.getItem(PKCE_SESSION_STORAGE_KEY);
    if (!rawPending) return;

    let pending: PendingPkceState;
    try {
      pending = JSON.parse(rawPending) as PendingPkceState;
    } catch {
      window.sessionStorage.removeItem(PKCE_SESSION_STORAGE_KEY);
      return;
    }

    if (pending.state !== state) return;
    window.sessionStorage.removeItem(PKCE_SESSION_STORAGE_KEY);

    // Clean OAuth parameters from URL immediately
    currentUrl.searchParams.delete("code");
    currentUrl.searchParams.delete("state");
    currentUrl.searchParams.delete("session_state");
    window.history.replaceState({}, "", currentUrl.toString());

    this.busy = true;
    this.lastError = null;
    try {
      const account = await exchangeHanzoIamAuthorizationCode({
        issuer: pending.issuer,
        clientId: pending.clientId,
        code,
        codeVerifier: pending.verifier,
        redirectUri: pending.redirectUri,
        brand: pending.brandId,
      });
      this.vault = upsertHanzoIamVaultAccount(this.vault, account, true);
      writeBrowserVault(this.vault);
    } catch (error) {
      this.lastError = error instanceof Error ? error.message : String(error);
    } finally {
      this.busy = false;
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
        };
      }
      this.vault = nextVault;
      writeBrowserVault(this.vault);
      if (this.loaded) {
        this.emit();
      }
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
    const redirectUrl = new URL(window.location.href);
    redirectUrl.searchParams.delete("code");
    redirectUrl.searchParams.delete("state");
    const redirectUri = redirectUrl.toString();

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
    window.sessionStorage.setItem(PKCE_SESSION_STORAGE_KEY, JSON.stringify(pending));
    const authorizeUrl = buildHanzoIamAuthorizeUrl({
      issuer,
      clientId,
      redirectUri,
      state,
      codeChallenge,
    });
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
  };

  removeAccount = (accountId: string): void => {
    this.persistVault(removeHanzoIamVaultAccount(this.vault, accountId));
  };

  signOut = async (): Promise<void> => {
    const active = this.activeAccount;
    if (!active) return;
    this.persistVault(removeHanzoIamVaultAccount(this.vault, active.id));
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
  const isBusy = useSyncExternalStore(
    controller.subscribe,
    () => controller.isBusy,
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
  const [localError, setLocalError] = useState<string | null>(null);

  if (!modal.open) return null;

  const activeCustomPage = modal.pages.find((page) => page.url === modal.tab) ?? null;
  const shouldShowSignInForm = accounts.length === 0 || showAddAccount;

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

  return (
    <div
      className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 p-4 backdrop-blur-xs"
      onClick={(event) => {
        if (event.target === event.currentTarget) {
          controller.closeModal();
        }
      }}
    >
      <div className="flex max-h-[85vh] w-full max-w-2xl flex-col overflow-hidden rounded-2xl border border-border bg-background text-foreground shadow-2xl">
        <div className="flex items-center justify-between border-b border-border px-5 py-3.5">
          <div className="flex items-center gap-2.5">
            <div className="flex size-7 items-center justify-center rounded-md bg-foreground text-xs font-bold text-background">
              H
            </div>
            <div>
              <div className="text-sm font-semibold">Hanzo IAM &amp; Connect</div>
              <div className="text-xs text-muted-foreground">
                Unified multi-account identity &amp; remote control
              </div>
            </div>
          </div>
          <button
            type="button"
            onClick={controller.closeModal}
            className="rounded-lg p-1.5 text-muted-foreground hover:bg-muted hover:text-foreground"
            aria-label="Close"
          >
            <XIcon className="size-4" />
          </button>
        </div>

        <div className="flex border-b border-border bg-muted/30 px-4">
          <button
            type="button"
            onClick={() => controller.setModalTab("accounts")}
            className={`border-b-2 px-3 py-2.5 text-xs font-medium transition-colors ${
              modal.tab === "accounts"
                ? "border-foreground text-foreground"
                : "border-transparent text-muted-foreground hover:text-foreground"
            }`}
          >
            Accounts ({accounts.length})
          </button>
          {activeAccount &&
            modal.pages.map((page) => (
              <button
                key={page.url}
                type="button"
                onClick={() => controller.setModalTab(page.url)}
                className={`flex items-center gap-1.5 border-b-2 px-3 py-2.5 text-xs font-medium transition-colors ${
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
          ) : (
            <div className="space-y-5">
              {accounts.length > 0 && (
                <div className="space-y-2.5">
                  <div className="flex items-center justify-between">
                    <span className="text-xs font-semibold tracking-wider text-muted-foreground uppercase">
                      Signed-in accounts
                    </span>
                    <div className="flex items-center gap-2">
                      {!isHostedStaticApp() && (
                        <button
                          type="button"
                          onClick={() => void controller.syncLocalCliIdentities()}
                          className="inline-flex items-center gap-1 rounded-md border border-border px-2 py-1 text-xs text-muted-foreground hover:bg-muted hover:text-foreground"
                        >
                          <RefreshCwIcon className="size-3" />
                          <span>Sync CLI</span>
                        </button>
                      )}
                      <button
                        type="button"
                        onClick={() => setShowAddAccount((prev) => !prev)}
                        className="inline-flex items-center gap-1 rounded-md border border-border px-2 py-1 text-xs font-medium hover:bg-muted"
                      >
                        <PlusIcon className="size-3" />
                        <span>Add account</span>
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
                                    Active
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
                                Switch
                              </button>
                            )}
                            <button
                              type="button"
                              onClick={() => controller.removeAccount(account.id)}
                              className="rounded-md p-1.5 text-muted-foreground hover:bg-destructive/10 hover:text-destructive"
                              aria-label="Remove account"
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
                      {accounts.length > 0 ? "Add another IAM account" : "Sign in with Hanzo IAM"}
                    </div>
                    <div className="mt-0.5 text-xs text-muted-foreground">
                      Connect Hanzo, Lux, Zoo, or Pars accounts for unified remote control across
                      desktop, web, and mobile.
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
                      Continue with{" "}
                      {HANZO_IAM_BRANDS.find((b) => b.id === selectedBrand)?.label ?? "Hanzo"}
                    </button>
                    <button
                      type="button"
                      onClick={() => setShowTokenEntry((prev) => !prev)}
                      className="inline-flex items-center gap-1.5 rounded-lg border border-border bg-background px-3 py-2 text-xs font-medium hover:bg-muted"
                    >
                      <KeyRoundIcon className="size-3.5" />
                      <span>{showTokenEntry ? "Hide token sign-in" : "Use CLI / API token"}</span>
                    </button>
                  </div>

                  {showTokenEntry && (
                    <div className="space-y-2 pt-1">
                      <input
                        type="password"
                        value={tokenInput}
                        onChange={(event) => setTokenInput(event.target.value)}
                        placeholder="Paste token from `dev token` or `hanzo iam token`"
                        className="w-full rounded-lg border border-border bg-background px-3 py-2 font-mono text-xs focus:outline-hidden"
                      />
                      <button
                        type="button"
                        disabled={isBusy || !tokenInput.trim()}
                        onClick={() => void handleTokenSubmit()}
                        className="rounded-lg border border-border bg-foreground px-3 py-1.5 text-xs font-semibold text-background disabled:opacity-50"
                      >
                        Save token account
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

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- hydrates persisted IAM vault and local ~/.hanzo/identities.json accounts once on startup
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

  return {
    isLoaded,
    isSignedIn: Boolean(activeAccount),
    userId: activeAccount?.id ?? null,
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

  const user = useMemo(() => {
    if (!activeAccount) return null;
    return {
      id: activeAccount.id,
      fullName: activeAccount.displayName,
      username: activeAccount.subject,
      imageUrl: activeAccount.avatarUrl ?? "",
      primaryEmailAddress: activeAccount.email ? { emailAddress: activeAccount.email } : null,
      emailAddresses: activeAccount.email ? [{ emailAddress: activeAccount.email }] : [],
    };
  }, [activeAccount]);

  return {
    isLoaded,
    isSignedIn: Boolean(activeAccount),
    user,
  };
}

export function useClerk() {
  const controller = useController();
  const activeAccount = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeAccount,
    () => null,
  );

  return useMemo(
    () => ({
      status: "ready" as "ready" | "error" | "loading",
      loaded: controller.isLoaded,
      user: activeAccount ? { id: activeAccount.id } : null,
      session: activeAccount
        ? {
            id: activeAccount.id,
            getToken: controller.getToken,
          }
        : null,
      addListener: (
        listener: (resources: { readonly user?: { readonly id: string } | null }) => void,
      ) => controller.addListener(listener),
      openSignIn: (props?: { readonly fallbackRedirectUrl?: string }) => {
        controller.openSignIn(
          props?.fallbackRedirectUrl ? { redirectUrl: props.fallbackRedirectUrl } : undefined,
        );
      },
      openUserProfile: (props?: { readonly __experimental_startPath?: string }) => {
        const startTab = props?.__experimental_startPath?.replace(/^\//u, "") || "accounts";
        controller.openAccountModal(startTab);
      },
      signOut: controller.signOut,
    }),
    [activeAccount, controller],
  );
}

export function useHanzoWebAccounts() {
  const controller = useController();
  const accounts = useSyncExternalStore(
    controller.subscribe,
    () => controller.accounts,
    () => [],
  );
  const activeAccount = useSyncExternalStore(
    controller.subscribe,
    () => controller.activeAccount,
    () => null,
  );

  const openModal = useCallback(
    (tab: ModalTabId = "accounts", pages?: ReadonlyArray<HanzoCustomAccountPage>) => {
      controller.openAccountModal(tab, pages);
    },
    [controller],
  );

  const registerPages = useCallback(
    (pages: ReadonlyArray<HanzoCustomAccountPage>) => {
      controller.registerCustomPages(pages);
    },
    [controller],
  );

  return {
    accounts,
    activeAccount,
    switchAccount: controller.switchAccount,
    removeAccount: controller.removeAccount,
    openModal,
    registerPages,
    syncLocalCliIdentities: () => controller.syncLocalCliIdentities(),
  };
}
