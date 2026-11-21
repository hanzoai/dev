// @effect-diagnostics nodeBuiltinImport:off globalDate:off
import * as NodeChildProcess from "node:child_process";
import * as NodeFSP from "node:fs/promises";
import * as NodePath from "node:path";

import {
  EMPTY_SHARED_IDENTITY,
  isSharedTokenUsable,
  parseSharedIdentity,
  sharedIdentityDirectory,
  type SharedIdentitySnapshot,
} from "@hanzodev/shared/hanzoSharedIdentity";

export interface SharedIdentityHost {
  readonly platform: string;
  readonly home: string;
  readonly env: Readonly<Record<string, string | undefined>>;
}

const read = (file: string) => NodeFSP.readFile(file, "utf8").catch(() => undefined);

/** The machine's identities, read-only from the vault the CLI and hanzod own. */
export async function readSharedIdentity(
  host: SharedIdentityHost,
): Promise<SharedIdentitySnapshot> {
  const dir = sharedIdentityDirectory(host);
  const [credentials, config] = await Promise.all([
    read(NodePath.join(dir, "credentials")),
    read(NodePath.join(dir, "config.toml")),
  ]);
  return credentials ? parseSharedIdentity({ credentials, config }) : EMPTY_SHARED_IDENTITY;
}

const hanzoBinary = async (host: SharedIdentityHost): Promise<string> => {
  for (const candidate of [
    NodePath.join(host.home, ".local", "bin", "hanzo"),
    NodePath.join(host.home, ".hanzo", "bin", "hanzo"),
    "/opt/homebrew/bin/hanzo",
    "/usr/local/bin/hanzo",
  ]) {
    if (
      await NodeFSP.access(candidate).then(
        () => true,
        () => false,
      )
    )
      return candidate;
  }
  return "hanzo";
};

/**
 * Runs `hanzo auth <args>`. The CLI is the writer: it takes the vault's lock, refreshes under the
 * refresh lock, and moves the active identity. Dev never writes the vault or spends a refresh token.
 */
export async function runHanzoAuth(
  host: SharedIdentityHost,
  args: ReadonlyArray<string>,
  timeoutMs: number,
): Promise<string> {
  const binary = await hanzoBinary(host);
  return new Promise((resolve, reject) => {
    NodeChildProcess.execFile(
      binary,
      ["auth", ...args],
      { timeout: timeoutMs, env: { ...host.env, NO_COLOR: "1" } },
      (error, stdout, stderr) =>
        error ? reject(new Error(stderr.trim() || error.message)) : resolve(stdout.trim()),
    );
  });
}

/** `owner/name` of an identity as the CLI addresses it; anything else is refused before it reaches argv. */
export const IDENTITY_ID = /^[A-Za-z0-9._-]+\/[A-Za-z0-9._-]+$/u;

/**
 * The active identity's access token: the vault's when unexpired, otherwise what the CLI prints
 * after refreshing it. Undefined when neither works, which is the only time the user is asked to sign in.
 */
export async function hanzoAccessToken(host: SharedIdentityHost): Promise<string | undefined> {
  const snapshot = await readSharedIdentity(host);
  const active = snapshot.accounts.find((account) => account.id === snapshot.activeAccountId);
  if (!active) return undefined;
  if (isSharedTokenUsable(active, Date.now())) return active.accessToken;
  const refreshed = await runHanzoAuth(host, ["token", "--as", active.id], 20_000).catch(
    () => undefined,
  );
  return refreshed || undefined;
}

/** The longest a client waits for the CLI to refresh the active identity before it is shown as is. */
const IDENTITY_REFRESH_WAIT_MS = 3_000;

/** Refresh tokens are single-use and the CLI's; a client gets access tokens only. */
const withoutRefreshTokens = (snapshot: SharedIdentitySnapshot): SharedIdentitySnapshot => ({
  ...snapshot,
  accounts: snapshot.accounts.map((account) => ({ ...account, refreshToken: null })),
});

/** The snapshot clients receive: the active identity's token is refreshed through the CLI when it expired. */
export async function readFreshSharedIdentity(
  host: SharedIdentityHost,
): Promise<SharedIdentitySnapshot> {
  const snapshot = await readSharedIdentity(host);
  const active = snapshot.accounts.find((account) => account.id === snapshot.activeAccountId);
  if (!active || isSharedTokenUsable(active, Date.now())) return withoutRefreshTokens(snapshot);
  // The CLI may stop on a keychain approval; the account must still show from the vault file.
  let timer: ReturnType<typeof setTimeout> | undefined;
  await Promise.race([
    runHanzoAuth(host, ["token", "--as", active.id], 20_000).catch(() => undefined),
    new Promise((resolve) => {
      timer = setTimeout(resolve, IDENTITY_REFRESH_WAIT_MS);
    }),
  ]);
  clearTimeout(timer);
  return withoutRefreshTokens(await readSharedIdentity(host));
}
