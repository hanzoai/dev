// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
import * as NodeChildProcess from "node:child_process";
import * as NodeFSP from "node:fs/promises";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";
import * as NodeSqlite from "node:sqlite";

/** Everything a fetcher touches outside itself. Tests replace it; nothing here reaches our cloud. */
export interface UsageEnv {
  readonly fetch: typeof globalThis.fetch;
  readonly platform: string;
  readonly home: string;
  readonly env: Readonly<Record<string, string | undefined>>;
  readonly now: () => Date;
  readonly readText: (file: string) => Promise<string | undefined>;
  readonly listFiles: (dir: string, suffix: string) => Promise<ReadonlyArray<string>>;
  /** Names of the entries directly inside `dir`; empty when it is missing. */
  readonly listDir: (dir: string) => Promise<ReadonlyArray<string>>;
  /** One text value from a SQLite database opened read-only; undefined when absent or unreadable. */
  readonly sqliteValue: (
    file: string,
    sql: string,
    ...params: string[]
  ) => Promise<string | undefined>;
  /** Runs a provider's own CLI and returns stdout; undefined when it is missing, fails or times out. */
  readonly exec: (
    file: string,
    args: ReadonlyArray<string>,
    options: {
      readonly env: Readonly<Record<string, string>>;
      readonly cwd: string;
      readonly timeoutMs: number;
    },
  ) => Promise<string | undefined>;
  /** A private temp directory holding `files` (relative path -> text), removed when `run` ends. */
  readonly withScratch: <T>(
    files: Readonly<Record<string, string>>,
    run: (dir: string) => Promise<T>,
  ) => Promise<T>;
  /** macOS keychain generic password, read through the user's own `security` tool. */
  readonly keychain: (service: string) => Promise<string | undefined>;
}

export interface HostFacts {
  readonly platform: string;
  readonly home: string;
  readonly env: Readonly<Record<string, string | undefined>>;
}

/** The real machine. Callers pass host facts from HostProcess so no global process access lives here. */
export const nodeEnv = ({ platform, home, env }: HostFacts): UsageEnv => ({
  fetch: globalThis.fetch,
  platform,
  home,
  env,
  now: () => new Date(),
  readText: (file) => NodeFSP.readFile(file, "utf8").catch(() => undefined),
  listFiles: async (dir, suffix) => {
    const out: string[] = [];
    const walk = async (d: string): Promise<void> => {
      const entries = await NodeFSP.readdir(d, { withFileTypes: true }).catch(() => []);
      for (const e of entries) {
        const p = NodePath.join(d, e.name);
        if (e.isDirectory()) await walk(p);
        else if (e.name.endsWith(suffix)) out.push(p);
      }
    };
    await walk(dir);
    return out;
  },
  listDir: (dir) => NodeFSP.readdir(dir).catch(() => []),
  sqliteValue: async (file, sql, ...params) => {
    try {
      const db = new NodeSqlite.DatabaseSync(file, { readOnly: true });
      try {
        const row = db.prepare(sql).get(...params);
        const value = row ? Object.values(row)[0] : undefined;
        return typeof value === "string" && value ? value : undefined;
      } finally {
        db.close();
      }
    } catch {
      return undefined;
    }
  },
  exec: (file, args, { env, cwd, timeoutMs }) =>
    new Promise((resolve) => {
      const child = NodeChildProcess.execFile(
        file,
        [...args],
        { env, cwd, timeout: timeoutMs, maxBuffer: 1 << 20 },
        (err, stdout) => resolve(err ? undefined : stdout),
      );
      child.stdin?.end();
    }),
  withScratch: async (files, run) => {
    const dir = await NodeFSP.mkdtemp(NodePath.join(NodeOS.tmpdir(), "hanzo-usage-"));
    try {
      for (const [rel, text] of Object.entries(files)) {
        const file = NodePath.join(dir, rel);
        await NodeFSP.mkdir(NodePath.dirname(file), { recursive: true });
        await NodeFSP.writeFile(file, text, { mode: rel.startsWith("bin/") ? 0o700 : 0o600 });
      }
      return await run(dir);
    } finally {
      await NodeFSP.rm(dir, { recursive: true, force: true });
    }
  },
  keychain: (service) =>
    platform !== "darwin"
      ? Promise.resolve(undefined)
      : new Promise((resolve) =>
          NodeChildProcess.execFile(
            "security",
            ["find-generic-password", "-s", service, "-w"],
            (err, stdout) => resolve(err ? undefined : stdout.trim() || undefined),
          ),
        ),
});
