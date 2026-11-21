// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
import * as NodeChildProcess from "node:child_process";
import * as NodeFSP from "node:fs/promises";
import * as NodePath from "node:path";

/** Everything a fetcher touches outside itself. Tests replace it; nothing here reaches our cloud. */
export interface UsageEnv {
  readonly fetch: typeof globalThis.fetch;
  readonly home: string;
  readonly env: Readonly<Record<string, string | undefined>>;
  readonly now: () => Date;
  readonly readText: (file: string) => Promise<string | undefined>;
  readonly listFiles: (dir: string, suffix: string) => Promise<ReadonlyArray<string>>;
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
