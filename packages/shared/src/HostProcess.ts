import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";
import * as NodeOS from "node:os";
import * as NodeSea from "node:sea";

export const Platform = Context.Reference<NodeJS.Platform>(
  "@hanzodev/shared/HostProcess/Platform",
  {
    defaultValue: () => process.platform,
  },
);

export const Architecture = Context.Reference<NodeJS.Architecture>(
  "@hanzodev/shared/HostProcess/Architecture",
  {
    defaultValue: () => process.arch,
  },
);

export const KernelRelease = Context.Reference<string>(
  "@hanzodev/shared/HostProcess/KernelRelease",
  {
    defaultValue: () => NodeOS.release(),
  },
);

export const LinuxLibc = Context.Reference<"gnu" | "musl">(
  "@hanzodev/shared/HostProcess/LinuxLibc",
  {
    defaultValue: () => {
      try {
        const report = process.report?.getReport() as
          | { header?: { glibcVersionRuntime?: unknown } }
          | undefined;
        return typeof report?.header?.glibcVersionRuntime === "string" ? "gnu" : "musl";
      } catch {
        return "musl";
      }
    },
  },
);

export const Hostname = Context.Reference<string>("@hanzodev/shared/HostProcess/Hostname", {
  defaultValue: () => NodeOS.hostname(),
});

export const Username = Context.Reference<string | null>("@hanzodev/shared/HostProcess/Username", {
  defaultValue: () => {
    try {
      return NodeOS.userInfo().username.trim() || null;
    } catch {
      return null;
    }
  },
});

export const HomeDirectory = Context.Reference<string>(
  "@hanzodev/shared/HostProcess/HomeDirectory",
  {
    defaultValue: () => NodeOS.homedir(),
  },
);

export const Environment = Context.Reference<NodeJS.ProcessEnv>(
  "@hanzodev/shared/HostProcess/Environment",
  {
    defaultValue: () => process.env,
  },
);

export const WorkingDirectory = Context.Reference<string>(
  "@hanzodev/shared/HostProcess/WorkingDirectory",
  {
    defaultValue: () => process.cwd(),
  },
);

export const ExecutablePath = Context.Reference<string>(
  "@hanzodev/shared/HostProcess/ExecutablePath",
  {
    defaultValue: () => process.execPath,
  },
);

export const Arguments = Context.Reference<ReadonlyArray<string>>(
  "@hanzodev/shared/HostProcess/Arguments",
  {
    defaultValue: () => process.argv,
  },
);

/** Original shell command spelling; argv[0] and execPath instead identify the resolved binary. */
export const InvokedAs = Context.Reference<string>("@hanzodev/shared/HostProcess/InvokedAs", {
  defaultValue: () => process.argv0,
});

export const IsExecutable = Context.Reference<boolean>(
  "@hanzodev/shared/HostProcess/IsExecutable",
  {
    defaultValue: () => NodeSea.isSea(),
  },
);

export class HostnameLookupError extends Schema.TaggedError<HostnameLookupError>()(
  "HostnameLookupError",
  { hostname: Schema.String, cause: Schema.Defect() },
) {
  override get message(): string {
    return `Could not resolve ${this.hostname}.`;
  }
}

export const UserId = Context.Reference<number | undefined>("@hanzodev/shared/HostProcess/UserId", {
  defaultValue: () => process.getuid?.(),
});

export const isWindows = Effect.map(Platform, (platform) => platform === "win32");
