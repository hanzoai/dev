#!/usr/bin/env node
import * as NodeRuntime from "@effect/platform-node/NodeRuntime";
import * as NodeServices from "@effect/platform-node/NodeServices";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Layer from "effect/Layer";
import * as Logger from "effect/Logger";
import * as Path from "effect/Path";
import * as Schedule from "effect/Schedule";
import * as Schema from "effect/Schema";
import * as Stream from "effect/Stream";
import { Command, Flag } from "effect/cli";
import { ChildProcess, ChildProcessSpawner } from "effect/process";

import * as NetService from "@hanzodev/shared/Net";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import { smokePackagedServer } from "./lib/packaged-server-smoke.ts";
import { windowsSystemTar } from "./lib/windows-tar.ts";
import { FetchHttpClient } from "effect/http";

export class CliArchiveSmokeError extends Schema.TaggedError<CliArchiveSmokeError>()(
  "CliArchiveSmokeError",
  { step: Schema.String, detail: Schema.String },
) {
  override get message(): string {
    return `CLI archive smoke test failed while ${this.step}: ${this.detail}`;
  }
}

const collect = <E>(stream: Stream.Stream<Uint8Array, E>) =>
  stream.pipe(
    Stream.decodeText(),
    Stream.runFold(
      () => "",
      (acc, chunk) => acc + chunk,
    ),
  );

const runExecutable = Effect.fn("runExecutable")(function* (
  executable: string,
  args: ReadonlyArray<string>,
  cwd: string,
) {
  const spawner = yield* ChildProcessSpawner.ChildProcessSpawner;
  const child = yield* spawner.spawn(
    ChildProcess.make(executable, args, {
      cwd,
      // Isolate system Node and inherited launcher context from the archive smoke check.
      env: { PATH: "", HOME: cwd, USERPROFILE: cwd, TMPDIR: cwd, TEMP: cwd },
      extendEnv: false,
    }),
  );
  const [stdout, stderr, exitCode] = yield* Effect.all(
    [collect(child.stdout), collect(child.stderr), child.exitCode.pipe(Effect.map(Number))],
    { concurrency: "unbounded" },
  );
  return { stdout, stderr, exitCode };
});

const smokeCliArchive = Effect.fn("smokeCliArchive")(function* (input: {
  readonly archive: string;
  readonly expectVersion: string;
}) {
  const fs = yield* FileSystem.FileSystem;
  const path = yield* Path.Path;
  const platform = yield* HostProcess.Platform;
  const spawner = yield* ChildProcessSpawner.ChildProcessSpawner;
  const scratch = yield* fs.makeTempDirectory({ prefix: "hanzo-cli-smoke-" });
  // Windows can briefly retain executable locks; cleanup retries but must not fail a successful smoke check.
  yield* Effect.addFinalizer(() =>
    fs.remove(scratch, { recursive: true }).pipe(
      Effect.retry({ times: 10, schedule: Schedule.spaced("500 millis") }),
      Effect.catch((error) =>
        Effect.logWarning(`[cli-smoke] could not remove ${scratch}: ${error.message}`),
      ),
    ),
  );

  // Windows bsdtar reads zip; Git Bash's GNU tar does not.
  const tar = platform === "win32" ? windowsSystemTar() : "tar";
  const extract = yield* spawner
    .spawn(ChildProcess.make(tar, ["-xf", input.archive, "-C", scratch]))
    .pipe(Effect.flatMap((child) => child.exitCode));
  if (Number(extract) !== 0) {
    return yield* new CliArchiveSmokeError({
      step: "extracting the archive",
      detail: `tar exited with ${String(extract)}`,
    });
  }
  const [root] = yield* fs.readDirectory(scratch);
  if (root === undefined) {
    return yield* new CliArchiveSmokeError({
      step: "extracting the archive",
      detail: "the archive was empty",
    });
  }
  const contentDir = path.join(scratch, root);
  const executable = path.join(contentDir, platform === "win32" ? "hanzo.exe" : "hanzo");
  const bundledLibatomic = path.join(contentDir, "lib/libatomic.so.1");
  for (const required of [
    executable,
    path.join(contentDir, "client/index.html"),
    ...(platform === "linux" ? [bundledLibatomic, `${bundledLibatomic}.LICENSE`] : []),
  ]) {
    if (!(yield* fs.exists(required))) {
      return yield* new CliArchiveSmokeError({
        step: "checking the archive layout",
        detail: `missing ${path.relative(contentDir, required)}`,
      });
    }
  }

  // The runner has libatomic installed; fresh Ubuntu servers do not.
  if (platform === "linux") {
    const ldd = yield* spawner.string(ChildProcess.make("ldd", [executable]));
    // Older glibc prints the path as given, newer prints it resolved; compare both resolved.
    const loaded = /libatomic\.so\.1 => (\/.*) \(0x[0-9a-f]+\)/.exec(ldd)?.[1];
    if (
      loaded === undefined ||
      (yield* fs.realPath(loaded)) !== (yield* fs.realPath(bundledLibatomic))
    ) {
      return yield* new CliArchiveSmokeError({
        step: "checking that hanzo loads the bundled libatomic",
        detail: ldd,
      });
    }
  }

  const version = yield* runExecutable(executable, ["--version"], contentDir);
  if (version.exitCode !== 0 || !version.stdout.includes(input.expectVersion)) {
    return yield* new CliArchiveSmokeError({
      step: "running --version",
      detail: `exit ${String(version.exitCode)}\n${version.stdout}${version.stderr}`,
    });
  }

  // The service launcher loads outside the embedded CLI; without HANZO_DEV_HOME it exits right away.
  const launcher = yield* runExecutable(executable, ["__service-launcher"], contentDir);
  if (launcher.exitCode !== 1 || !launcher.stderr.includes("HANZO_DEV_HOME is required")) {
    return yield* new CliArchiveSmokeError({
      step: "running __service-launcher",
      detail: `exit ${String(launcher.exitCode)}\n${launcher.stdout}${launcher.stderr}`,
    });
  }

  const served = yield* smokePackagedServer({
    cli: { command: executable, args: [], cwd: contentDir },
    scratch: path.join(scratch, "serve"),
    // The archive carries the web client, so the server's root answers too.
    readyPath: "/",
  });
  yield* Effect.log(
    `[cli-smoke] ${root}: --version and __service-launcher passed, and serve on ${String(served.port)} dispatched a turn.`,
  );
});

const command = Command.make(
  "smoke-cli-archive",
  {
    archive: Flag.String("archive"),
    expectVersion: Flag.String("expect-version"),
  },
  (input) => smokeCliArchive(input).pipe(Effect.scoped),
).pipe(Command.withDescription("Extract a CLI archive and run its executable."));

if (import.meta.main) {
  Command.run(command, { version: "0.0.0" }).pipe(
    Effect.provide(
      Layer.mergeAll(
        Logger.layer([Logger.consolePretty()]),
        NodeServices.layer,
        NetService.layer,
        FetchHttpClient.layer,
      ),
    ),
    NodeRuntime.runMain,
  );
}
