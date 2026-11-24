import * as Duration from "effect/Duration";
import * as Effect from "effect/Effect";
import * as Fiber from "effect/Fiber";
import * as FileSystem from "effect/FileSystem";
import * as Path from "effect/Path";
import * as Schema from "effect/Schema";
import * as Stream from "effect/Stream";
import { HttpClient, HttpClientRequest } from "effect/http";
import { ChildProcess, ChildProcessSpawner } from "effect/process";

import * as NetService from "@hanzodev/shared/Net";

export class PackagedServerSmokeError extends Schema.TaggedError<PackagedServerSmokeError>()(
  "PackagedServerSmokeError",
  { step: Schema.String, detail: Schema.String },
) {
  override get message(): string {
    return `Packaged server smoke test failed while ${this.step}: ${this.detail}`;
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

/** How to run the packaged CLI: the single executable, or the bundled Node with `bin.mjs`. */
export interface PackagedCli {
  readonly command: string;
  readonly args: ReadonlyArray<string>;
  readonly cwd: string;
}

const LaunchedThread = Schema.Struct({
  threadId: Schema.String,
  runId: Schema.NullOr(Schema.String),
});
const decodeLaunchedThread = Schema.decodeUnknownEffect(Schema.fromJsonString(LaunchedThread));
const ShownThread = Schema.Struct({
  messages: Schema.Array(Schema.Struct({ role: Schema.String, text: Schema.String })),
});
const decodeShownThread = Schema.decodeUnknownEffect(Schema.fromJsonString(ShownThread));

const SMOKE_MESSAGE = "Reply with OK.";

/**
 * Serves the packaged server, adds a project and dispatches one turn. The dispatch decides through
 * `dev.wasm`, so a bundle that cannot load it fails here. Codex is not on the empty PATH: its
 * session fails after the dispatch, and the thread keeps the run and the message.
 */
export const smokePackagedServer = Effect.fn("smokePackagedServer")(function* (input: {
  readonly cli: PackagedCli;
  readonly scratch: string;
  /** The path that answers 200 once the server is up. `/` also needs the web client beside it. */
  readonly readyPath?: string;
}) {
  const readyPath = input.readyPath ?? "/.well-known/hanzo/environment";
  const fs = yield* FileSystem.FileSystem;
  const path = yield* Path.Path;
  const spawner = yield* ChildProcessSpawner.ChildProcessSpawner;
  const home = path.join(input.scratch, "home");
  const project = path.join(input.scratch, "project");
  yield* fs.makeDirectory(project, { recursive: true });
  yield* fs.writeFileString(path.join(project, "README.md"), "# smoke\n");
  const env = {
    PATH: "",
    HOME: home,
    USERPROFILE: home,
    TMPDIR: input.scratch,
    TEMP: input.scratch,
    HANZO_DEV_HOME: home,
    // A packaged build has a release channel built in, so its events would
    // look like a real new install.
    HANZO_DEV_TELEMETRY_ENABLED: "false",
  };

  const runCli = Effect.fn("smokePackagedServer.runCli")(function* (args: ReadonlyArray<string>) {
    const child = yield* spawner.spawn(
      ChildProcess.make(input.cli.command, [...input.cli.args, ...args], {
        cwd: input.cli.cwd,
        env,
        extendEnv: false,
      }),
    );
    const [stdout, stderr, exitCode] = yield* Effect.all(
      [collect(child.stdout), collect(child.stderr), child.exitCode.pipe(Effect.map(Number))],
      { concurrency: "unbounded" },
    ).pipe(
      Effect.timeout(Duration.seconds(60)),
      Effect.catchTags({
        TimeoutError: () => Effect.succeed(["", "timed out after 60s", -1] as const),
      }),
    );
    return { stdout, stderr, exitCode };
  });

  const net = yield* NetService.NetService;
  const port = yield* net.findAvailablePort(47700);
  const server = yield* spawner.spawn(
    ChildProcess.make(
      input.cli.command,
      [...input.cli.args, "serve", "--host", "127.0.0.1", "--port", String(port), "--no-browser"],
      { cwd: input.cli.cwd, env, extendEnv: false },
    ),
  );
  const output = yield* Effect.forkScoped(
    Effect.all([collect(server.stdout), collect(server.stderr)]),
  );
  const stopServer = Effect.gen(function* () {
    yield* server.kill({ killSignal: "SIGTERM" }).pipe(Effect.ignore);
    yield* server.exitCode.pipe(Effect.timeout(Duration.seconds(10)), Effect.ignore);
    const [stdout, stderr] = yield* Fiber.join(output).pipe(
      Effect.timeout(Duration.seconds(5)),
      Effect.orElseSucceed(() => ["", ""] as const),
    );
    return `${stdout}${stderr}`;
  });
  const fail = (step: string, detail: string) =>
    stopServer.pipe(
      Effect.flatMap((log) =>
        Effect.fail(new PackagedServerSmokeError({ step, detail: `${detail}\n${log}` })),
      ),
    );

  const httpClient = yield* HttpClient.HttpClient;
  // Probes can hang during server initialization; bound each request as SSH readiness does.
  const probe = httpClient
    .execute(HttpClientRequest.get(`http://127.0.0.1:${String(port)}${readyPath}`))
    .pipe(
      Effect.map((response) => response.status === 200),
      Effect.timeout(Duration.seconds(2)),
      Effect.orElseSucceed(() => false),
    );
  const pollUntilReady = Effect.gen(function* () {
    while (!(yield* probe)) {
      yield* Effect.sleep(Duration.millis(250));
    }
    return true;
  });
  const ready = yield* pollUntilReady.pipe(
    Effect.timeout(Duration.seconds(30)),
    Effect.orElseSucceed(() => false),
  );
  if (!ready) return yield* fail("serving", `no 200 from ${readyPath} within 30s`);

  const added = yield* runCli(["project", "add", project]);
  if (added.exitCode !== 0) {
    return yield* fail(
      "adding a project",
      `exit ${String(added.exitCode)}\n${added.stdout}${added.stderr}`,
    );
  }

  const started = yield* runCli([
    "thread",
    "start",
    project,
    SMOKE_MESSAGE,
    "--provider",
    "codex",
    "--model",
    "gpt-5",
    "--json",
  ]);
  const launched =
    started.exitCode === 0
      ? yield* decodeLaunchedThread(started.stdout.trim()).pipe(Effect.option)
      : undefined;
  if (launched === undefined || launched._tag === "None" || launched.value.runId === null) {
    return yield* fail(
      "dispatching a turn",
      `exit ${String(started.exitCode)}\n${started.stdout}${started.stderr}`,
    );
  }

  const shown = yield* runCli(["thread", "show", launched.value.threadId, "--json"]);
  const thread =
    shown.exitCode === 0
      ? yield* decodeShownThread(shown.stdout.trim()).pipe(Effect.option)
      : undefined;
  if (
    thread === undefined ||
    thread._tag === "None" ||
    !thread.value.messages.some(
      (message) => message.role === "user" && message.text === SMOKE_MESSAGE,
    )
  ) {
    return yield* fail(
      "reading the dispatched turn",
      `exit ${String(shown.exitCode)}\n${shown.stdout}${shown.stderr}`,
    );
  }

  yield* stopServer;
  return { port, threadId: launched.value.threadId };
});
