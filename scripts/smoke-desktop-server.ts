#!/usr/bin/env node
import * as NodeRuntime from "@effect/platform-node/NodeRuntime";
import * as NodeServices from "@effect/platform-node/NodeServices";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Layer from "effect/Layer";
import * as Logger from "effect/Logger";
import * as Path from "effect/Path";
import { Command, Flag } from "effect/cli";
import { FetchHttpClient } from "effect/http";

import * as NetService from "@hanzodev/shared/Net";
import { PackagedServerSmokeError, smokePackagedServer } from "./lib/packaged-server-smoke.ts";

/** Runs the desktop app's own Node and server bundle, as the app does, and dispatches one turn. */
const smokeDesktopServer = Effect.fn("smokeDesktopServer")(function* (input: {
  readonly node: string;
  readonly server: string;
}) {
  const fs = yield* FileSystem.FileSystem;
  const path = yield* Path.Path;
  const node = path.resolve(input.node);
  const server = path.resolve(input.server);
  for (const required of [node, server]) {
    if (!(yield* fs.exists(required))) {
      return yield* new PackagedServerSmokeError({
        step: "checking the app bundle",
        detail: `missing ${required}`,
      });
    }
  }
  const scratch = yield* fs.makeTempDirectoryScoped({ prefix: "hanzo-desktop-smoke-" });
  const served = yield* smokePackagedServer({
    cli: { command: node, args: [server], cwd: path.dirname(server) },
    scratch,
  });
  yield* Effect.log(
    `[desktop-smoke] the bundled server on ${String(served.port)} dispatched a turn.`,
  );
});

const command = Command.make(
  "smoke-desktop-server",
  {
    node: Flag.String("node").pipe(Flag.withDescription("The app's bundled Node executable.")),
    server: Flag.String("server").pipe(Flag.withDescription("The app's server/dist/bin.mjs.")),
  },
  (input) => smokeDesktopServer(input).pipe(Effect.scoped),
).pipe(Command.withDescription("Serve the desktop app's bundled server and dispatch one turn."));

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
