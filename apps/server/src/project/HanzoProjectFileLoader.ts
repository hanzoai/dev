import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";
import * as Path from "effect/Path";
import * as Schema from "effect/Schema";

import { HANZO_PROJECT_FILE_NAME, type HanzoProjectFile } from "@hanzodev/contracts";
import { HanzoProjectFileFromJson } from "@hanzodev/shared/hanzoProjectFile";

const decodeHanzoProjectFileJson = Schema.decodeEffect(HanzoProjectFileFromJson);

export class HanzoProjectFileLoadError extends Schema.TaggedError<HanzoProjectFileLoadError>()(
  "HanzoProjectFileLoadError",
  {
    operation: Schema.Literals(["read", "decode"]),
    workspaceRoot: Schema.String,
    filePath: Schema.String,
    cause: Schema.Defect(),
  },
) {
  override get message(): string {
    return `Failed to ${this.operation} ${HANZO_PROJECT_FILE_NAME} at ${this.filePath}.`;
  }
}

export class HanzoProjectFileLoader extends Context.Service<
  HanzoProjectFileLoader,
  {
    /** Never fails; missing/unreadable/invalid files return none, with warnings for invalid files. */
    readonly load: (workspaceRoot: string) => Effect.Effect<Option.Option<HanzoProjectFile>>;
  }
>()("hanzo/project/HanzoProjectFileLoader") {}

const logHanzoProjectFileLoadError = (error: HanzoProjectFileLoadError) =>
  Effect.logWarning(error).pipe(
    Effect.annotateLogs({
      operation: error.operation,
      workspaceRoot: error.workspaceRoot,
      filePath: error.filePath,
      errorTag: error._tag,
    }),
  );

/** @public */
export const make = Effect.gen(function* () {
  const fileSystem = yield* FileSystem.FileSystem;
  const path = yield* Path.Path;

  const load: HanzoProjectFileLoader["Service"]["load"] = Effect.fn("HanzoProjectFileLoader.load")(
    function* (workspaceRoot) {
      const filePath = path.join(workspaceRoot, HANZO_PROJECT_FILE_NAME);
      const raw = yield* fileSystem.readFileString(filePath).pipe(
        Effect.asSome,
        Effect.catchTags({
          PlatformError: (error) =>
            error.reason._tag === "NotFound"
              ? Effect.succeed(Option.none<string>())
              : logHanzoProjectFileLoadError(
                  new HanzoProjectFileLoadError({
                    operation: "read",
                    workspaceRoot,
                    filePath,
                    cause: error,
                  }),
                ).pipe(Effect.as(Option.none<string>())),
        }),
      );
      if (Option.isNone(raw)) {
        return Option.none<HanzoProjectFile>();
      }
      return yield* decodeHanzoProjectFileJson(raw.value).pipe(
        Effect.asSome,
        Effect.catchTags({
          SchemaError: (error) =>
            logHanzoProjectFileLoadError(
              new HanzoProjectFileLoadError({
                operation: "decode",
                workspaceRoot,
                filePath,
                cause: error,
              }),
            ).pipe(Effect.as(Option.none<HanzoProjectFile>())),
        }),
      );
    },
  );

  return HanzoProjectFileLoader.of({ load });
});

export const layer = Layer.effect(HanzoProjectFileLoader, make);
