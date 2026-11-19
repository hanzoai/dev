import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import type * as Scope from "effect/Scope";
import * as Electron from "electron";

import { makeComponentLogger } from "./DesktopObservability.ts";
import * as DesktopWindow from "../window/DesktopWindow.ts";

export const GLOBAL_NEW_CHAT_ACCELERATOR = "CommandOrControl+Shift+I";
export const GLOBAL_SPOTLIGHT_ACCELERATOR = "CommandOrControl+Shift+J";

export const GLOBAL_SHORTCUT_BINDINGS = [
  {
    accelerator: GLOBAL_NEW_CHAT_ACCELERATOR,
    action: "global-new-chat",
  },
  {
    accelerator: GLOBAL_SPOTLIGHT_ACCELERATOR,
    action: "global-spotlight",
  },
] as const;

export class DesktopGlobalShortcuts extends Context.Service<
  DesktopGlobalShortcuts,
  {
    readonly register: Effect.Effect<void, never, Scope.Scope>;
  }
>()("@hanzodev/desktop/app/DesktopGlobalShortcuts") {}

const { logInfo, logWarning } = makeComponentLogger("desktop-global-shortcuts");

export const make = Effect.gen(function* () {
  const desktopWindow = yield* DesktopWindow.DesktopWindow;
  const context = yield* Effect.context<DesktopWindow.DesktopWindow>();
  const runPromise = Effect.runPromiseWith(context);

  const register = Effect.acquireRelease(
    Effect.gen(function* () {
      const registered: string[] = [];
      for (const binding of GLOBAL_SHORTCUT_BINDINGS) {
        const ok = yield* Effect.sync(() => {
          try {
            return Electron.globalShortcut.register(binding.accelerator, () => {
              void runPromise(
                desktopWindow
                  .dispatchMenuAction(binding.action, { reveal: true })
                  .pipe(Effect.ignoreCause),
              );
            });
          } catch {
            return false;
          }
        });
        if (ok) {
          registered.push(binding.accelerator);
        } else {
          yield* logWarning("could not register desktop global shortcut", {
            accelerator: binding.accelerator,
            action: binding.action,
          });
        }
      }
      if (registered.length > 0) {
        yield* logInfo("registered desktop global shortcuts", { accelerators: registered });
      }
      return registered;
    }),
    (registered) =>
      Effect.sync(() => {
        for (const accelerator of registered) {
          try {
            Electron.globalShortcut.unregister(accelerator);
          } catch {
            // Ignore shutdown teardown errors after Electron destroys globalShortcut.
          }
        }
      }),
  ).pipe(Effect.asVoid, Effect.withSpan("desktop.globalShortcuts.register"));

  return DesktopGlobalShortcuts.of({ register });
});

export const layer = Layer.effect(DesktopGlobalShortcuts, make);
