import { assert, describe, it } from "@effect/vitest";
import * as Deferred from "effect/Deferred";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import { vi } from "vite-plus/test";

const { handlers, registerMock, unregisterMock } = vi.hoisted(() => {
  const map = new Map<string, () => void>();
  return {
    handlers: map,
    registerMock: vi.fn((accelerator: string, cb: () => void) => {
      map.set(accelerator, cb);
      return true;
    }),
    unregisterMock: vi.fn((accelerator: string) => {
      map.delete(accelerator);
    }),
  };
});

vi.mock("electron", () => ({
  globalShortcut: {
    register: registerMock,
    unregister: unregisterMock,
  },
}));

import * as DesktopWindow from "../window/DesktopWindow.ts";
import * as DesktopGlobalShortcuts from "./DesktopGlobalShortcuts.ts";

describe("DesktopGlobalShortcuts", () => {
  it.effect(
    "registers CommandOrControl+Shift+I and CommandOrControl+Shift+J, dispatches menu actions, and cleans up on scope close",
    () =>
      Effect.gen(function* () {
        handlers.clear();
        registerMock.mockClear();
        unregisterMock.mockClear();

        const firstAction = yield* Deferred.make<string>();
        const secondAction = yield* Deferred.make<string>();
        let callCount = 0;

        const windowLayer = Layer.succeed(DesktopWindow.DesktopWindow, {
          createMain: Effect.die("unused"),
          ensureMain: Effect.die("unused"),
          revealOrCreateMain: Effect.die("unused"),
          activate: Effect.void,
          createMainIfBackendReady: Effect.void,
          showConnectingSplash: Effect.void,
          handleBackendReady: () => Effect.void,
          handleBackendNotReady: Effect.void,
          flushMainWindowBounds: Effect.void,
          prepareCaptureReveal: Effect.void,
          dispatchMenuAction: (action) => {
            callCount += 1;
            return Deferred.succeed(callCount === 1 ? firstAction : secondAction, action).pipe(
              Effect.asVoid,
            );
          },
          dispatchSnapShotEvent: () => Effect.void,
          zoomMain: () => Effect.void,
          runMainContentsCommand: () => Effect.void,
          syncAppearance: Effect.void,
        } satisfies DesktopWindow.DesktopWindow["Service"]);

        yield* Effect.scoped(
          Effect.gen(function* () {
            const shortcuts = yield* DesktopGlobalShortcuts.DesktopGlobalShortcuts;
            yield* shortcuts.register;

            assert.isTrue(handlers.has(DesktopGlobalShortcuts.GLOBAL_NEW_CHAT_ACCELERATOR));
            assert.isTrue(handlers.has(DesktopGlobalShortcuts.GLOBAL_SPOTLIGHT_ACCELERATOR));

            handlers.get(DesktopGlobalShortcuts.GLOBAL_NEW_CHAT_ACCELERATOR)?.();
            assert.strictEqual(yield* Deferred.await(firstAction), "global-new-chat");

            handlers.get(DesktopGlobalShortcuts.GLOBAL_SPOTLIGHT_ACCELERATOR)?.();
            assert.strictEqual(yield* Deferred.await(secondAction), "global-spotlight");
          }),
        ).pipe(
          Effect.provide(DesktopGlobalShortcuts.layer.pipe(Layer.provideMerge(windowLayer))),
        );

        assert.strictEqual(unregisterMock.mock.calls.length, 2);
        assert.strictEqual(handlers.size, 0);
      }),
  );
});
