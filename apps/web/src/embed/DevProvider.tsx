import "./embedded";
import { createMemoryHistory, RouterProvider } from "@tanstack/react-router";
import { useEffect, useState, useSyncExternalStore, type ReactNode } from "react";
import { createPortal } from "react-dom";

import type { EnvironmentId } from "@hanzodev/contracts";
import { startAppServices } from "../appInit";
import { connectPairing } from "../connection/onboarding";
import { I18nProvider } from "../i18n/I18nProvider";
import { whenLocaleReady } from "../i18n/runtime";
import { AppAtomRegistryProvider } from "../rpc/atomRegistry";
import { useAtomCommand } from "../state/use-atom-command";
import { createSlot, EmbedContext, useEmbed, type EmbedState } from "./context";
import type { DevRuntime } from "./runtime";

// Services are app-lifetime singletons; one router per page.
let started: { readonly router: EmbedState["router"] } | null = null;

function start() {
  if (started === null) {
    started = { router: startAppServices(createMemoryHistory({ initialEntries: ["/"] })) };
  }
  return started.router;
}

const paired = new Map<string, EnvironmentId>();
const waiting = new Map<string, Array<(id: EnvironmentId) => void>>();

/** The environment a runtime became once its pairing finished; resolves at once if it already has. */
export function whenPaired(runtimeId: string): Promise<EnvironmentId> {
  const done = paired.get(runtimeId);
  if (done) return Promise.resolve(done);
  return new Promise((resolve) =>
    waiting.set(runtimeId, [...(waiting.get(runtimeId) ?? []), resolve]),
  );
}

/** Dev's own state for a runtime that would not pair, drawn where the workspace goes. */
function Failure({ runtime, retry }: { runtime: DevRuntime; retry: () => void }) {
  const embed = useEmbed();
  const slot = embed?.slots.workspace;
  const target = useSyncExternalStore(
    (notify) => (slot ? slot.subscribe(notify) : () => {}),
    () => (slot ? slot.get() : null),
    () => null,
  );
  const body = (
    <div
      role="alert"
      className="flex min-h-0 min-w-0 flex-1 flex-col items-center justify-center gap-3 p-6 text-center"
    >
      <h2 className="text-lg font-medium text-foreground">Could not connect to {runtime.label}</h2>
      <p className="max-w-sm text-sm text-muted-foreground">
        The environment did not answer. Check that it is online, then try again.
      </p>
      <button
        type="button"
        onClick={retry}
        className="rounded-md border border-border px-3 py-1.5 text-sm text-foreground hover:bg-accent"
      >
        Retry
      </button>
    </div>
  );
  return target ? createPortal(body, target) : null;
}

/** Redeems the runtime's pairing credential once per session, then draws the core. */
function Pair({
  runtime,
  onFail,
  children,
}: {
  runtime: DevRuntime;
  onFail?: ((runtime: DevRuntime) => void) | undefined;
  children: ReactNode;
}) {
  const pair = useAtomCommand(connectPairing, { reportFailure: false });
  const [failed, setFailed] = useState(false);
  const [attempt, setAttempt] = useState(0);
  const [ready, setReady] = useState(paired.has(runtime.id));
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- redeems the pairing credential with the runtime once per session
  useEffect(() => {
    if (paired.has(runtime.id)) return;
    let live = true;
    setFailed(false);
    void (async () => {
      const result = await pair({
        host: runtime.zap.replace(/^https:/, "zaps:"),
        pairingCode: await runtime.token(),
      });
      if (!live) return;
      if (result._tag === "Success") {
        paired.set(runtime.id, result.value);
        for (const resolve of waiting.get(runtime.id) ?? []) resolve(result.value);
        waiting.delete(runtime.id);
        setReady(true);
      } else {
        setFailed(true);
        onFail?.(runtime);
      }
    })();
    return () => {
      live = false;
    };
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- `onFail` is the host's, read when the pairing fails.
  }, [pair, runtime, attempt]);
  if (failed) return <Failure runtime={runtime} retry={() => setAttempt((n) => n + 1)} />;
  return ready ? <>{children}</> : null;
}

/**
 * The Dev core for a host. Mount once, high in the tree; draw `DevThreadList` and
 * `DevWorkspace` anywhere under it. They share one router, one connection and one
 * atom registry, so a thread opened in the list is the one the workspace shows.
 */
export function DevProvider({
  runtime,
  onFail,
  children,
}: {
  /** Null until a machine runtime is active; keeping DevProvider mounted avoids remounting host children. */
  runtime: DevRuntime | null;
  /** The runtime would not pair: the host may try another (a machine, else a cloud cell). */
  onFail?: (runtime: DevRuntime) => void;
  children: ReactNode;
}) {
  const slots = { threads: createSlot(), workspace: createSlot() };
  const router = runtime ? start() : null;
  const state = runtime && router ? { runtime, router, slots } : null;
  const [ready, setReady] = useState(false);
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- waits on the router's load and the locale catalog while mounted
  useEffect(() => {
    if (!router) return;
    let live = true;
    void router
      .load()
      .then(whenLocaleReady)
      .then(() => live && setReady(true));
    return () => {
      live = false;
    };
  }, [router]);
  return (
    <EmbedContext.Provider value={state}>
      <I18nProvider>
        <AppAtomRegistryProvider>
          {runtime && router ? (
            <Pair runtime={runtime} onFail={onFail}>
              {ready ? <RouterProvider router={router} /> : null}
            </Pair>
          ) : null}
          {children}
        </AppAtomRegistryProvider>
      </I18nProvider>
    </EmbedContext.Provider>
  );
}
