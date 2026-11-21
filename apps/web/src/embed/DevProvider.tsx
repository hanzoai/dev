import { createMemoryHistory, RouterProvider } from "@tanstack/react-router";
import { useEffect, useMemo, useState, type ReactNode } from "react";

import type { EnvironmentId } from "@hanzodev/contracts";
import { startAppServices } from "../appInit";
import { connectPairing } from "../connection/onboarding";
import { I18nProvider } from "../i18n/I18nProvider";
import { whenLocaleReady } from "../i18n/runtime";
import { AppAtomRegistryProvider } from "../rpc/atomRegistry";
import { useAtomCommand } from "../state/use-atom-command";
import { createSlot, EmbedContext, markEmbedded, type EmbedState } from "./context";
import type { DevRuntime } from "./runtime";

// Services are app-lifetime singletons; one router per page.
let started: { readonly router: EmbedState["router"] } | null = null;

function start() {
  if (started === null) {
    markEmbedded();
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
  return new Promise((resolve) => waiting.set(runtimeId, [...(waiting.get(runtimeId) ?? []), resolve]));
}

/** Redeems the runtime's pairing credential once per session, then draws the core. */
function Pair({ runtime, children }: { runtime: DevRuntime; children: ReactNode }) {
  const pair = useAtomCommand(connectPairing, { reportFailure: false });
  const [failure, setFailure] = useState<string | null>(null);
  const [ready, setReady] = useState(paired.has(runtime.id));
  useEffect(() => {
    if (paired.has(runtime.id)) return;
    let live = true;
    void (async () => {
      const result = await pair({ host: runtime.zap.replace(/^https:/, "zaps:"), pairingCode: await runtime.token() });
      if (!live) return;
      if (result._tag === "Success") {
        paired.set(runtime.id, result.value);
        for (const resolve of waiting.get(runtime.id) ?? []) resolve(result.value);
        waiting.delete(runtime.id);
        setReady(true);
      } else {
        setFailure(`Could not connect to ${runtime.label}.`);
      }
    })();
    return () => {
      live = false;
    };
  }, [pair, runtime]);
  if (failure) return <p role="alert">{failure}</p>;
  return ready ? <>{children}</> : null;
}

/**
 * The Dev core for a host. Mount once, high in the tree; draw `DevThreadList` and
 * `DevWorkspace` anywhere under it. They share one router, one connection and one
 * atom registry, so a thread opened in the list is the one the workspace shows.
 */
export function DevProvider({ runtime, children }: { runtime: DevRuntime; children: ReactNode }) {
  const router = useMemo(start, []);
  const slots = useMemo(() => ({ threads: createSlot(), workspace: createSlot() }), []);
  const state = useMemo(() => ({ runtime, router, slots }), [runtime, router, slots]);
  const [ready, setReady] = useState(false);
  useEffect(() => {
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
          <Pair runtime={runtime}>{ready ? <RouterProvider router={router} /> : null}</Pair>
          {children}
        </AppAtomRegistryProvider>
      </I18nProvider>
    </EmbedContext.Provider>
  );
}
