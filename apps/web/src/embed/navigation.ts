import { useEffect, useSyncExternalStore } from "react";

import type { EmbedState } from "./context";

const THREAD = /^\/([^/]+)\/([^/]+)$/;
const RESERVED = new Set(["settings", "threads", "projects"]);

/** `<environmentId>/<threadId>` for a thread's address, null for anything else. */
export function refOf(pathname: string): string | null {
  const match = THREAD.exec(pathname);
  return match && !RESERVED.has(match[1]!) ? `${match[1]}/${match[2]}` : null;
}

function pathOf(ref: string | null): string {
  return ref === null ? "/" : `/${ref}`;
}

/**
 * Keeps the core's router and the host's address in step: the router is read for the open
 * thread, and `thread` (when it differs) moves it. A move the person makes inside the core
 * reaches the host through `onChange`.
 */
export function useThreadRef(
  embed: EmbedState | null,
  thread: string | null | undefined,
  onChange: ((ref: string | null) => void) | undefined,
): string | null {
  const router = embed?.router ?? null;
  const open = useSyncExternalStore(
    (notify) => (router ? router.history.subscribe(notify) : () => {}),
    () => (router ? refOf(router.history.location.pathname) : null),
    () => null,
  );
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- moves the core's router to the host's address
  useEffect(() => {
    if (router && thread !== undefined && thread !== open) {
      void router.navigate({ to: pathOf(thread) });
    }
    // The host's value moves the router; a router move is reported by the effect below.
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- `open` is read, not reacted to.
  }, [router, thread]);
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- reports a router move to the host
  useEffect(() => {
    if (open !== (thread ?? null)) onChange?.(open);
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- reports router moves only.
  }, [open]);
  return open;
}
