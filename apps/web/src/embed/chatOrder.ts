import type { ThreadSummary } from "@hanzodev/client-runtime/cloud-chat";

/** Pinned first, then newest: the order every surface lists conversations in. */
export function orderThreads(threads: ReadonlyArray<ThreadSummary>): ReadonlyArray<ThreadSummary> {
  return threads.toSorted((a, b) => Number(b.pinned) - Number(a.pinned) || b.updatedAt - a.updatedAt);
}

/** The thread a host address names: a remote conversation id or a device key. */
export function threadFor(
  threads: ReadonlyArray<ThreadSummary>,
  ref: string | null,
): ThreadSummary | null {
  if (ref === null) return null;
  return threads.find((entry) => entry.remoteId === ref || entry.key === ref) ?? null;
}

/** The address a host keeps for a thread: its remote id once the cloud holds it, else its device key. */
export function refOfThread(entry: ThreadSummary): string {
  return entry.remoteId ?? entry.key;
}
