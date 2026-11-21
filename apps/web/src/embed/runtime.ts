// The one interface between a host (hanzo.ai/dev) and the Dev core.
//
// A host resolves a `DevRuntime` for the signed-in person and hands it to
// `DevProvider`. Two kinds implement it:
//
//   machine  the person's own computer, linked through Hanzo Connect
//            (relay brokers a DPoP-bound bootstrap; the tunnel hostname is `host`).
//   cell     a Hanzo cloud sandbox leased through `POST /v1/dev` (HIP-1330),
//            running `hanzo-dev serve`; the cloud bridges its WebSocket at `host`.
//
// Both speak the same Dev environment protocol (`/ws` + `/api`, packages/contracts);
// the person's Hanzo IAM identity is what lets the relay or `/v1/dev` mint the pairing credential. When `resolve` returns
// null the host keeps its previous Dev view: no runtime is not an empty screen.

export interface DevRuntime {
  readonly kind: "machine" | "cell";
  /** Stable id of the environment; keys the thread list and local caches. */
  readonly id: string;
  /** Shown beside the thread list. */
  readonly label: string;
  /** HTTPS origin that serves the Dev environment's `/ws`, `/api`, `/.well-known`. */
  readonly host: string;
  /** Mints a fresh one-time pairing credential for `host` (the one `/pair` redeems): the relay's
   *  bootstrap for a machine, `/v1/dev`'s for a cell. The core redeems it once per session and
   *  keeps the environment session it gets back; it never keeps the credential. */
  readonly token: () => Promise<string>;
  /** Sandbox that backs the dock (`/v1/sandboxes/:id/{terminal|screen}/ticket`).
   *  Absent: the dock shows only what `host` itself serves. */
  readonly sandbox?: string;
}

export interface DevRuntimeSource {
  /** The runtime this person has now: a linked machine first, else a cloud cell, else null. */
  resolve(signal: AbortSignal): Promise<DevRuntime | null>;
  /** Runtimes the person may switch between (linked machines, their cell). */
  list(signal: AbortSignal): Promise<readonly DevRuntime[]>;
}
