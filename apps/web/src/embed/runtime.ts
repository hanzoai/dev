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
// Both speak the same Dev environment protocol (`/ws` + `/api`, packages/contracts),
// authenticated with the person's Hanzo IAM access token. When `resolve` returns
// null the host keeps its previous Dev view: no runtime is not an empty screen.

export interface DevRuntime {
  readonly kind: "machine" | "cell";
  /** Stable id of the environment; keys the thread list and local caches. */
  readonly id: string;
  /** Shown beside the thread list. */
  readonly label: string;
  /** HTTPS origin that serves the Dev environment's `/ws`, `/api`, `/.well-known`. */
  readonly host: string;
  /** Bearer for `host`: the person's Hanzo IAM access token, or the pairing credential
   *  the relay minted. Never stored by the core beyond the session. */
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
