// The one interface between a host and the Dev core: a machine (Hanzo Connect) or a cloud cell
// (`/v1/dev`, HIP-1330), reached over ZAP on WebTransport with the IAM token as the first `cap`.
// Wire framing: docs/internals/embed-runtime.md. A null `resolve` keeps the host's previous view.

export interface DevRuntime {
  readonly kind: "machine" | "cell";
  /** Stable id of the environment; keys the thread list and local caches. */
  readonly id: string;
  /** Shown beside the thread list. */
  readonly label: string;
  /** The runtime's ZAP endpoint: an `https://` URL that accepts WebTransport sessions, used
   *  exactly as given and never built from parts (a cell: what `POST /v1/dev/sessions`
   *  returns, `https://zap.hanzo.ai/v1/dev/zap/<id>`; a machine: its Connect tunnel). */
  readonly zap: string;
  /** The person's Hanzo IAM access token, fresh per call. Presented as the `cap` of the
   *  first envelope of a session; never stored by the core. */
  readonly token: () => Promise<string>;
  /** Sandbox that backs the dock (`/v1/sandboxes/:id/{terminal|screen}/ticket`).
   *  Absent: the dock shows only what the runtime itself serves. */
  readonly sandbox?: string;
}

export interface DevRuntimeSource {
  /** The runtime this person has now: a linked machine first, else a cloud cell, else null. */
  resolve(signal: AbortSignal): Promise<DevRuntime | null>;
  /** Runtimes the person may switch between (linked machines, their cell). */
  list(signal: AbortSignal): Promise<readonly DevRuntime[]>;
}
