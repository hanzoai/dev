// @effect-diagnostics globalDate:off -- Cross-window lease claims stamp wall-clock epoch ms so storage listeners observe monotonic updates.

export const DEFAULT_MIC_LEASE_CHANNEL = "hanzo-mic-lease";

export interface MicLeaseOptions {
  readonly instanceId: string;
  readonly onPreempted: () => void;
  readonly channelName?: string | undefined;
}

export interface MicLeaseMessage {
  readonly type: "claim" | "release";
  readonly owner: string;
  readonly at: number;
}

export interface MicLeaseArbitrator {
  readonly claim: () => void;
  readonly release: () => void;
  readonly isHolding: () => boolean;
  readonly handleIncoming: (msg: unknown) => void;
  readonly dispose: () => void;
}

/**
 * Arbitrates microphone ownership across open Hanzo Desktop and Web windows via
 * BroadcastChannel('hanzo-mic-lease') plus a localStorage storage-event fallback.
 */
export function createMicLeaseArbitrator(options: MicLeaseOptions): MicLeaseArbitrator {
  const channelName = options.channelName ?? DEFAULT_MIC_LEASE_CHANNEL;
  let holding = false;
  let sequence = 0;

  const bc = typeof BroadcastChannel !== "undefined" ? new BroadcastChannel(channelName) : null;

  const handleIncoming = (msg: unknown) => {
    if (!msg || typeof msg !== "object") return;
    const data = msg as { readonly type?: unknown; readonly owner?: unknown };
    if (
      data.type === "claim" &&
      typeof data.owner === "string" &&
      data.owner.length > 0 &&
      data.owner !== options.instanceId &&
      holding
    ) {
      holding = false;
      options.onPreempted();
    }
  };

  if (bc) {
    bc.onmessage = (event) => handleIncoming(event.data);
  }

  const onStorage = (event: StorageEvent) => {
    if (event.key !== channelName || !event.newValue) return;
    try {
      handleIncoming(JSON.parse(event.newValue));
    } catch {
      // Ignore malformed third-party storage payloads.
    }
  };

  if (typeof window !== "undefined" && typeof window.addEventListener === "function") {
    window.addEventListener("storage", onStorage);
  }

  const writeFallbackStorage = (payload: MicLeaseMessage) => {
    if (typeof window === "undefined") return;
    try {
      window.localStorage?.setItem(channelName, JSON.stringify(payload));
    } catch {
      // Ignore private-mode or sandboxed storage errors.
    }
  };

  const claim = () => {
    holding = true;
    sequence += 1;
    const payload: MicLeaseMessage = {
      type: "claim",
      owner: options.instanceId,
      at: Date.now() * 1000 + (sequence % 1000),
    };
    bc?.postMessage(payload);
    writeFallbackStorage(payload);
  };

  const release = () => {
    if (!holding) return;
    holding = false;
    sequence += 1;
    const payload: MicLeaseMessage = {
      type: "release",
      owner: options.instanceId,
      at: Date.now() * 1000 + (sequence % 1000),
    };
    bc?.postMessage(payload);
    writeFallbackStorage(payload);
  };

  const isHolding = () => holding;

  const dispose = () => {
    if (holding) {
      release();
    }
    bc?.close();
    if (typeof window !== "undefined" && typeof window.removeEventListener === "function") {
      window.removeEventListener("storage", onStorage);
    }
  };

  return { claim, release, isHolding, handleIncoming, dispose };
}
