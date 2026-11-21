export interface CloudChatModel {
  readonly id: string;
  readonly label: string;
}

export interface CloudChat {
  /** Whether a prompt can be answered without an environment. */
  readonly available: boolean;
  readonly models: readonly CloudChatModel[];
}

/**
 * The seam to chat that needs no environment. Mobile has no client for it yet: the Hanzo provider
 * runs inside an environment's server, and there is no environment-free thread runtime or model
 * list behind api.hanzo.ai that the phone can call. When one exists this returns it, and the
 * landing sends through it with no other change.
 */
export function useCloudChat(): CloudChat {
  return { available: false, models: [] };
}
