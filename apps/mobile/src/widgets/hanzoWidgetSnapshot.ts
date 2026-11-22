export interface HanzoWidgetMachine {
  readonly id: string;
  readonly name: string;
  readonly online: boolean;
  readonly kind: "cloud" | "desktop" | "local";
}

export interface HanzoWidgetThread {
  readonly key: string;
  readonly title: string;
  readonly preview: string;
  readonly updatedAt: number;
  readonly pinned?: boolean;
}

export interface HanzoWidgetSnapshot {
  readonly signedIn: boolean;
  readonly accountName: string | null;
  readonly accountEmail: string | null;
  readonly cloudAvailable: boolean;
  readonly cloudModel: string;
  readonly connectionStatus: string;
  readonly environmentsCount: number;
  readonly onlineMachinesCount: number;
  readonly machines: ReadonlyArray<HanzoWidgetMachine>;
  readonly recentThreads: ReadonlyArray<HanzoWidgetThread>;
  readonly updatedAt: number;
}

export function buildWidgetConnectionStatus(input: {
  readonly signedIn: boolean;
  readonly onlineMachinesCount: number;
  readonly totalMachinesCount: number;
}): string {
  /* oxlint-disable hanzo/no-untranslated-copy -- Status line for the native Swift widget, which shows it beside its own English copy and feeds it to its on-device model prompt. */
  if (!input.signedIn) {
    return input.onlineMachinesCount > 0
      ? `${input.onlineMachinesCount} machine${input.onlineMachinesCount === 1 ? "" : "s"} connected`
      : "Sign in to connect Hanzo Cloud";
  }
  if (input.onlineMachinesCount > 0) {
    return `Hanzo Cloud · ${input.onlineMachinesCount} Mac${input.onlineMachinesCount === 1 ? "" : "s"} online`;
  }
  if (input.totalMachinesCount > 0) {
    return `Hanzo Cloud · ${input.totalMachinesCount} linked`;
  }
  return "Hanzo Cloud · Ready";
  /* oxlint-enable hanzo/no-untranslated-copy */
}
