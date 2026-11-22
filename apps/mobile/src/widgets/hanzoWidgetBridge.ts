import { requireOptionalNativeModule } from "expo";
import { Platform } from "react-native";

export {
  buildWidgetConnectionStatus,
  type HanzoWidgetMachine,
  type HanzoWidgetSnapshot,
  type HanzoWidgetThread,
} from "./hanzoWidgetSnapshot";
import type { HanzoWidgetSnapshot } from "./hanzoWidgetSnapshot";

interface HanzoNativeControlsWidgetModule {
  syncWidgetSnapshot?: (payloadJson: string) => Promise<boolean>;
}

let lastSyncedPayload: string | null = null;

export async function syncHanzoWidgetSnapshot(
  snapshot: HanzoWidgetSnapshot,
  force = false,
): Promise<boolean> {
  if (Platform.OS !== "ios") return false;
  const serialized = JSON.stringify({
    ...snapshot,
    updatedAt: 0,
  });
  if (!force && serialized === lastSyncedPayload) {
    return true;
  }
  const mod = requireOptionalNativeModule<HanzoNativeControlsWidgetModule>("HanzoNativeControls");
  if (!mod?.syncWidgetSnapshot) return false;
  try {
    const ok = await mod.syncWidgetSnapshot(JSON.stringify(snapshot));
    lastSyncedPayload = serialized;
    return ok;
  } catch {
    return false;
  }
}

export function resetHanzoWidgetSyncCacheForTesting(): void {
  lastSyncedPayload = null;
}
