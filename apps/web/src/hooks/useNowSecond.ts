import { secondTick } from "@hanzodev/client-runtime/clock";
import { useSyncExternalStore } from "react";

export function useNowSecond(): number {
  return useSyncExternalStore(secondTick.subscribe, secondTick.getSnapshot, secondTick.getSnapshot);
}
