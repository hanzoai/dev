import { useAtomValue } from "@effect/atom-react";
import { Atom } from "effect/reactivity";

import { writeFileAtomically } from "../lib/atomic-file";
import { appAtomRegistry } from "./atom-registry";

const MEMORY_SWITCH_DIRECTORY = "composer-drafts";
const MEMORY_SWITCH_FILE = "memory-switch.json";

export const threadMemorySwitchAtom = Atom.make<Readonly<Record<string, boolean>>>({}).pipe(
  Atom.keepAlive,
);

let hydrated = false;

async function getMemorySwitchFile() {
  const { Directory, File, Paths } = await import("expo-file-system");
  const directory = new Directory(Paths.document, MEMORY_SWITCH_DIRECTORY);
  directory.create({ idempotent: true, intermediates: true });
  return new File(directory, MEMORY_SWITCH_FILE);
}

function hydrateOnce(): void {
  if (hydrated) return;
  hydrated = true;
  void (async () => {
    try {
      const file = await getMemorySwitchFile();
      if (!file.exists) return;
      const raw = await file.text();
      const parsed = JSON.parse(raw) as unknown;
      if (typeof parsed !== "object" || parsed === null || Array.isArray(parsed)) return;
      const persisted: Record<string, boolean> = {};
      for (const [key, value] of Object.entries(parsed as Record<string, unknown>)) {
        if (value === true) persisted[key] = true;
      }
      const current = appAtomRegistry.get(threadMemorySwitchAtom);
      appAtomRegistry.set(threadMemorySwitchAtom, { ...persisted, ...current });
    } catch {
      // Best-effort hydration; never break composer rendering on storage errors.
    }
  })();
}

function persistMemorySwitch(state: Readonly<Record<string, boolean>>): void {
  void (async () => {
    try {
      const file = await getMemorySwitchFile();
      await writeFileAtomically(file, JSON.stringify(state));
    } catch {
      // Best-effort persistence.
    }
  })();
}

export function isThreadMemoryEnabled(key: string): boolean {
  hydrateOnce();
  return appAtomRegistry.get(threadMemorySwitchAtom)[key] === true;
}

export function setThreadMemoryEnabled(key: string, enabled: boolean): void {
  hydrateOnce();
  const current = appAtomRegistry.get(threadMemorySwitchAtom);
  if ((current[key] === true) === enabled) return;
  const next = { ...current };
  if (enabled) {
    next[key] = true;
  } else {
    delete next[key];
  }
  appAtomRegistry.set(threadMemorySwitchAtom, next);
  persistMemorySwitch(next);
}

export function toggleThreadMemoryEnabled(key: string): boolean {
  const next = !isThreadMemoryEnabled(key);
  setThreadMemoryEnabled(key, next);
  return next;
}

export function transferThreadMemorySetting(fromKey: string, toKey: string): void {
  if (fromKey === toKey) return;
  const current = appAtomRegistry.get(threadMemorySwitchAtom);
  if (current[fromKey] !== true) return;
  const next = { ...current };
  delete next[fromKey];
  next[toKey] = true;
  appAtomRegistry.set(threadMemorySwitchAtom, next);
  persistMemorySwitch(next);
}

export function useThreadMemoryEnabled(key: string): boolean {
  hydrateOnce();
  return useAtomValue(threadMemorySwitchAtom)[key] === true;
}
