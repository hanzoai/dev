// `use-sync-external-store` ships CommonJS that calls `require("react")`. React 19 has the hook
// itself, so the library build answers both entry points from React and bundles no CommonJS React.
import { useDebugValue, useMemo, useRef, useSyncExternalStore } from "react";

export { useSyncExternalStore };

export function useSyncExternalStoreWithSelector<Snapshot, Selection>(
  subscribe: (onStoreChange: () => void) => () => void,
  getSnapshot: () => Snapshot,
  getServerSnapshot: undefined | null | (() => Snapshot),
  selector: (snapshot: Snapshot) => Selection,
  isEqual?: (a: Selection, b: Selection) => boolean,
): Selection {
  const instance = useRef<{ hasValue: boolean; value: Selection | null }>(null);
  if (instance.current === null) instance.current = { hasValue: false, value: null };
  // oxlint-disable-next-line react/refs -- port of React's own selector shim, which keeps the last selection on an instance ref
  const inst = instance.current;
  const [getSelection, getServerSelection] = useMemo(() => {
    let hasMemo = false;
    let memoizedSnapshot: Snapshot;
    let memoizedSelection: Selection;
    const memoizedSelector = (nextSnapshot: Snapshot): Selection => {
      if (!hasMemo) {
        // oxlint-disable-next-line react/immutability -- the selector's memo lives in this useMemo closure, as in React's shim
        hasMemo = true;
        memoizedSnapshot = nextSnapshot;
        const nextSelection = selector(nextSnapshot);
        if (isEqual !== undefined && inst.hasValue) {
          const current = inst.value as Selection;
          if (isEqual(current, nextSelection)) return (memoizedSelection = current);
        }
        return (memoizedSelection = nextSelection);
      }
      const prevSnapshot = memoizedSnapshot;
      const prevSelection = memoizedSelection;
      if (Object.is(prevSnapshot, nextSnapshot)) return prevSelection;
      const nextSelection = selector(nextSnapshot);
      if (isEqual !== undefined && isEqual(prevSelection, nextSelection)) {
        memoizedSnapshot = nextSnapshot;
        return prevSelection;
      }
      memoizedSnapshot = nextSnapshot;
      // oxlint-disable-next-line react/immutability -- the selector's memo lives in this useMemo closure, as in React's shim
      return (memoizedSelection = nextSelection);
    };
    const maybeServer = getServerSnapshot ?? null;
    return [
      () => memoizedSelector(getSnapshot()),
      maybeServer === null ? undefined : () => memoizedSelector(maybeServer()),
    ] as const;
  }, [getSnapshot, getServerSnapshot, selector, isEqual, inst]);
  const value = useSyncExternalStore(subscribe, getSelection, getServerSelection);
  inst.hasValue = true;
  inst.value = value;
  useDebugValue(value);
  return value;
}
