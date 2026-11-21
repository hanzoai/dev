import { useCallback } from "react";

import { useEmbed } from "./context";
import { useThreadRef } from "./navigation";

/**
 * Dev's thread list, drawn where the host puts it (its own sidebar). The list itself is the
 * core's `Sidebar` content, portalled in by the root route.
 */
export function DevThreadList({
  active,
  onOpen,
  className,
}: {
  /** `<environmentId>/<threadId>` the host has open. */
  active?: string | null;
  /** The person opened a thread (or left for the empty state) in the list. */
  onOpen?: (ref: string | null) => void;
  className?: string;
}) {
  const embed = useEmbed();
  const ref = useCallback((el: HTMLDivElement | null) => embed?.slots.threads.set(el), [embed]);
  useThreadRef(embed, active, onOpen);
  return <div ref={ref} data-dev-threads="" className={className} />;
}
