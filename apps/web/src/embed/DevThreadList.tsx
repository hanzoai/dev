import { useCallback, useEffect } from "react";

import { embedTools, useEmbed } from "./context";
import { moveThroughRows } from "./focus";
import { useThreadRef } from "./navigation";

/**
 * Dev's thread list, drawn where the host puts it (its own sidebar). The list itself is the
 * core's `Sidebar` content, portalled in by the root route.
 */
export function DevThreadList({
  active,
  onOpen,
  tools = true,
  className,
}: {
  /** `<environmentId>/<threadId>` the host has open. */
  active?: string | null;
  /** The person opened a thread (or left for the empty state) in the list. */
  onOpen?: (ref: string | null) => void;
  /** Show the code-only icons (source control, issues): /dev yes, /chat no. */
  tools?: boolean;
  className?: string;
}) {
  const embed = useEmbed();
  const ref = useCallback((el: HTMLDivElement | null) => embed?.slots.threads.set(el), [embed]);
  useThreadRef(embed, active, onOpen);
  useEffect(() => embedTools.set(tools), [tools]);
  return (
    <div
      ref={ref}
      data-dev-threads=""
      className={className}
      onKeyDown={(event) => moveThroughRows(event, event.currentTarget)}
    />
  );
}
