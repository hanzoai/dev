import { useCallback, type ReactNode } from "react";

import { useEmbed } from "./context";
import { useHasWorkspace } from "./capabilities";
import { useThreadRef } from "./navigation";
import type { DevRuntime } from "./runtime";

/**
 * Dev's thread view: the thread, its composer and model picker, and the dock the host
 * supplies. The dock is the host's because the Terminal, Browser and Computer panes are its
 * sandboxes (@hanzo/tabs), not the core's.
 */
export function DevWorkspace({
  thread,
  onNavigate,
  dock,
  className,
}: {
  /** `<environmentId>/<threadId>` to show; null is Dev's empty state. */
  thread?: string | null;
  /** The person moved to another thread (or to none) inside the core. */
  onNavigate?: (ref: string | null) => void;
  /** The dock beside the thread. */
  dock?: (context: { runtime: DevRuntime; thread: string | null }) => ReactNode;
  className?: string;
}) {
  const embed = useEmbed();
  const ref = useCallback((el: HTMLDivElement | null) => embed?.slots.workspace.set(el), [embed]);
  const open = useThreadRef(embed, thread, onNavigate);
  // The code tools hang on a workspace: a conversation has no dock until a project is attached.
  const workspace = useHasWorkspace(open);
  return (
    <div
      className={className}
      data-dev-workspace=""
      style={{ display: "flex", minWidth: 0, minHeight: 0, flex: 1 }}
    >
      <div ref={ref} style={{ display: "flex", minWidth: 0, minHeight: 0, flex: 1 }} />
      {embed && dock && workspace ? dock({ runtime: embed.runtime, thread: open }) : null}
    </div>
  );
}
