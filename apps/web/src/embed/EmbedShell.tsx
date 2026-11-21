import { useSyncExternalStore, type ReactNode } from "react";
import { createPortal } from "react-dom";

import { SidebarProvider } from "../components/ui/sidebar";
import ThreadSidebar from "../components/Sidebar";
import { useEmbed, type Slot } from "./context";

const sidebarRouteReveal = { observedKey: null, pendingKey: null };

function useSlot(slot: Slot): HTMLElement | null {
  return useSyncExternalStore(slot.subscribe, slot.get, () => null);
}

/**
 * The core's root layout when a host mounts it: the thread list and the thread are two
 * portals into the host's own tree, so one React tree (one router, one registry, one
 * connection) draws both and the host keeps a single sidebar.
 */
export function EmbedShell({ children }: { children: ReactNode }) {
  const embed = useEmbed();
  const threads = useSlot(embed!.slots.threads);
  const workspace = useSlot(embed!.slots.workspace);
  return (
    <SidebarProvider className="min-h-0! h-full! w-full!" storageKey="dev_embed_sidebar_open">
      {threads ? createPortal(<ThreadSidebar routeReveal={sidebarRouteReveal} />, threads) : null}
      {workspace ? createPortal(children, workspace) : null}
    </SidebarProvider>
  );
}
