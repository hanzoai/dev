import { useEffect, useEffectEvent, useRef, type ComponentProps } from "react";

import { cn } from "~/lib/utils";
import {
  DRAWER_DURATION_MS,
  DRAWER_EASING,
  drawerAxis,
  drawerDim,
  drawerGrabs,
  drawerPull,
  drawerSettles,
} from "./mobileDrawer.logic";

interface Gesture {
  readonly identifier: number;
  readonly startX: number;
  readonly startY: number;
  readonly from: number;
  claimed: boolean;
  lastX: number;
  lastTime: number;
  speed: number;
}

/** How far open the panel stands right now, mid-animation included. */
function readOpenAmount(panel: HTMLElement): number {
  const { m41 } = new DOMMatrixReadOnly(getComputedStyle(panel).transform);
  return Math.min(panel.offsetWidth, Math.max(0, panel.offsetWidth + m41));
}

/**
 * A left drawer that follows the finger. Position and dim are written straight to the
 * elements as transform and opacity: no React render per frame, one CSS transition per
 * settle, and a new touch takes over wherever the last one stood.
 */
export function MobileDrawer({
  open,
  onOpenChange,
  onSettled,
  animated,
  className,
  children,
  ...props
}: Omit<ComponentProps<"div">, "onChange"> & {
  readonly open: boolean;
  readonly onOpenChange: (open: boolean) => void;
  /** Called once a closed drawer has finished moving. */
  readonly onSettled: (open: boolean) => void;
  /** False snaps instead of sliding. */
  readonly animated: boolean;
}) {
  const panelRef = useRef<HTMLDivElement>(null);
  const scrimRef = useRef<HTMLDivElement>(null);
  const openRef = useRef(false);
  const dragging = useRef(false);
  const returnFocusRef = useRef<HTMLElement | null>(null);

  const paint = (amount: number, transition: string) => {
    const panel = panelRef.current;
    const scrim = scrimRef.current;
    if (!panel || !scrim) return;
    panel.style.transition = transition === "none" ? "none" : `transform ${transition}`;
    scrim.style.transition = transition === "none" ? "none" : `opacity ${transition}`;
    panel.style.transform = `translate3d(${amount - panel.offsetWidth}px,0,0)`;
    scrim.style.opacity = String(drawerDim(amount, panel.offsetWidth));
  };

  const settle = (nextOpen: boolean) => {
    const panel = panelRef.current;
    if (!panel) return;
    const reduced = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    const duration = animated && !reduced ? DRAWER_DURATION_MS : 0;
    openRef.current = nextOpen;
    const target = nextOpen ? panel.offsetWidth : 0;
    if (duration === 0 || readOpenAmount(panel) === target) {
      paint(target, "none");
      onSettled(nextOpen);
      return;
    }
    paint(target, `${duration}ms ${DRAWER_EASING}`);
  };

  const follow = useEffectEvent((nextOpen: boolean) => {
    if (openRef.current === nextOpen && panelRef.current?.style.transform) return;
    if (nextOpen) returnFocusRef.current = document.activeElement as HTMLElement | null;
    settle(nextOpen);
    if (nextOpen) panelRef.current?.focus({ preventScroll: true });
    else if (panelRef.current?.contains(document.activeElement)) {
      returnFocusRef.current?.focus({ preventScroll: true });
    }
  });

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- the button, a shortcut or Escape drives the drawer through `open`; the DOM follows it here
  useEffect(() => {
    follow(open);
  }, [open]);

  const handleOpenChange = useEffectEvent(onOpenChange);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- window keydown listener: Escape closes the drawer wherever focus is
  useEffect(() => {
    if (!open) return;
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key !== "Escape" || event.defaultPrevented) return;
      event.stopPropagation();
      handleOpenChange(false);
    };
    window.addEventListener("keydown", onKeyDown);
    return () => window.removeEventListener("keydown", onKeyDown);
  }, [open]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- window touch listeners for the edge swipe; React cannot register a non-passive touchmove
  useEffect(() => {
    let gesture: Gesture | null = null;
    const find = (event: TouchEvent) =>
      gesture
        ? Array.from(event.changedTouches).find((touch) => touch.identifier === gesture!.identifier)
        : undefined;

    const onStart = (event: TouchEvent) => {
      const panel = panelRef.current;
      const touch = event.touches[0];
      if (!panel || !touch || event.touches.length !== 1) return;
      if (!drawerGrabs(touch.clientX, openRef.current || readOpenAmount(panel) > 0)) return;
      gesture = {
        identifier: touch.identifier,
        startX: touch.clientX,
        startY: touch.clientY,
        from: readOpenAmount(panel),
        claimed: false,
        lastX: touch.clientX,
        lastTime: event.timeStamp,
        speed: 0,
      };
    };
    const onMove = (event: TouchEvent) => {
      const touch = find(event);
      const panel = panelRef.current;
      if (!gesture || !touch || !panel) return;
      if (!gesture.claimed) {
        const axis = drawerAxis(touch.clientX - gesture.startX, touch.clientY - gesture.startY);
        if (axis === null) return;
        if (axis === "y") {
          gesture = null;
          return;
        }
        gesture.claimed = true;
        dragging.current = true;
      }
      event.preventDefault();
      const elapsed = event.timeStamp - gesture.lastTime;
      if (elapsed > 0) {
        gesture.speed = 0.6 * ((touch.clientX - gesture.lastX) / elapsed) + 0.4 * gesture.speed;
      }
      gesture.lastX = touch.clientX;
      gesture.lastTime = event.timeStamp;
      paint(drawerPull(gesture.from, touch.clientX - gesture.startX, panel.offsetWidth), "none");
    };
    const onEnd = (event: TouchEvent) => {
      const panel = panelRef.current;
      if (!gesture || !find(event) || !panel) return;
      const { claimed, speed, from, startX } = gesture;
      const touch = find(event)!;
      gesture = null;
      dragging.current = false;
      if (!claimed) return;
      const at = drawerPull(from, touch.clientX - startX, panel.offsetWidth);
      const next =
        event.type === "touchcancel"
          ? openRef.current
          : drawerSettles(at, panel.offsetWidth, speed);
      settle(next);
      handleOpenChange(next);
    };

    window.addEventListener("touchstart", onStart, { passive: true });
    window.addEventListener("touchmove", onMove, { passive: false });
    window.addEventListener("touchend", onEnd, { passive: true });
    window.addEventListener("touchcancel", onEnd, { passive: true });
    return () => {
      window.removeEventListener("touchstart", onStart);
      window.removeEventListener("touchmove", onMove);
      window.removeEventListener("touchend", onEnd);
      window.removeEventListener("touchcancel", onEnd);
    };
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- settle reads refs and the latest `animated` through the element, so listeners stay registered once
  }, []);

  return (
    <>
      <div
        ref={scrimRef}
        aria-hidden="true"
        data-slot="drawer-scrim"
        className="fixed inset-0 z-[55] bg-black opacity-0 data-[open=false]:pointer-events-none"
        data-open={open}
        onClick={() => onOpenChange(false)}
      />
      <div
        ref={panelRef}
        tabIndex={-1}
        inert={!open}
        data-mobile="true"
        data-sidebar="sidebar"
        data-slot="sidebar"
        data-state={open ? "open" : "closed"}
        className={cn(
          "fixed inset-y-0 left-0 z-[56] flex w-[min(calc(100vw-64px),360px)] flex-col overflow-hidden bg-sidebar surface-grain text-sidebar-foreground outline-none will-change-transform [-webkit-app-region:no-drag]",
          className,
        )}
        style={{ transform: "translate3d(-100%,0,0)" }}
        onTransitionEnd={(event) => {
          if (event.target !== event.currentTarget || event.propertyName !== "transform") return;
          if (!dragging.current) onSettled(openRef.current);
        }}
        {...props}
      >
        {children}
      </div>
    </>
  );
}
