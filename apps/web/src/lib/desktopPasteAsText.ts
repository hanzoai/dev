import type { DesktopBridge } from "@hanzodev/contracts";

export const DESKTOP_PASTE_AS_TEXT_EVENT = "hanzo:paste-as-text";

/**
 * Arm composer paste handling before Electron delivers the native clipboard event. The event
 * bubbles from the focused element, so a composer can listen on its own form or on `target`.
 */
export function installDesktopPasteAsText(
  bridge: Pick<DesktopBridge, "onMenuAction" | "pasteAsText"> | undefined,
  target: EventTarget,
): (() => void) | undefined {
  return bridge?.onMenuAction((action) => {
    if (action !== "paste-as-text") return;
    (document.activeElement ?? target).dispatchEvent(
      new Event(DESKTOP_PASTE_AS_TEXT_EVENT, { bubbles: true }),
    );
    void bridge.pasteAsText();
  });
}
