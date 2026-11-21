// Where a host's focus keys land inside the core, found by the attributes the core already draws.

const ROW = "[data-thread-item] [data-sidebar-row]";

/** The thread rows in the list, top to bottom. */
const rows = (list: ParentNode): HTMLElement[] => [...list.querySelectorAll<HTMLElement>(ROW)];

/** Focuses the composer's editor. Returns whether there was one. */
export function focusComposer(): boolean {
  const editor = document.querySelector<HTMLElement>(
    '[data-dev-workspace] [data-testid="composer-editor"] [contenteditable="true"], [data-dev-workspace] [data-testid="composer-editor"][contenteditable="true"]',
  );
  editor?.focus();
  return editor !== null;
}

/** Focuses the open thread's row, else the first row. Returns whether there was a list. */
export function focusThreadList(): boolean {
  const list = document.querySelector<HTMLElement>("[data-dev-threads]");
  if (!list) return false;
  const all = rows(list);
  const open = all.find((row) => row.getAttribute("aria-current") === "page");
  const target = open ?? all[0] ?? list.querySelector<HTMLElement>("input, button, [tabindex]");
  target?.focus();
  return target != null;
}

/** ↑ and ↓ move between rows; Home and End go to the ends. Enter and Space open, as the row does. */
export function moveThroughRows(
  event: { key: string; target: EventTarget | null; preventDefault(): void },
  list: ParentNode,
): void {
  const current =
    event.target instanceof HTMLElement ? event.target.closest<HTMLElement>(ROW) : null;
  if (!current) return;
  const all = rows(list);
  const at = all.indexOf(current);
  const next =
    event.key === "ArrowDown"
      ? all[Math.min(all.length - 1, at + 1)]
      : event.key === "ArrowUp"
        ? all[Math.max(0, at - 1)]
        : event.key === "Home"
          ? all[0]
          : event.key === "End"
            ? all[all.length - 1]
            : undefined;
  if (!next) return;
  event.preventDefault();
  next.focus();
}
