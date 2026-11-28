import { AlignJustifyIcon, CheckIcon, Link2Icon, PencilIcon, PinIcon } from "lucide-react";
import { useState, type ReactNode } from "react";

import { orderThreads, refOfThread } from "./chatOrder";
import { useCloudThreads } from "./homeChat";
import type { DevCloud } from "./runtime";

export type ChatListDensity = 1 | 2 | 3;
export type ChatListViewMode = "priority" | "chronological";

const DENSITY_STORAGE_KEY = "hanzo.dev.chatlist.density";
const VIEW_STORAGE_KEY = "hanzo.dev.chatlist.view";

function readStoredDensity(): ChatListDensity {
  try {
    const raw = window.localStorage.getItem(DENSITY_STORAGE_KEY);
    if (raw === "1") return 1;
    if (raw === "2") return 2;
    if (raw === "3") return 3;
  } catch {
    // ignore storage failures
  }
  return 1;
}

function readStoredViewMode(): ChatListViewMode {
  try {
    const raw = window.localStorage.getItem(VIEW_STORAGE_KEY);
    if (raw === "chronological" || raw === "priority") return raw;
  } catch {
    // ignore storage failures
  }
  return "priority";
}

function dayBucketFor(timestampMs: number, nowMs: number): string {
  const startOfDay = (ms: number) => {
    const d = new Date(ms);
    d.setHours(0, 0, 0, 0);
    return d.getTime();
  };
  const todayStart = startOfDay(nowMs);
  const diffDays = Math.floor((todayStart - startOfDay(timestampMs)) / 86_400_000);
  /* oxlint-disable hanzo/no-untranslated-copy -- rendered outside host I18nProvider */
  if (diffDays <= 0) return "Today";
  if (diffDays === 1) return "Yesterday";
  if (diffDays <= 7) return "Previous 7 days";
  if (diffDays <= 30) return "Previous 30 days";
  return "Older";
  /* oxlint-enable hanzo/no-untranslated-copy */
}

function formatShortAge(timestampMs: number, nowMs: number): string {
  const mins = Math.max(0, Math.floor((nowMs - timestampMs) / 60_000));
  if (mins < 1) return "now";
  if (mins < 60) return `${mins}m`;
  const hrs = Math.floor(mins / 60);
  if (hrs < 24) return `${hrs}h`;
  const days = Math.floor(hrs / 24);
  if (days < 30) return `${days}d`;
  return new Date(timestampMs).toLocaleDateString(undefined, { month: "short", day: "numeric" });
}

/**
 * The account's cloud conversations as a list for the host's sidebar: the same ones hanzo.app Chat
 * and the phone list, so a chat started anywhere is here and opens in `DevHome`.
 * Supports 1/2/3-line density modes and Priority / Day-by-Day view switching in place.
 */
export function DevChatList(props: {
  cloud: DevCloud;
  /** The conversation the host has open (a remote id or a device key). */
  active?: string | null;
  /** A thread was opened in the list; `null` is a new conversation. */
  onOpen?: (ref: string | null) => void;
  /** Controlled density override (1, 2, or 3 lines). */
  density?: ChatListDensity;
  /** Controlled view mode override ("priority" or "chronological"). */
  viewMode?: ChatListViewMode;
  /** Show the inline `≡` Priority / Day-by-Day & density menu (default true). */
  showViewControls?: boolean;
  className?: string;
}): ReactNode {
  const { controller, threads } = useCloudThreads(props.cloud);
  const [storedDensity, setStoredDensity] = useState<ChatListDensity>(readStoredDensity);
  const [storedViewMode, setStoredViewMode] = useState<ChatListViewMode>(readStoredViewMode);
  const [controlsOpen, setControlsOpen] = useState(false);
  const density = props.density ?? storedDensity;
  const viewMode = props.viewMode ?? storedViewMode;
  const showControls = props.showViewControls ?? true;

  const setDensity = (next: ChatListDensity) => {
    setStoredDensity(next);
    try {
      window.localStorage.setItem(DENSITY_STORAGE_KEY, String(next));
    } catch {
      // ignore storage failures
    }
  };
  const setViewMode = (next: ChatListViewMode) => {
    setStoredViewMode(next);
    try {
      window.localStorage.setItem(VIEW_STORAGE_KEY, next);
    } catch {
      // ignore storage failures
    }
  };

  const ordered =
    viewMode === "chronological"
      ? threads.toSorted((a, b) => b.updatedAt - a.updatedAt)
      : orderThreads(threads);
  const [renamingKey, setRenamingKey] = useState<string | null>(null);
  const [renameDraft, setRenameDraft] = useState("");
  const [sharedKey, setSharedKey] = useState<string | null>(null);
  // oxlint-disable-next-line react/purity -- bucket labels use current wall-clock day
  const nowMs = Date.now();

  const commitRename = (key: string) => {
    const next = renameDraft.trim();
    setRenamingKey(null);
    if (next) void controller.renameThread(key, next);
  };

  const shareThread = (key: string, ref: string) => {
    const origin = typeof window !== "undefined" ? window.location.origin : "https://hanzo.app";
    const url = `${origin}/chat/${encodeURIComponent(ref)}`;
    void navigator.clipboard?.writeText(url).catch(() => undefined);
    setSharedKey(key);
    window.setTimeout(() => {
      setSharedKey((prev) => (prev === key ? null : prev));
    }, 1800);
  };

  const renderEntry = (entry: (typeof ordered)[number]) => {
    const ref = refOfThread(entry);
    const open = props.active === ref || props.active === entry.key;
    const age = formatShortAge(entry.updatedAt, nowMs);
    if (renamingKey === entry.key) {
      return (
        <li key={entry.key} className="px-1 py-0.5">
          <input
            autoFocus
            type="text"
            value={renameDraft}
            onChange={(e) => setRenameDraft(e.target.value)}
            onBlur={() => commitRename(entry.key)}
            onKeyDown={(e) => {
              if (e.key === "Enter") commitRename(entry.key);
              else if (e.key === "Escape") setRenamingKey(null);
            }}
            // oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider
            aria-label="Rename chat"
            className="w-full rounded-md border border-border bg-background px-2 py-1 text-sm text-foreground outline-none"
          />
        </li>
      );
    }
    return (
      <li key={entry.key} data-chat-density={density} className="group flex items-center gap-0.5">
        <button
          type="button"
          aria-current={open ? "true" : undefined}
          onClick={() => props.onOpen?.(ref)}
          className="flex min-w-0 flex-1 flex-col gap-0.5 rounded-md px-2 py-1 text-left text-sm hover:bg-accent aria-[current=true]:bg-accent"
        >
          <span className="flex min-w-0 w-full items-center gap-1.5">
            {entry.pinned ? <PinIcon className="size-3 shrink-0 opacity-60" /> : null}
            {/* oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider */}
            <span className="min-w-0 flex-1 truncate">{entry.title || "New chat"}</span>
            {density === 1 ? (
              <span className="shrink-0 text-xs tabular-nums text-muted-foreground/70">{age}</span>
            ) : null}
          </span>
          {density >= 2 ? (
            <span className="flex min-w-0 w-full items-center gap-1.5 text-xs text-muted-foreground/80">
              {/* oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider */}
              <span className="truncate">{entry.remoteId ? "Cloud" : "Local"}</span>
              <span aria-hidden className="opacity-50">
                ·
              </span>
              <span className="shrink-0 tabular-nums">{age}</span>
            </span>
          ) : null}
          {density === 3 ? (
            <span className="truncate text-2xs text-muted-foreground/65">
              {/* oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider */}
              {entry.pinned ? "Pinned conversation" : new Date(entry.updatedAt).toLocaleString()}
            </span>
          ) : null}
        </button>
        <div className="flex shrink-0 items-center opacity-0 transition-opacity group-hover:opacity-100 focus-within:opacity-100">
          <button
            type="button"
            // oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider
            aria-label="Rename chat"
            onClick={(e) => {
              e.stopPropagation();
              setRenamingKey(entry.key);
              setRenameDraft(entry.title || "");
            }}
            className="inline-flex size-6 cursor-pointer items-center justify-center rounded-md text-muted-foreground hover:bg-accent hover:text-foreground"
          >
            <PencilIcon className="size-3" />
          </button>
          <button
            type="button"
            // oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider
            aria-label={sharedKey === entry.key ? "Link copied" : "Copy chat link"}
            onClick={(e) => {
              e.stopPropagation();
              shareThread(entry.key, ref);
            }}
            className="inline-flex size-6 cursor-pointer items-center justify-center rounded-md text-muted-foreground hover:bg-accent hover:text-foreground"
          >
            {sharedKey === entry.key ? (
              <CheckIcon className="size-3" />
            ) : (
              <Link2Icon className="size-3" />
            )}
          </button>
        </div>
      </li>
    );
  };

  const dayGroups =
    viewMode === "chronological"
      ? (() => {
          const buckets = new Map<string, Array<(typeof ordered)[number]>>();
          for (const entry of ordered) {
            const label = dayBucketFor(entry.updatedAt, nowMs);
            const list = buckets.get(label) ?? [];
            list.push(entry);
            buckets.set(label, list);
          }
          return [...buckets.entries()];
        })()
      : null;

  return (
    <nav
      // oxlint-disable-next-line hanzo/no-untranslated-copy -- the host draws this list in its own sidebar, outside the I18nProvider DevHome and DevProvider mount and possibly before GT starts, where gt-react throws
      aria-label="Chats"
      className={props.className}
      data-dev-chat-list=""
      data-view-mode={viewMode}
      data-density={density}
    >
      {showControls && ordered.length > 0 ? (
        <div className="relative mb-1 flex items-center justify-end px-1">
          <button
            type="button"
            data-testid="chat-list-view-toggle"
            // oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider
            aria-label="List view and density"
            aria-expanded={controlsOpen}
            onClick={() => setControlsOpen((open) => !open)}
            className="inline-flex size-6 cursor-pointer items-center justify-center rounded-md text-muted-foreground hover:bg-accent hover:text-foreground"
          >
            <AlignJustifyIcon className="size-3.5" />
          </button>
          {controlsOpen ? (
            <div
              role="menu"
              data-testid="chat-list-view-menu"
              className="absolute top-7 right-1 z-30 w-44 rounded-lg border border-border bg-popover p-1 text-xs shadow-lg"
            >
              {/* oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider */}
              <div className="px-2 py-1 font-medium text-muted-foreground">View</div>
              {(
                [
                  ["priority", "Priority"],
                  ["chronological", "Day by day"],
                ] as const
              ).map(([mode, label]) => (
                <button
                  key={mode}
                  type="button"
                  role="menuitemradio"
                  aria-checked={viewMode === mode}
                  onClick={() => {
                    setViewMode(mode);
                    setControlsOpen(false);
                  }}
                  className="flex w-full cursor-pointer items-center justify-between rounded-md px-2 py-1 text-left text-foreground hover:bg-accent"
                >
                  <span>{label}</span>
                  {viewMode === mode ? <CheckIcon className="size-3" /> : null}
                </button>
              ))}
              <div className="my-1 border-t border-border/70" />
              {/* oxlint-disable-next-line hanzo/no-untranslated-copy -- outside I18nProvider */}
              <div className="px-2 py-1 font-medium text-muted-foreground">Density</div>
              {(
                [
                  [1, "1 line"],
                  [2, "2 lines"],
                  [3, "3 lines"],
                ] as const
              ).map(([lines, label]) => (
                <button
                  key={lines}
                  type="button"
                  role="menuitemradio"
                  aria-checked={density === lines}
                  onClick={() => {
                    setDensity(lines);
                    setControlsOpen(false);
                  }}
                  className="flex w-full cursor-pointer items-center justify-between rounded-md px-2 py-1 text-left text-foreground hover:bg-accent"
                >
                  <span>{label}</span>
                  {density === lines ? <CheckIcon className="size-3" /> : null}
                </button>
              ))}
            </div>
          ) : null}
        </div>
      ) : null}
      {dayGroups ? (
        <div className="flex flex-col gap-2">
          {dayGroups.map(([label, entries]) => (
            <div key={label} data-chat-day-group={label}>
              <div className="px-2 pb-0.5 text-2xs font-medium text-muted-foreground/75">
                {label}
              </div>
              <ul className="flex flex-col gap-0.5">{entries.map(renderEntry)}</ul>
            </div>
          ))}
        </div>
      ) : (
        <ul className="flex flex-col gap-0.5">{ordered.map(renderEntry)}</ul>
      )}
    </nav>
  );
}
