import { CheckIcon, Link2Icon, PencilIcon, PinIcon } from "lucide-react";
import { useState, type ReactNode } from "react";

import { orderThreads, refOfThread } from "./chatOrder";
import { useCloudThreads } from "./homeChat";
import type { DevCloud } from "./runtime";

/**
 * The account's cloud conversations as a list for the host's sidebar: the same ones hanzo.app Chat
 * and the phone list, so a chat started anywhere is here and opens in `DevHome`.
 */
export function DevChatList(props: {
  cloud: DevCloud;
  /** The conversation the host has open (a remote id or a device key). */
  active?: string | null;
  /** A thread was opened in the list; `null` is a new conversation. */
  onOpen?: (ref: string | null) => void;
  className?: string;
}): ReactNode {
  const { controller, threads } = useCloudThreads(props.cloud);
  const ordered = orderThreads(threads);
  const [renamingKey, setRenamingKey] = useState<string | null>(null);
  const [renameDraft, setRenameDraft] = useState("");
  const [sharedKey, setSharedKey] = useState<string | null>(null);

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

  return (
    // oxlint-disable-next-line hanzo/no-untranslated-copy -- the host draws this list in its own sidebar, outside the I18nProvider DevHome and DevProvider mount and possibly before GT starts, where gt-react throws
    <nav aria-label="Chats" className={props.className} data-dev-chat-list="">
      <ul className="flex flex-col gap-0.5">
        {ordered.map((entry) => {
          const ref = refOfThread(entry);
          const open = props.active === ref || props.active === entry.key;
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
            <li key={entry.key} className="group flex items-center gap-0.5">
              <button
                type="button"
                aria-current={open ? "true" : undefined}
                onClick={() => props.onOpen?.(ref)}
                className="flex min-w-0 flex-1 items-center gap-1.5 rounded-md px-2 py-1 text-left text-sm hover:bg-accent aria-[current=true]:bg-accent"
              >
                {entry.pinned ? <PinIcon className="size-3 shrink-0 opacity-60" /> : null}
                {/* oxlint-disable-next-line hanzo/no-untranslated-copy -- the host draws this list in its own sidebar, outside the I18nProvider DevHome and DevProvider mount and possibly before GT starts, where gt-react throws */}
                <span className="truncate">{entry.title || "New chat"}</span>
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
        })}
      </ul>
    </nav>
  );
}
