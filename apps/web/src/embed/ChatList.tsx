import { PinIcon } from "lucide-react";
import type { ReactNode } from "react";

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
  const { threads } = useCloudThreads(props.cloud);
  const ordered = orderThreads(threads);
  return (
    // oxlint-disable-next-line hanzo/no-untranslated-copy -- the host draws this list in its own sidebar, outside the I18nProvider DevHome and DevProvider mount and possibly before GT starts, where gt-react throws
    <nav aria-label="Chats" className={props.className} data-dev-chat-list="">
      <ul className="flex flex-col gap-0.5">
        {ordered.map((entry) => {
          const ref = refOfThread(entry);
          const open = props.active === ref || props.active === entry.key;
          return (
            <li key={entry.key}>
              <button
                type="button"
                aria-current={open ? "true" : undefined}
                onClick={() => props.onOpen?.(ref)}
                className="flex w-full min-w-0 items-center gap-1.5 rounded-md px-2 py-1 text-left text-sm hover:bg-accent aria-[current=true]:bg-accent"
              >
                {entry.pinned ? <PinIcon className="size-3 shrink-0 opacity-60" /> : null}
                {/* oxlint-disable-next-line hanzo/no-untranslated-copy -- the host draws this list in its own sidebar, outside the I18nProvider DevHome and DevProvider mount and possibly before GT starts, where gt-react throws */}
                <span className="truncate">{entry.title || "New chat"}</span>
              </button>
            </li>
          );
        })}
      </ul>
    </nav>
  );
}
