import { useRouter } from "@tanstack/react-router";
import { T, useGT } from "gt-react";
import { PlusIcon } from "lucide-react";

import { useCloudChat, useCloudThreads } from "../../cloud/cloudChat";
import { useAuth } from "../../cloud/hanzoWebAuth";
import { hasCloudPublicConfig } from "../../cloud/publicConfig";
import { isHostedStaticApp } from "../../hostedPairing";
import { useEnvironments } from "../../state/environments";
import { threadRowLabel } from "../chatLanding.logic";

/**
 * Saved plain-chat threads for the hosted app while it has no environment. Reopening one
 * returns to the landing with its messages loaded.
 */
export function CloudThreadsSection() {
  const { isSignedIn } = useAuth({ treatPendingAsSignedOut: false });
  const { environments } = useEnvironments();
  if (!isHostedStaticApp() || !hasCloudPublicConfig() || !isSignedIn || environments.length > 0) {
    return null;
  }
  return <CloudThreadList />;
}

function CloudThreadList() {
  const gt = useGT();
  const router = useRouter();
  const chat = useCloudChat();
  const threads = useCloudThreads();
  const go = (action: () => void | Promise<void>) => {
    void Promise.resolve(action()).then(() => router.navigate({ to: "/" }));
  };
  return (
    <div className="flex flex-col gap-0.5 px-2 pb-2" data-testid="cloud-threads">
      <button
        className="flex h-9 w-full items-center gap-2.5 rounded-md px-2.5 text-left text-sm hover:bg-sidebar-row-hover"
        onClick={() => go(chat.reset)}
        type="button"
      >
        <PlusIcon aria-hidden className="size-4" />
        <T>New chat</T>
      </button>
      {threads.map((thread) => (
        <button
          aria-current={thread.id === chat.state.threadId}
          className="flex h-9 w-full items-center truncate rounded-md px-2.5 text-left text-sm hover:bg-sidebar-row-hover aria-[current=true]:bg-sidebar-row-hover"
          key={thread.id}
          onClick={() => go(() => chat.open(thread.id))}
          type="button"
        >
          <span className="truncate">{threadRowLabel(thread, gt("Chat"))}</span>
        </button>
      ))}
    </div>
  );
}
