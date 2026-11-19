import { findErrorTraceId, thrownErrorMessage } from "@hanzodev/client-runtime/errors";
import {
  isAtomCommandInterrupted,
  squashAtomCommandFailure,
} from "@hanzodev/client-runtime/state/runtime";
import { msg } from "gt-react";
import { useState } from "react";

import {
  createManagedRelayHanzoChatLinkRequestCommand,
  unlinkManagedRelayHanzoChatCommand,
  useManagedRelayHanzoChatLink,
} from "../../cloud/managedRelayState";
import { m } from "../../i18n/messages";
import { readLocalApi } from "../../localApi";
import { useAtomCommand } from "../../state/use-atom-command";
import { toastManager } from "../ui/toast";

function reportFailure(title: string, cause: unknown) {
  const traceId = findErrorTraceId(cause);
  toastManager.add({
    type: "error",
    title,
    description: thrownErrorMessage(cause, m) ?? m(msg("Try again.")),
    data: traceId
      ? {
          secondaryActionProps: {
            children: m(msg("Copy trace ID")),
            onClick: () => void navigator.clipboard?.writeText(traceId),
          },
        }
      : undefined,
  });
}

/** Starts and removes the Hanzo Chat link that raises the Hanzo Connect tunnel cap. */
export function useHanzoChatLink() {
  const linkState = useManagedRelayHanzoChatLink();
  const createLinkRequest = useAtomCommand(createManagedRelayHanzoChatLinkRequestCommand, {
    reportFailure: false,
  });
  const unlinkCommand = useAtomCommand(unlinkManagedRelayHanzoChatCommand, { reportFailure: false });
  const [pending, setPending] = useState<"link" | "unlink" | null>(null);
  // Kept so a blocked popup still leaves the user a link to open.
  const [linkUrl, setLinkUrl] = useState<string | null>(null);

  const link = async () => {
    const accountId = linkState.accountId;
    if (!accountId || pending) return;
    setPending("link");
    const result = await createLinkRequest({ accountId });
    if (result._tag === "Success") {
      setLinkUrl(result.value);
      await readLocalApi()
        ?.shell.openExternal(result.value)
        .catch(() => undefined);
    } else if (!isAtomCommandInterrupted(result)) {
      reportFailure(m(msg("Could not start linking")), squashAtomCommandFailure(result));
    }
    setPending(null);
  };

  const unlink = async () => {
    const accountId = linkState.accountId;
    if (!accountId || pending) return;
    setPending("unlink");
    const result = await unlinkCommand({ accountId });
    if (result._tag === "Success") {
      setLinkUrl(null);
      linkState.refresh();
    } else if (!isAtomCommandInterrupted(result)) {
      reportFailure(m(msg("Could not unlink Hanzo Chat")), squashAtomCommandFailure(result));
    }
    setPending(null);
  };

  return { linkState, pending, linkUrl, link, unlink };
}
