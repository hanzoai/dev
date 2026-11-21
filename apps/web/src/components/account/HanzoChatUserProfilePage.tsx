import { HANZOCHAT_PLAN_TUNNEL_LIMITS, type RelayHanzoChatLink } from "@hanzodev/contracts/relay";
import { MessageSquareIcon } from "lucide-react";
import { T, useGT, Var } from "gt-react";
import type { ReactNode } from "react";

import { useManagedTunnelUsage } from "../../cloud/managedRelayState";
import { HANZOCHAT_SUBSCRIPTION_URL, TunnelUsageMeter } from "../cloud/TunnelUsage";
import { Button } from "../ui/button";
import { AccountPage, AccountPageRefreshButton, AccountPageRow } from "./AccountPage";
import { useHanzoChatLink } from "./useHanzoChatLink";

function planLabel(link: RelayHanzoChatLink): ReactNode {
  switch (link.plan) {
    case "premier":
      return (
        <T>
          Premier · up to <Var>{HANZOCHAT_PLAN_TUNNEL_LIMITS.premier}</Var> tunnels
        </T>
      );
    case "pro":
      return (
        <T>
          Pro · up to <Var>{HANZOCHAT_PLAN_TUNNEL_LIMITS.pro}</Var> tunnels.{" "}
          <a
            className="underline"
            href={HANZOCHAT_SUBSCRIPTION_URL}
            target="_blank"
            rel="noopener noreferrer"
          >
            Upgrade on Hanzo Chat
          </a>{" "}
          to Premier for <Var>{HANZOCHAT_PLAN_TUNNEL_LIMITS.premier}</Var>.
        </T>
      );
    case "free":
      return (
        <T>
          <a
            className="underline"
            href={HANZOCHAT_SUBSCRIPTION_URL}
            target="_blank"
            rel="noopener noreferrer"
          >
            Upgrade on Hanzo Chat
          </a>{" "}
          for more tunnels
        </T>
      );
  }
}

/** Links a Hanzo Chat account so paid subscribers get more tunnels. */
export function HanzoChatUserProfilePage() {
  const {
    linkState,
    pending,
    linkUrl,
    link: handleLink,
    unlink: handleUnlink,
  } = useHanzoChatLink();
  const usage = useManagedTunnelUsage();
  const gt = useGT();

  const data = linkState.data;
  const link = data?.link ?? null;

  return (
    <AccountPage
      title="Hanzo Chat"
      description={gt(
        "Link a paid Hanzo Chat account for more Hanzo Link tunnels: up to {pro} with Pro, {premier} with Premier.",
        { pro: HANZOCHAT_PLAN_TUNNEL_LIMITS.pro, premier: HANZOCHAT_PLAN_TUNNEL_LIMITS.premier },
      )}
      action={
        <AccountPageRefreshButton
          disabled={pending !== null}
          isPending={linkState.isPending}
          onClick={usage.refresh}
        />
      }
    >
      {linkState.error ? (
        <div className="border-t py-3 text-xs" role="alert">
          <p className="font-medium text-destructive-foreground">
            <T>Could not load Hanzo Chat link</T>
          </p>
          <p className="mt-0.5 text-muted-foreground">{linkState.error}</p>
        </div>
      ) : data === null ? null : !data.available ? (
        <p className="border-t py-4 text-xs text-muted-foreground">
          <T>This Hanzo Link server does not support Hanzo Chat linking.</T>
        </p>
      ) : (
        <ul className="border-t">
          <AccountPageRow icon={<MessageSquareIcon className="size-4" />}>
            <div className="flex items-start gap-4">
              <div className="min-w-0 flex-1">
                <h3 className="truncate text-sm leading-4.5 font-medium text-foreground">
                  {link ? link.email : gt("Not linked")}
                </h3>
                <p className="mt-1 text-xs leading-4.5 text-muted-foreground">
                  {link ? (
                    planLabel(link)
                  ) : linkUrl ? (
                    <T>
                      Confirm on{" "}
                      <a
                        className="underline"
                        href={linkUrl}
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        hanzo.chat
                      </a>
                      , then select Refresh.
                    </T>
                  ) : (
                    gt("You confirm the link on hanzo.chat.")
                  )}
                </p>
              </div>
              {link ? (
                <Button
                  size="sm"
                  variant="destructive-outline"
                  disabled={pending !== null}
                  onClick={() => void handleUnlink()}
                >
                  {pending === "unlink" ? gt("Unlinking…") : gt("Unlink")}
                </Button>
              ) : (
                <Button size="sm" disabled={pending !== null} onClick={() => void handleLink()}>
                  {pending === "link" ? gt("Opening…") : gt("Link Hanzo Chat")}
                </Button>
              )}
            </div>
          </AccountPageRow>
          {usage.used !== null && usage.limit !== null ? (
            <li className="border-t py-4">
              <TunnelUsageMeter used={usage.used} limit={usage.limit} />
            </li>
          ) : null}
        </ul>
      )}
    </AccountPage>
  );
}
