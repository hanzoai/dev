import { MessageSquareIcon, ServerIcon, SmartphoneIcon } from "lucide-react";
import { msg, useMessages } from "gt-react";
import { type ReactNode, useMemo } from "react";

import { useAuth, useHanzoWebAccounts } from "../../cloud/hanzoWebAuth";
import { MobileClientsUserProfilePage } from "./MobileClientsUserProfilePage";
import { HanzoChatUserProfilePage } from "./HanzoChatUserProfilePage";
import { HanzoConnectUserProfilePage } from "./HanzoConnectUserProfilePage";

export const HANZO_CONNECT_ACCOUNT_PAGES = [
  {
    label: msg("Mobile clients"),
    url: "mobile-clients",
    icon: <SmartphoneIcon className="size-4" />,
    content: <MobileClientsUserProfilePage />,
  },
  {
    label: "Hanzo Connect",
    url: "hanzo-connect",
    icon: <ServerIcon className="size-4" />,
    content: <HanzoConnectUserProfilePage />,
  },
  {
    label: "Hanzo Chat",
    url: "hanzo-chat",
    icon: <MessageSquareIcon className="size-4" />,
    content: <HanzoChatUserProfilePage />,
  },
] as const;

export type HanzoConnectAccountPageUrl = (typeof HANZO_CONNECT_ACCOUNT_PAGES)[number]["url"];

export function useHanzoConnectAccountPage(): {
  readonly open: ((page?: HanzoConnectAccountPageUrl) => void) | null;
  readonly portals: ReactNode;
} {
  const { isSignedIn } = useAuth();
  const { openModal } = useHanzoWebAccounts();
  const m = useMessages();

  const resolvedPages = useMemo(
    () =>
      HANZO_CONNECT_ACCOUNT_PAGES.map((page) => ({
        url: page.url,
        label: m(page.label),
        icon: page.icon,
        content: page.content,
      })),
    [m],
  );

  const open = (page: HanzoConnectAccountPageUrl = "hanzo-connect") => {
    openModal(page, resolvedPages);
  };

  return { open: isSignedIn ? open : null, portals: null };
}
