import { T, useMessages } from "gt-react";
import { LogInIcon, RotateCwIcon } from "lucide-react";
import { useMemo } from "react";

import { useAuth, useHanzoWebAccounts } from "../../cloud/hanzoWebAuth";
import { hasCloudPublicConfig } from "../../cloud/publicConfig";
import { SidebarMenu, SidebarMenuButton, SidebarMenuItem } from "../ui/sidebar";
import { retryClerkLoad, useClerkLoadFailed } from "./clerkLoadFailure";
import { HANZO_CONNECT_ACCOUNT_PAGES } from "./HanzoConnectAccountPages";
import { useHanzoConnectAuthPrompt } from "./useHanzoConnectAuthPrompt";

export function HanzoConnectSidebarSignIn() {
  if (!hasCloudPublicConfig()) return null;

  return <ConfiguredHanzoConnectSidebarSignIn />;
}

export function HanzoConnectSidebarAvatar() {
  if (!hasCloudPublicConfig()) return null;

  return <ConfiguredHanzoConnectSidebarAvatar />;
}

function ConfiguredHanzoConnectSidebarAvatar() {
  const { isLoaded, isSignedIn } = useAuth();
  const { activeAccount, accounts, openModal } = useHanzoWebAccounts();
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

  if (!isLoaded || !isSignedIn || !activeAccount) return null;

  const initials = (activeAccount.displayName ?? activeAccount.email ?? "H")
    .slice(0, 2)
    .toUpperCase();

  return (
    <button
      type="button"
      onClick={() => openModal("accounts", resolvedPages)}
      title={`${activeAccount.displayName ?? activeAccount.userId}${
        accounts.length > 1 ? ` (${accounts.length} accounts)` : ""
      }`}
      className="relative mb-1 flex size-8 items-center justify-center rounded-lg p-1 hover:bg-sidebar-row-hover"
    >
      {activeAccount.avatarUrl ? (
        <img
          src={activeAccount.avatarUrl}
          alt={activeAccount.displayName ?? "Account"}
          className="size-6 rounded-full object-cover"
        />
      ) : (
        <span className="flex size-6 items-center justify-center rounded-full bg-foreground text-[10px] font-bold text-background">
          {initials}
        </span>
      )}
      {accounts.length > 1 && (
        <span className="absolute -top-0.5 -right-0.5 flex size-3.5 items-center justify-center rounded-full bg-emerald-500 text-[9px] font-bold text-white">
          {accounts.length}
        </span>
      )}
    </button>
  );
}

function ConfiguredHanzoConnectSidebarSignIn() {
  const { isLoaded, isSignedIn } = useAuth();
  const loadFailed = useClerkLoadFailed();
  const { openAuthPrompt } = useHanzoConnectAuthPrompt();

  if (loadFailed) {
    return (
      <SidebarMenu>
        <SidebarMenuItem>
          <SidebarMenuButton onClick={retryClerkLoad}>
            <RotateCwIcon />
            <span>
              <T>Retry Hanzo Connect</T>
            </span>
          </SidebarMenuButton>
        </SidebarMenuItem>
      </SidebarMenu>
    );
  }
  if (!isLoaded || isSignedIn) return null;

  return (
    <SidebarMenu>
      <SidebarMenuItem>
        <SidebarMenuButton onClick={openAuthPrompt}>
          <LogInIcon />
          <span>
            <T>Sign in to Hanzo Connect</T>
          </span>
        </SidebarMenuButton>
      </SidebarMenuItem>
    </SidebarMenu>
  );
}
