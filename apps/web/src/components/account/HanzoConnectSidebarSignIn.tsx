import { T, useGT } from "gt-react";
import { LogInIcon } from "lucide-react";

import { useAuth, useHanzoWebAccounts } from "../../cloud/hanzoWebAuth";
import { hasCloudPublicConfig } from "../../cloud/publicConfig";
import { SidebarMenu, SidebarMenuButton, SidebarMenuItem } from "../ui/sidebar";
import { useResolvedHanzoConnectAccountPages } from "./HanzoConnectAccountPages";
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
  const gt = useGT();
  const { isLoaded, isSignedIn } = useAuth();
  const { activeAccount, accounts, openModal } = useHanzoWebAccounts();
  const resolvedPages = useResolvedHanzoConnectAccountPages();

  if (!isLoaded || !isSignedIn || !activeAccount) return null;

  const initials = (activeAccount.displayName ?? activeAccount.email ?? "H")
    .slice(0, 2)
    .toUpperCase();
  const label = activeAccount.displayName ?? activeAccount.id;

  return (
    <button
      type="button"
      onClick={() => openModal("accounts", resolvedPages)}
      aria-label={
        accounts.length > 1
          ? gt("{account} ({count} accounts)", { account: label, count: accounts.length })
          : label
      }
      className="relative mb-1 flex size-8 items-center justify-center rounded-lg p-1 hover:bg-sidebar-row-hover"
    >
      {activeAccount.avatarUrl ? (
        <img
          src={activeAccount.avatarUrl}
          alt={label}
          className="size-6 rounded-full object-cover"
        />
      ) : (
        <span className="flex size-6 items-center justify-center rounded-full bg-foreground text-3xs font-bold text-background">
          {initials}
        </span>
      )}
      {accounts.length > 1 && (
        <span className="absolute top-0 right-0 flex size-3.5 items-center justify-center rounded-full bg-success text-4xs font-bold text-white">
          {accounts.length}
        </span>
      )}
    </button>
  );
}

function ConfiguredHanzoConnectSidebarSignIn() {
  const { isLoaded, isSignedIn } = useAuth();
  const { openAuthPrompt } = useHanzoConnectAuthPrompt();

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
