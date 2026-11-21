import { T, useGT } from "gt-react";

import { useAuth, useHanzoWebAccounts } from "../../cloud/hanzoWebAuth";
import { hasCloudPublicConfig } from "../../cloud/publicConfig";
import { useHanzoConnectAuthPrompt } from "../account/useHanzoConnectAuthPrompt";
import { SidebarMenuButton, SidebarMenuItem } from "../ui/sidebar";
import { drawerAccountCaption, drawerAccountInitial } from "../ui/mobileDrawer.logic";

/** The drawer's bottom-left account: avatar, name, plan or role. Signed out it is the sign-in. */
export function SidebarDrawerAccount() {
  if (!hasCloudPublicConfig()) return null;
  return <ConfiguredSidebarDrawerAccount />;
}

function ConfiguredSidebarDrawerAccount() {
  const gt = useGT();
  const { isLoaded, isSignedIn } = useAuth();
  const { activeAccount, billing, openModal } = useHanzoWebAccounts();
  const { openAuthPrompt } = useHanzoConnectAuthPrompt();

  if (!isLoaded) return null;
  if (!isSignedIn || !activeAccount) {
    return (
      <SidebarMenuItem className="min-w-0 flex-1">
        <SidebarMenuButton onClick={openAuthPrompt}>
          <span>
            <T>Sign in to Hanzo</T>
          </span>
        </SidebarMenuButton>
      </SidebarMenuItem>
    );
  }

  const name = activeAccount.displayName ?? activeAccount.email ?? activeAccount.id;
  const caption = drawerAccountCaption({
    owner: activeAccount.owner,
    planName: billing?.planName ?? null,
  });
  return (
    <SidebarMenuItem className="min-w-0 flex-1">
      <SidebarMenuButton
        aria-label={gt("Account: {name}", { name })}
        size="lg"
        onClick={() => openModal("accounts")}
      >
        {activeAccount.avatarUrl ? (
          <img alt="" className="size-8 rounded-full object-cover" src={activeAccount.avatarUrl} />
        ) : (
          <span className="flex size-8 shrink-0 items-center justify-center rounded-full bg-foreground text-sm font-semibold text-background">
            {drawerAccountInitial(name)}
          </span>
        )}
        <span className="flex min-w-0 flex-col text-left leading-tight">
          <span className="truncate text-sm font-medium">{name}</span>
          {caption.kind === "superAdmin" ? (
            <span className="truncate text-xs text-muted-foreground">
              <T>SuperAdmin</T>
            </span>
          ) : caption.plan ? (
            <span className="truncate text-xs text-muted-foreground">{caption.plan}</span>
          ) : null}
        </span>
      </SidebarMenuButton>
    </SidebarMenuItem>
  );
}
