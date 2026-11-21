import {
  buildHanzoSurfaceLaunchUrl,
  DEFAULT_HANZO_LOGIN_URL,
  type HanzoIamAccount,
} from "@hanzodev/shared/hanzoIamAuth";
import { isSharedSuperAdmin } from "@hanzodev/shared/hanzoSharedIdentity";
import { useAtomValue } from "@effect/atom-react";
import {
  ChartNoAxesColumnIcon,
  CreditCardIcon,
  LogOutIcon,
  PlusIcon,
  SmartphoneIcon,
  SettingsIcon,
} from "lucide-react";
import { T, useGT } from "gt-react";
import { memo, useState } from "react";

import {
  getHanzoWebAuthController,
  openExternalUrl,
  useHanzoWebAccounts,
} from "../../cloud/hanzoWebAuth";
import { shortcutLabelForCommand } from "../../keybindings";
import { primaryServerKeybindingsAtom } from "../../state/server";
import { Badge } from "../ui/badge";
import {
  Menu,
  MenuGroup,
  MenuGroupLabel,
  MenuItem,
  MenuPopup,
  MenuRadioGroup,
  MenuRadioItem,
  MenuSeparator,
  MenuShortcut,
  MenuSub,
  MenuSubPopup,
  MenuSubTrigger,
  MenuTrigger,
} from "../ui/menu";
import { PhoneAppDialog } from "../account/PhoneAppDialog";
import { SidebarMenuButton, SidebarMenuItem } from "../ui/sidebar";

const initials = (name: string): string =>
  name
    .split(/[\s@._-]+/u)
    .filter(Boolean)
    .slice(0, 2)
    .map((word) => word.charAt(0).toUpperCase())
    .join("") || "?";

export const accountName = (account: HanzoIamAccount): string =>
  account.subject || account.displayName || account.email || account.id;

/** SuperAdmin shows "Admin"; everyone else shows their plan, when billing has answered. */
export const accountBadge = (
  account: HanzoIamAccount,
  planName: string | null,
  adminLabel: string,
): string | null => (isSharedSuperAdmin(account) ? adminLabel : planName);

function Face({ account, size }: { readonly account: HanzoIamAccount; readonly size: number }) {
  const [failed, setFailed] = useState(false);
  const src = failed ? null : account.avatarUrl;
  return (
    <span
      aria-hidden="true"
      className="inline-flex shrink-0 items-center justify-center overflow-hidden rounded-full bg-muted text-[10px] font-semibold text-foreground"
      style={{ width: size, height: size }}
    >
      {src ? (
        <img alt="" height={size} onError={() => setFailed(true)} src={src} width={size} />
      ) : (
        initials(accountName(account))
      )}
    </span>
  );
}

export const SidebarAccount = memo(function SidebarAccount({
  onNavigate,
}: {
  readonly onNavigate: (to: "/usage" | "/settings") => void;
}) {
  const gt = useGT();
  const [phoneOpen, setPhoneOpen] = useState(false);
  const keybindings = useAtomValue(primaryServerKeybindingsAtom);
  const session = useHanzoWebAccounts();
  const active = session.activeAccount;
  if (!active) return null;

  const controller = getHanzoWebAuthController();
  const name = accountName(active);
  const badge = accountBadge(active, session.billing?.planName ?? null, gt("Admin"));
  const settingsShortcut = shortcutLabelForCommand(keybindings, "settings.open");
  const billingUrl = buildHanzoSurfaceLaunchUrl(`${DEFAULT_HANZO_LOGIN_URL}/billing`, {
    orgId: session.activeOrgId,
  });

  return (
    <SidebarMenuItem className="min-w-0 flex-1">
      <Menu>
        <MenuTrigger
          render={
            <SidebarMenuButton
              aria-label={gt("Account: {name}", { name })}
              className="gap-2"
              data-testid="sidebar-account"
            />
          }
        >
          <Face account={active} size={20} />
          <span className="min-w-0 flex-1 truncate text-start">{name}</span>
        </MenuTrigger>
        <MenuPopup align="start" className="min-w-64" side="top">
          <MenuGroup>
            <MenuGroupLabel>
              <span className="flex items-center gap-2">
                <Face account={active} size={28} />
                <span className="flex min-w-0 flex-col">
                  <span className="flex items-center gap-1.5 truncate text-foreground">
                    {name}
                    {badge ? (
                      <Badge size="sm" variant="outline">
                        {badge}
                      </Badge>
                    ) : null}
                  </span>
                  {active.email ? (
                    <span className="truncate font-normal text-muted-foreground">
                      {active.email}
                    </span>
                  ) : null}
                </span>
              </span>
            </MenuGroupLabel>
          </MenuGroup>
          <MenuSeparator />
          <MenuSub>
            <MenuSubTrigger>
              <T>Account and organization</T>
            </MenuSubTrigger>
            <MenuSubPopup>
              <MenuGroup>
                <MenuGroupLabel>
                  <T>Accounts</T>
                </MenuGroupLabel>
                <MenuRadioGroup
                  value={active.id}
                  onValueChange={(id) => controller.switchAccount(String(id))}
                >
                  {session.accounts.map((account) => (
                    <MenuRadioItem key={account.id} value={account.id}>
                      {account.id}
                    </MenuRadioItem>
                  ))}
                </MenuRadioGroup>
                <MenuItem onClick={() => void controller.startOAuthSignIn()}>
                  <PlusIcon />
                  <T>Add account</T>
                </MenuItem>
              </MenuGroup>
              {session.organizations.length > 0 ? (
                <>
                  <MenuSeparator />
                  <MenuGroup>
                    <MenuGroupLabel>
                      <T>Organization</T>
                    </MenuGroupLabel>
                    <MenuRadioGroup
                      value={session.activeOrgId ?? ""}
                      onValueChange={(org) => controller.switchOrganization(String(org))}
                    >
                      {session.organizations.map((org) => (
                        <MenuRadioItem key={org.id} value={org.id}>
                          {org.displayName}
                        </MenuRadioItem>
                      ))}
                    </MenuRadioGroup>
                  </MenuGroup>
                </>
              ) : null}
            </MenuSubPopup>
          </MenuSub>
          <MenuItem onClick={() => onNavigate("/usage")}>
            <ChartNoAxesColumnIcon />
            <T>Usage</T>
          </MenuItem>
          <MenuItem onClick={() => onNavigate("/settings")}>
            <SettingsIcon />
            <T>Settings</T>
            {settingsShortcut ? <MenuShortcut>{settingsShortcut}</MenuShortcut> : null}
          </MenuItem>
          <MenuItem onClick={() => openExternalUrl(billingUrl)}>
            <CreditCardIcon />
            <T>Plans and billing</T>
          </MenuItem>
          <MenuItem onClick={() => setPhoneOpen(true)}>
            <SmartphoneIcon />
            <T>Get Hanzo Dev on your phone</T>
          </MenuItem>
          <MenuSeparator />
          <MenuItem onClick={() => void controller.signOut()} variant="destructive">
            <LogOutIcon />
            <T>Sign out</T>
          </MenuItem>
        </MenuPopup>
      </Menu>
      <PhoneAppDialog open={phoneOpen} onOpenChange={setPhoneOpen} />
    </SidebarMenuItem>
  );
});
