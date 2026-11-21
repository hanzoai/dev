import { errorDisplayMessage, findErrorTraceId } from "@hanzodev/client-runtime/errors";
import {
  isAtomCommandInterrupted,
  squashAtomCommandFailure,
} from "@hanzodev/client-runtime/state/runtime";
import type { EnvironmentId } from "@hanzodev/contracts";
import type { RelayClientEnvironmentRecord } from "@hanzodev/contracts/relay";
import {
  buildHanzoSurfaceLaunchUrl,
  HANZO_BILLING_PORTAL_URL,
  HANZO_ECOSYSTEM_SURFACES,
  HANZO_SUBSCRIPTION_PLANS,
} from "@hanzodev/shared/hanzoIamAuth";
import type { MenuAction } from "@react-native-menu/menu";
import { msg, T, useGT, useMessages } from "gt-react-native";
import { type ReactNode, useRef, useState } from "react";
import { Linking, Pressable, ScrollView, Text, View } from "react-native";

import { AppActivityIndicator } from "../../components/ui/AppActivityIndicator";
import { SymbolView } from "../../components/ui/AppSymbol";
import * as Dialog from "../../components/ui/Dialog";
import { Menu } from "../../components/ui/Menu";
import { RefreshControl } from "../../components/ui/RefreshControl";
import { m, type MessageResolver } from "../../i18n/messages";
import { copyTextWithHaptic } from "../../lib/copyTextWithHaptic";
import { relayEnvironmentDiscovery } from "../../state/relay";
import { useAtomCommand } from "../../state/use-atom-command";
import { useHanzoMobileAccounts } from "./hanzoMobileAuth";
import {
  deregisterManagedRelayEnvironmentCommand,
  useManagedRelayEnvironments,
} from "./managedRelayState";

const linkedAtFormatter = new Intl.DateTimeFormat(undefined, { dateStyle: "medium" });

function formatUsdCents(cents: number): string {
  return `$${(cents / 100).toFixed(2)}`;
}

function linkedAtLabel(value: string, resolve: MessageResolver): string {
  const linkedAt = new Date(value);
  return Number.isNaN(linkedAt.getTime())
    ? resolve(msg("Link date unavailable"))
    : resolve(msg("Linked {date}", { date: linkedAtFormatter.format(linkedAt) }));
}

function endpointLabel(
  environment: RelayClientEnvironmentRecord,
  resolve: MessageResolver,
): string {
  return environment.endpoint.providerKind === "cloudflare_tunnel"
    ? resolve(msg("Managed tunnel"))
    : resolve(msg("Activity publishing only"));
}

function confirmDeregister(environment: RelayClientEnvironmentRecord, onConfirm: () => void) {
  const title = m(msg("Deregister server?"));
  const message = m(
    msg(
      "“{environment}” will be removed from this account. {product} access will be revoked, any managed tunnel will be removed, and a host space will become available. Local connections on your devices are not changed.",
      { environment: environment.label, product: "Hanzo Link" },
    ),
  );
  Dialog.confirm({
    title,
    message,
    confirmText: m(msg("Deregister")),
    destructive: true,
    onConfirm,
  });
}

export function HanzoConnectProfilePage() {
  const gt = useGT();
  const m = useMessages();
  const {
    activeAccount,
    activeOrgId,
    organizations,
    billing,
    switchOrganization,
    refreshOrganizationsAndBilling,
  } = useHanzoMobileAccounts();
  const environmentsState = useManagedRelayEnvironments();
  const deregisterEnvironment = useAtomCommand(deregisterManagedRelayEnvironmentCommand, {
    reportFailure: false,
  });
  const refreshRelayEnvironments = useAtomCommand(relayEnvironmentDiscovery.refreshAfterMutation, {
    reportFailure: false,
  });
  const [deregisteringEnvironmentId, setDeregisteringEnvironmentId] =
    useState<EnvironmentId | null>(null);
  const mutationPendingRef = useRef(false);
  // Hide cached rows by linkedAt until refresh; re-linking changes linkedAt and restores visibility.
  const [removedEnvironments, setRemovedEnvironments] = useState<{
    readonly accountId: string | null;
    readonly linkedAtById: ReadonlyMap<EnvironmentId, string>;
  }>({ accountId: null, linkedAtById: new Map() });

  const handleDeregister = async (environment: RelayClientEnvironmentRecord) => {
    const accountId = environmentsState.accountId;
    if (!accountId || mutationPendingRef.current) return;

    mutationPendingRef.current = true;
    setDeregisteringEnvironmentId(environment.environmentId);
    const result = await deregisterEnvironment({
      accountId,
      environmentId: environment.environmentId,
    });
    mutationPendingRef.current = false;
    setDeregisteringEnvironmentId(null);

    if (result._tag === "Success") {
      setRemovedEnvironments((current) => {
        const linkedAtById = new Map(current.accountId === accountId ? current.linkedAtById : []);
        linkedAtById.set(environment.environmentId, environment.linkedAt);
        return { accountId, linkedAtById };
      });
      environmentsState.refresh();
      void refreshRelayEnvironments();
      return;
    }
    if (isAtomCommandInterrupted(result)) return;

    const cause = squashAtomCommandFailure(result);
    const message =
      cause instanceof Error
        ? (errorDisplayMessage(cause, m) ?? cause.message)
        : gt("Could not deregister the server.");
    const traceId = findErrorTraceId(cause);
    console.error("[hanzo-connect] Could not deregister environment", {
      environmentId: environment.environmentId,
      message,
      traceId,
      cause,
    });
    Dialog.alert(
      gt("Could not deregister server"),
      traceId ? gt("{message}\n\nTrace ID: {traceId}", { message, traceId }) : message,
      traceId
        ? [
            {
              text: gt("Copy trace ID"),
              onPress: () => copyTextWithHaptic(traceId, { target: "connection-trace-id" }),
            },
            { text: gt("OK"), style: "cancel" },
          ]
        : undefined,
    );
  };

  const handleRefreshAll = () => {
    environmentsState.refresh();
    void refreshOrganizationsAndBilling();
  };

  const openSurfaceUrl = (url: string) => {
    const targetUrl = buildHanzoSurfaceLaunchUrl(url, { orgId: activeOrgId });
    void Linking.openURL(targetUrl).catch(() => {});
  };

  const removedEnvironmentLinkedAt =
    removedEnvironments.accountId === environmentsState.accountId
      ? removedEnvironments.linkedAtById
      : new Map<EnvironmentId, string>();
  const environments = (environmentsState.data ?? []).filter(
    (environment) =>
      removedEnvironmentLinkedAt.get(environment.environmentId) !== environment.linkedAt,
  );
  const isInitialLoad =
    !environmentsState.accountId || (environmentsState.data === null && !environmentsState.error);
  const errorTraceId = environmentsState.errorTraceId;
  const environmentMenuActions = [
    {
      id: "deregister",
      title: gt("Deregister"),
      image: "trash",
      attributes: { destructive: true },
    },
  ] satisfies MenuAction[];

  const activePlanLabel = billing?.planName ?? "Hanzo Go ($8/mo)";
  const creditSummary = billing
    ? `${formatUsdCents(billing.creditBalanceCents)} credits · ${formatUsdCents(billing.monthlyUsedCents)} used`
    : "$25.00 credits · Unified Hanzo Cloud";

  return (
    <ScrollView
      className="flex-1 bg-profile-page"
      contentContainerClassName="pb-8"
      contentInsetAdjustmentBehavior="automatic"
      refreshControl={
        <RefreshControl
          refreshing={environmentsState.isPending && environmentsState.data !== null}
          onRefresh={handleRefreshAll}
        />
      }
    >
      {activeAccount ? (
        <>
          <ClerkSectionHeader>{gt("Organization context (X-Org-Id)")}</ClerkSectionHeader>
          <View className="border-b border-profile-border px-6 py-4">
            <Text className="text-sm text-profile-foreground-muted">
              {gt("Active org header: X-Org-Id = {org}", {
                org: activeOrgId ?? activeAccount.owner,
              })}
            </Text>
            <View className="mt-3 flex-row flex-wrap gap-2">
              {organizations.map((org) => {
                const selected = org.id === (activeOrgId ?? activeAccount.owner);
                return (
                  <Pressable
                    key={org.id}
                    accessibilityRole="button"
                    accessibilityLabel={gt("Switch organization to {org}", {
                      org: org.displayName,
                    })}
                    onPress={() => {
                      void switchOrganization(org.id);
                    }}
                    className={`rounded-full border px-3.5 py-1.5 ${
                      selected
                        ? "border-profile-foreground bg-profile-foreground"
                        : "border-profile-border bg-transparent"
                    }`}
                  >
                    <Text
                      className={`text-xs font-semibold ${
                        selected ? "text-profile-page" : "text-profile-foreground"
                      }`}
                    >
                      {`${org.displayName} (${org.id})`}
                    </Text>
                  </Pressable>
                );
              })}
            </View>
          </View>

          <ClerkSectionHeader>{gt("Subscription plan & usage")}</ClerkSectionHeader>
          <ClerkRow title={activePlanLabel} subtitle={creditSummary} />
          <View className="border-b border-profile-border px-6 py-3">
            <Text className="text-xs text-profile-foreground-muted">
              {HANZO_SUBSCRIPTION_PLANS.filter((p) => p.id !== "free")
                .map((p) => `${p.name} (${p.priceLabel})`)
                .join(" · ")}
            </Text>
          </View>
          <ClerkButtonRow
            label={gt("Open Billing Portal (billing.hanzo.ai)")}
            onPress={() => openSurfaceUrl(HANZO_BILLING_PORTAL_URL)}
          />

          <ClerkSectionHeader>{gt("Connected Hanzo surfaces")}</ClerkSectionHeader>
          {HANZO_ECOSYSTEM_SURFACES.filter((s) => s.id !== "hanzo-billing").map((surface) => (
            <Pressable
              key={surface.id}
              accessibilityRole="link"
              onPress={() => openSurfaceUrl(surface.url)}
              className="flex-row items-center justify-between border-b border-profile-border px-6 py-3.5 active:opacity-60"
            >
              <View className="min-w-0 flex-1 gap-0.5">
                <Text className="text-base text-profile-foreground" numberOfLines={1}>
                  {surface.label}
                </Text>
                <Text className="text-xs text-profile-foreground-muted" numberOfLines={1}>
                  {`${surface.hostname} · ${surface.description}`}
                </Text>
              </View>
              <SymbolView
                name="arrow.up.right"
                size={14}
                tintColorClassName="accent-profile-foreground-muted"
                type="monochrome"
              />
            </Pressable>
          ))}
        </>
      ) : null}

      <ClerkSectionHeader>{gt("Registered servers")}</ClerkSectionHeader>

      {environmentsState.error ? (
        <>
          <ClerkRow
            title={gt("Could not load {product} environments", { product: "Hanzo Link" })}
            subtitle={environmentsState.error}
          />
          {errorTraceId ? (
            <ClerkButtonRow
              label={gt("Copy trace ID")}
              onPress={() => {
                copyTextWithHaptic(errorTraceId, { target: "connection-trace-id" });
              }}
            />
          ) : null}
        </>
      ) : isInitialLoad ? (
        <View className="flex-row items-center gap-3 px-6 py-4">
          <AppActivityIndicator colorClassName={"accent-profile-foreground-muted"} size="small" />
          <Text className="text-base text-profile-foreground-muted">
            <T>Loading environments</T>
          </Text>
        </View>
      ) : environments.length > 0 ? (
        environments.map((environment) => (
          <ClerkRow
            key={environment.environmentId}
            title={environment.label}
            subtitle={`${linkedAtLabel(environment.linkedAt, m)} · ${endpointLabel(environment, m)}`}
            accessory={
              deregisteringEnvironmentId === environment.environmentId ? (
                <AppActivityIndicator
                  colorClassName={"accent-profile-foreground-muted"}
                  size="small"
                />
              ) : (
                <Menu
                  actions={environmentMenuActions}
                  isAnchoredToRight
                  onPressAction={() =>
                    confirmDeregister(environment, () => void handleDeregister(environment))
                  }
                >
                  <Pressable
                    accessibilityLabel={gt("Actions for {environment}", {
                      environment: environment.label,
                    })}
                    accessibilityRole="button"
                    disabled={deregisteringEnvironmentId !== null}
                    className="size-[30px] items-center justify-center active:opacity-60 disabled:opacity-50"
                  >
                    <View className="rotate-90">
                      <SymbolView
                        name="ellipsis"
                        size={18}
                        tintColorClassName={"accent-profile-foreground-muted"}
                        type="monochrome"
                      />
                    </View>
                  </Pressable>
                </Menu>
              )
            }
          />
        ))
      ) : (
        <ClerkRow
          title={gt("No servers registered")}
          subtitle={gt("Link a server from its local Settings to reach it through {product}.", {
            product: "Hanzo Link",
          })}
        />
      )}

      <Text className="px-6 pt-6 text-xs leading-normal text-profile-foreground-muted">
        <T>Connections on this device are managed in Settings.</T>
      </Text>
    </ScrollView>
  );
}

function ClerkSectionHeader(props: { readonly children: string }) {
  return (
    <Text className="min-h-4 border-b border-profile-border px-6 pt-8 pb-4 text-xs font-medium tracking-[0.3px] text-profile-foreground-muted uppercase">
      {props.children}
    </Text>
  );
}

function ClerkRow(props: {
  readonly title: string;
  readonly subtitle: string;
  readonly accessory?: ReactNode;
}) {
  return (
    <View
      collapsable={false}
      className="flex-row items-center gap-3 border-b border-profile-border px-6 py-4"
    >
      <View className="min-w-0 flex-1 gap-0.5">
        <Text className="min-h-[22px] text-base text-profile-foreground" numberOfLines={1}>
          {props.title}
        </Text>
        <Text className="min-h-5 text-sm text-profile-foreground-muted" numberOfLines={2}>
          {props.subtitle}
        </Text>
      </View>
      {props.accessory}
    </View>
  );
}

function ClerkButtonRow(props: { readonly label: string; readonly onPress: () => void }) {
  return (
    <Pressable
      accessibilityRole="button"
      onPress={props.onPress}
      className="border-b border-profile-border px-6 py-4 active:opacity-60"
    >
      <Text className="text-base font-semibold text-profile-foreground">{props.label}</Text>
    </Pressable>
  );
}
