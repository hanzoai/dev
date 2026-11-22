import { useGT } from "gt-react-native";
import { ScreenScrollView as ScrollView } from "../../components/ui/ScreenScrollView";
import { useNavigation } from "@react-navigation/native";
import { useAtomValue } from "@effect/atom-react";
import { managedRelaySessionAtom } from "@hanzodev/client-runtime/relay";
import type { EnvironmentId } from "@hanzodev/contracts";
import { useEffect, useRef, useState } from "react";
import { RefreshControl } from "../../components/ui/RefreshControl";
import { Pressable, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { SettingsScreen } from "./components/SettingsScreen";
import { CloudEnvironmentRows } from "../connection/CloudEnvironmentRows";
import { LocalEnvironmentList } from "../connection/LocalEnvironmentList";
import { PullRequestRoutingSettings } from "../connection/PullRequestRoutingSettings";
import { splitEnvironmentSections } from "../connection/environmentSections";
import { useConnectionController } from "../connection/useConnectionController";
import { useCloudChat } from "../chat/cloud-chat";
import { useAuth } from "../cloud/hanzoMobileAuth";
import { SymbolView } from "../../components/ui/AppSymbol";
import { Button } from "../../components/ui/Button";
import { AppText as Text } from "../../components/ui/AppText";
import { useDrawerEnvironments } from "../layout/workspace/useDrawerEnvironments";
import { openWorkspaceSection } from "../layout/workspace/workspace-section-store";
import { useUniwindTheme } from "../../lib/useUniwindTheme";
import { useRemoteConnections } from "../../state/use-remote-environment-registry";
import { relayEnvironmentDiscovery } from "../../state/relay";
import { useAtomCommand } from "../../state/use-atom-command";
import {
  applyShowcaseLocalEnvironmentDisplayUrls,
  resolveShowcaseEnvironmentUpdateDisplayUrl,
  SHOWCASE_AVAILABLE_CLOUD_ENVIRONMENTS,
  SHOWCASE_CONNECTED_CLOUD_ENVIRONMENTS,
} from "../showcase/showcaseEnvironmentRows";

const SHOWCASE_ENABLED = process.env.EXPO_PUBLIC_SHOWCASE === "1";

export function SettingsEnvironmentsRouteScreen() {
  const gt = useGT();
  const {
    connectedEnvironments,
    onReconnectEnvironment,
    onRemoveEnvironmentPress,
    onSetEnvironmentEnabled,
    onUpdateEnvironment,
  } = useRemoteConnections();
  const navigation = useNavigation();
  const { isSignedIn } = useAuth({ treatPendingAsSignedOut: false });
  const chat = useCloudChat();
  const controller = useConnectionController();
  const { cloud } = useDrawerEnvironments();
  const insets = useSafeAreaInsets();
  const environmentSections = splitEnvironmentSections({
    connectedEnvironments,
    cloudEnvironments: null,
  });
  const localEnvironments = SHOWCASE_ENABLED
    ? applyShowcaseLocalEnvironmentDisplayUrls(environmentSections.localEnvironments)
    : environmentSections.localEnvironments;
  const connectedCloudEnvironments = SHOWCASE_ENABLED
    ? SHOWCASE_CONNECTED_CLOUD_ENVIRONMENTS
    : environmentSections.connectedCloudEnvironments;
  const headerIconColor = useUniwindTheme()["--color-icon"];
  const relaySession = useAtomValue(managedRelaySessionAtom);
  const discovery = useAtomValue(relayEnvironmentDiscovery.stateValueAtom);
  const refreshRelayEnvironments = useAtomCommand(
    relayEnvironmentDiscovery.refresh,
    "relay environment refresh",
  );
  const [isRefreshingCloud, setIsRefreshingCloud] = useState(false);
  const [launchingSandbox, setLaunchingSandbox] = useState(false);
  const cloudRefreshPendingRef = useRef(false);

  async function refreshCloudEnvironments() {
    if (!relaySession || cloudRefreshPendingRef.current) return;
    cloudRefreshPendingRef.current = true;
    setIsRefreshingCloud(true);
    await refreshRelayEnvironments();
    cloudRefreshPendingRef.current = false;
    setIsRefreshingCloud(false);
  }

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- ensures /v1/sources discovery runs when opening Environments while signed in
  useEffect(() => {
    if (relaySession && !discovery.listed && !discovery.refreshing) {
      void refreshCloudEnvironments();
    }
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- Discovery state is the trigger; refreshCloudEnvironments is rebuilt each render and guards against overlapping runs itself.
  }, [relaySession, discovery.listed, discovery.refreshing]);

  const openCloudChat = () => {
    openWorkspaceSection("chat");
    navigation.navigate("Home");
  };

  const launchCloudSandbox = () => {
    if (launchingSandbox) return;
    setLaunchingSandbox(true);
    void chat.runInCloud().finally(() => {
      setLaunchingSandbox(false);
      openCloudChat();
    });
  };

  const openEnvironment = (environmentId: EnvironmentId) => {
    navigation.navigate("SettingsEnvironmentDetail", { environmentId });
  };
  const handleUpdateEnvironment = (
    environmentId: EnvironmentId,
    updates: { readonly label: string; readonly displayUrl: string },
  ) => {
    if (!SHOWCASE_ENABLED) return onUpdateEnvironment(environmentId, updates);
    const actualEnvironment = environmentSections.localEnvironments.find(
      (environment) => environment.environmentId === environmentId,
    );
    const presentedEnvironment = localEnvironments.find(
      (environment) => environment.environmentId === environmentId,
    );
    return onUpdateEnvironment(environmentId, {
      ...updates,
      displayUrl:
        actualEnvironment && presentedEnvironment
          ? resolveShowcaseEnvironmentUpdateDisplayUrl({
              actualDisplayUrl: actualEnvironment.displayUrl,
              presentedDisplayUrl: presentedEnvironment.displayUrl,
              submittedDisplayUrl: updates.displayUrl,
            })
          : updates.displayUrl,
    });
  };

  const hasCloudOrLinked =
    isSignedIn ||
    connectedCloudEnvironments.length > 0 ||
    controller.availableRelayEnvironments.length > 0;

  return (
    <SettingsScreen
      title={gt("Environments")}
      refresh={
        relaySession
          ? {
              accessibilityLabel: gt("Refresh cloud environments"),
              disabled: isRefreshingCloud,
              onPress: () => void refreshCloudEnvironments(),
            }
          : undefined
      }
      actions={[
        {
          accessibilityLabel: gt("Add environment"),
          icon: "plus",
          tintColor: headerIconColor,
          onPress: () => navigation.navigate("SettingsEnvironmentNew"),
        },
      ]}
    >
      <ScrollView
        alwaysBounceVertical
        contentInsetAdjustmentBehavior="automatic"
        showsVerticalScrollIndicator={false}
        className="flex-1"
        contentContainerClassName="gap-4 px-5 pt-4"
        contentContainerStyle={{
          paddingBottom: Math.max(insets.bottom, 18) + 18,
        }}
        refreshControl={
          relaySession ? (
            <RefreshControl
              refreshing={isRefreshingCloud}
              onRefresh={() => void refreshCloudEnvironments()}
            />
          ) : undefined
        }
      >
        {/* First-class Hanzo Cloud environment: always ready to chat when signed in */}
        <View className="gap-2">
          <View className="px-1">
            <Text className="text-sm font-hanzo-bold uppercase text-foreground-muted">
              {gt("Hanzo Cloud")}
            </Text>
          </View>
          <View className="gap-3 rounded-[24px] bg-grouped-card p-4">
            <View className="flex-row items-center gap-3">
              <View className="h-10 w-10 items-center justify-center rounded-2xl bg-subtle">
                <SymbolView
                  name="cloud"
                  size={20}
                  tintColorClassName="accent-foreground"
                  type="monochrome"
                />
              </View>
              <View className="min-w-0 flex-1 gap-0.5">
                <View className="flex-row items-center gap-2">
                  <View
                    className={
                      isSignedIn
                        ? "h-2 w-2 rounded-full bg-emerald-500"
                        : "h-2 w-2 rounded-full bg-border"
                    }
                  />
                  <Text className="text-base font-hanzo-bold text-foreground" numberOfLines={1}>
                    {gt("Hanzo Cloud")}
                  </Text>
                </View>
                <Text className="text-xs text-foreground-muted" numberOfLines={1}>
                  {isSignedIn
                    ? gt("Connected · api.hanzo.ai · {model}", { model: chat.state.model })
                    : gt("Sign in to chat with Hanzo Cloud and auto-link desktop machines")}
                </Text>
              </View>
            </View>
            <View className="flex-row flex-wrap gap-2">
              {isSignedIn ? (
                <>
                  <Button label={gt("Open Cloud Chat")} tone="primary" onPress={openCloudChat} />
                  {cloud?.available ? (
                    <Button
                      label={launchingSandbox ? gt("Starting…") : gt("Cloud Sandbox")}
                      tone="secondary"
                      disabled={launchingSandbox}
                      onPress={launchCloudSandbox}
                    />
                  ) : null}
                  <Button
                    label={gt("Models")}
                    tone="secondary"
                    onPress={() => navigation.navigate("ChatLandingModelSettings")}
                  />
                </>
              ) : (
                <Button
                  label={gt("Sign in to Hanzo")}
                  tone="primary"
                  onPress={() => navigation.navigate("SettingsAuth")}
                />
              )}
            </View>
          </View>
        </View>

        {/* Keep connected relays visible and removable without cloud config or sign-in; this component gates discovery. */}
        <CloudEnvironmentRows
          connectedCloudEnvironments={connectedCloudEnvironments}
          onOpenEnvironment={openEnvironment}
          onSetEnvironmentEnabled={onSetEnvironmentEnabled}
          onRemoveEnvironment={onRemoveEnvironmentPress}
          {...(SHOWCASE_ENABLED
            ? {
                showcaseAvailableEnvironments: SHOWCASE_AVAILABLE_CLOUD_ENVIRONMENTS,
                showcaseSignedIn: true,
              }
            : {})}
        />

        {localEnvironments.length > 0 || !hasCloudOrLinked ? (
          <View className="gap-2">
            {localEnvironments.length > 0 ? (
              <View className="px-1">
                <Text className="text-sm font-hanzo-bold uppercase text-foreground-muted">
                  {gt("Local & Direct Machines")}
                </Text>
              </View>
            ) : null}
            <LocalEnvironmentList
              environments={localEnvironments}
              expandedId={null}
              onToggle={openEnvironment}
              opensDetails
              onReconnect={onReconnectEnvironment}
              onRemove={onRemoveEnvironmentPress}
              onSetEnabled={onSetEnvironmentEnabled}
              onUpdate={handleUpdateEnvironment}
            />
          </View>
        ) : (
          <View className="gap-2">
            <View className="px-1">
              <Text className="text-sm font-hanzo-bold uppercase text-foreground-muted">
                {gt("Local & Direct Machines")}
              </Text>
            </View>
            <Pressable
              accessibilityRole="button"
              accessibilityLabel={gt("Add local or Tailscale environment")}
              onPress={() => navigation.navigate("SettingsEnvironmentNew")}
              className="flex-row items-center gap-3 rounded-[24px] bg-grouped-card px-4 py-3.5 active:opacity-70"
            >
              <SymbolView
                name="plus"
                size={18}
                tintColorClassName="accent-foreground"
                type="monochrome"
              />
              <View className="min-w-0 flex-1 gap-0.5">
                <Text className="text-base font-medium text-foreground">
                  {gt("Pair a local or Tailscale machine")}
                </Text>
                <Text className="text-xs text-foreground-muted">
                  {gt("Scan a QR code or enter a hanzod address manually")}
                </Text>
              </View>
              <SymbolView name="chevron.right" size={12} tintColorClassName="accent-icon-subtle" />
            </Pressable>
          </View>
        )}

        <PullRequestRoutingSettings />
      </ScrollView>
    </SettingsScreen>
  );
}
