import { useAtomValue } from "@effect/atom-react";
import { useEffect, useMemo } from "react";
import { Platform } from "react-native";

import { useCloudChat } from "../features/chat/cloud-chat";
import { useHanzoMobileAccounts } from "../features/cloud/hanzoMobileAuth";
import { relayEnvironmentDiscovery } from "../state/relay";
import { useWorkspaceEnvironments } from "../state/workspace";
import {
  buildWidgetConnectionStatus,
  syncHanzoWidgetSnapshot,
  type HanzoWidgetMachine,
  type HanzoWidgetSnapshot,
  type HanzoWidgetThread,
} from "./hanzoWidgetBridge";

/**
 * Keeps the native iOS Home Screen widgets (`AskHanzo`, `HanzoBrief`) synchronized with live
 * Hanzo account, Hanzo Cloud, linked desktop machines, and recent chat threads via shared
 * App Group UserDefaults + `WidgetCenter.shared.reloadAllTimelines()`.
 */
export function HanzoWidgetSyncCoordinator() {
  const { activeAccount } = useHanzoMobileAccounts();
  const chat = useCloudChat();
  const connectedEnvironments = useWorkspaceEnvironments();
  const discovery = useAtomValue(relayEnvironmentDiscovery.stateValueAtom);

  const isSignedIn = activeAccount !== null;

  const snapshot = useMemo<HanzoWidgetSnapshot>(() => {
    const machineById = new Map<string, HanzoWidgetMachine>();

    for (const env of connectedEnvironments) {
      const id = String(env.environmentId);
      machineById.set(id, {
        id,
        name: env.environmentLabel,
        online: env.connectionState === "connected",
        kind: env.isRelayManaged ? "desktop" : "local",
      });
    }

    for (const entry of discovery.environments.values()) {
      const id = String(entry.environment.environmentId);
      const existing = machineById.get(id);
      const discoveredOnline =
        entry.availability === "online" || entry.environment.state === "online";
      machineById.set(id, {
        id,
        name: existing?.name || entry.environment.label,
        online: existing ? existing.online || discoveredOnline : discoveredOnline,
        kind: "desktop",
      });
    }

    const desktopMachines = [...machineById.values()].sort(
      (a, b) => Number(b.online) - Number(a.online) || a.name.localeCompare(b.name),
    );
    const onlineMachinesCount = desktopMachines.filter((m) => m.online).length;
    const environmentsCount = (isSignedIn ? 1 : 0) + desktopMachines.length;

    const machines: HanzoWidgetMachine[] = [
      ...(isSignedIn
        ? [
            {
              id: "hanzo-cloud",
              name: "Hanzo Cloud",
              online: true,
              kind: "cloud" as const,
            },
          ]
        : []),
      ...desktopMachines.slice(0, 6),
    ];

    const activeThreadKey = chat.state.threadKey;
    const activePreview =
      chat.state.messages.length > 0
        ? (chat.state.messages[chat.state.messages.length - 1]?.content ?? "")
            .replace(/\s+/g, " ")
            .trim()
            .slice(0, 96)
        : "";

    const recentThreads: HanzoWidgetThread[] = chat.state.threads.slice(0, 5).map((thread) => ({
      key: thread.key,
      title: thread.title.trim() || "New chat",
      preview:
        activeThreadKey === thread.key && activePreview
          ? activePreview
          : thread.remoteId
            ? "Synced with Hanzo Cloud"
            : "Saved on this iPhone",
      updatedAt: thread.updatedAt,
      pinned: thread.pinned,
    }));

    return {
      signedIn: isSignedIn,
      accountName: activeAccount?.displayName ?? null,
      accountEmail: activeAccount?.email ?? null,
      cloudAvailable: isSignedIn,
      cloudModel: chat.state.model,
      connectionStatus: buildWidgetConnectionStatus({
        signedIn: isSignedIn,
        onlineMachinesCount,
        totalMachinesCount: desktopMachines.length,
      }),
      environmentsCount,
      onlineMachinesCount,
      machines,
      recentThreads,
      updatedAt: Date.now(),
    };
  }, [
    isSignedIn,
    activeAccount?.displayName,
    activeAccount?.email,
    connectedEnvironments,
    discovery.environments,
    chat.state.model,
    chat.state.threadKey,
    chat.state.messages,
    chat.state.threads,
  ]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- pushes live Hanzo environments and chat threads to the iOS WidgetKit extension
  useEffect(() => {
    if (Platform.OS !== "ios") return;
    void syncHanzoWidgetSnapshot(snapshot);
  }, [snapshot]);

  return null;
}
