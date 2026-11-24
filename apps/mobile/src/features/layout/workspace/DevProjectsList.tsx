import { scopedProjectKey } from "@hanzodev/client-runtime/environment";
import { useAtomValue } from "@effect/atom-react";
import { useNavigation } from "@react-navigation/native";
import { useGT } from "gt-react-native";
import { useMemo } from "react";
import { Pressable, ScrollView, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { SymbolView } from "../../../components/ui/AppSymbol";
import { AppText as Text } from "../../../components/ui/AppText";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { useProjects } from "../../../state/entities";
import { steadyNavigationThreadShellsAtom } from "../../../state/threads";
import { useWorkspaceEnvironments } from "../../../state/workspace";
import { useHomeListOptions } from "../../home/home-list-options";
import { openSettings } from "../../settings/settings-navigation";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import { showDevProjects } from "./workspace-section-store";

/** Dev's projects list: every active project across connected machines, opening its threads on tap. */
export function DevProjectsList() {
  const gt = useGT();
  const { colors } = useControlTheme();
  const insets = useSafeAreaInsets();
  const navigation = useNavigation();
  const selectTab = useSelectWorkspaceTab();
  const projects = useProjects();
  const threads = useAtomValue(steadyNavigationThreadShellsAtom);
  const environments = useWorkspaceEnvironments();

  const availableEnvironmentIds = useMemo(
    () => new Set(environments.map((environment) => environment.environmentId)),
    [environments],
  );
  const { setSelectedProjectKey, setSelectedEnvironmentId } =
    useHomeListOptions(availableEnvironmentIds);

  const envLabelById = useMemo(
    () =>
      new Map(
        environments.map((environment) => [
          environment.environmentId,
          environment.environmentLabel,
        ]),
      ),
    [environments],
  );
  const showEnvironmentLabel = environments.length > 1;

  const threadCountByProjectKey = useMemo(() => {
    const counts = new Map<string, number>();
    for (const thread of threads) {
      if (thread.archivedAt != null || thread.projectId == null) continue;
      const key = scopedProjectKey({
        environmentId: thread.environmentId,
        projectId: thread.projectId,
      });
      counts.set(key, (counts.get(key) ?? 0) + 1);
    }
    return counts;
  }, [threads]);

  const activeProjects = useMemo(
    () =>
      projects
        .filter((project) => project.archivedAt == null)
        .sort((left, right) => left.title.localeCompare(right.title)),
    [projects],
  );

  const openProjectThreads = (projectKey: string | null) => {
    setSelectedEnvironmentId(null);
    setSelectedProjectKey(projectKey);
    showDevProjects(false);
    selectTab("threads", "dev");
  };

  return (
    <ScrollView
      className="flex-1"
      contentInsetAdjustmentBehavior="automatic"
      contentContainerClassName="gap-0.5 px-4 pt-3"
      contentContainerStyle={{ paddingBottom: insets.bottom + 16 }}
    >
      {activeProjects.length === 0 ? (
        <Pressable
          accessibilityRole="button"
          onPress={() => openSettings(navigation, "SettingsEnvironments")}
          className="px-3 py-6"
        >
          <Text className="text-center text-base text-foreground-muted">
            {gt("No projects yet. Connect a machine in Environments to browse its projects.")}
          </Text>
        </Pressable>
      ) : (
        <>
          <Pressable
            accessibilityRole="button"
            accessibilityLabel={gt("All projects")}
            onPress={() => openProjectThreads(null)}
            className="h-12 flex-row items-center gap-3 rounded-xl px-3"
          >
            <SymbolView name="square.grid.2x2" size={18} tintColor={colors["--color-foreground"]} />
            <Text className="flex-1 text-base text-foreground" numberOfLines={1}>
              {gt("All projects")}
            </Text>
            <Text className="text-xs text-foreground-muted">{activeProjects.length}</Text>
          </Pressable>
          {activeProjects.map((project) => {
            const key = scopedProjectKey({
              environmentId: project.environmentId,
              projectId: project.id,
            });
            const count = threadCountByProjectKey.get(key) ?? 0;
            const envLabel = showEnvironmentLabel
              ? envLabelById.get(project.environmentId)
              : undefined;
            const subtitle = envLabel
              ? `${envLabel} · ${project.workspaceRoot}`
              : project.workspaceRoot;
            return (
              <Pressable
                key={key}
                accessibilityRole="button"
                accessibilityLabel={project.title}
                onPress={() => openProjectThreads(key)}
                className="min-h-[52px] flex-row items-center gap-3 rounded-xl px-3 py-2"
              >
                <SymbolView name="folder" size={18} tintColor={colors["--color-foreground"]} />
                <View className="min-w-0 flex-1">
                  <Text className="text-base text-foreground" numberOfLines={1}>
                    {project.title}
                  </Text>
                  <Text className="text-xs text-foreground-muted" numberOfLines={1}>
                    {subtitle}
                  </Text>
                </View>
                {count > 0 ? <Text className="text-xs text-foreground-muted">{count}</Text> : null}
              </Pressable>
            );
          })}
        </>
      )}
    </ScrollView>
  );
}
