import { isScratchProject } from "@hanzodev/client-runtime/state/projects";
import type { EnvironmentId, ProjectId } from "@hanzodev/contracts";

interface ProjectOption {
  readonly id: ProjectId;
  readonly environmentId: EnvironmentId;
  readonly title: string;
  readonly workspaceRoot: string;
  readonly isScratch?: true;
  readonly deletedAt?: unknown;
}

/**
 * The projects a conversation can take on in place: the environment's own, never the scratch
 * project it is in now or one that is gone.
 */
export function attachableProjects<P extends ProjectOption>(
  projects: ReadonlyArray<P>,
  input: {
    readonly environmentId: EnvironmentId;
    readonly currentProjectId: ProjectId;
    readonly scratchWorkspaceRoot: string | null;
  },
): ReadonlyArray<P> {
  return projects.filter(
    (project) =>
      project.environmentId === input.environmentId &&
      project.id !== input.currentProjectId &&
      !project.deletedAt &&
      !isScratchProject(project, input.scratchWorkspaceRoot),
  );
}
