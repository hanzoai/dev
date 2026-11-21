import type { EnvironmentId, ProjectId, ThreadId } from "@hanzodev/contracts";
import { T, useGT } from "gt-react";
import { FolderGitIcon } from "lucide-react";

import { threadEnvironment } from "../../state/threads";
import { useProjects } from "../../state/entities";
import { useOrchestrationCommand } from "../../state/use-orchestration-command";
import { Menu, MenuItem, MenuPopup, MenuTrigger } from "../ui/menu";
import { toastManager } from "../ui/toast";
import { attachableProjects } from "./attachProject.logic";

/**
 * A conversation becomes a dev thread in place: attach a project and the thread keeps its history
 * while the git, checkout and access controls appear. It is offered only on a thread with no workspace.
 */
export function AttachProjectControl(props: {
  readonly environmentId: EnvironmentId;
  readonly threadId: ThreadId;
  readonly currentProjectId: ProjectId;
  readonly scratchWorkspaceRoot: string | null;
}) {
  const gt = useGT();
  const projects = attachableProjects(useProjects(), props);
  const setProject = useOrchestrationCommand(threadEnvironment.setProject, { reportFailure: false });
  if (projects.length === 0) return null;
  return (
    <Menu>
      <MenuTrigger
        data-testid="attach-project"
        className="inline-flex items-center gap-1.5 rounded-md px-2 py-1 text-xs text-muted-foreground hover:bg-accent"
      >
        <FolderGitIcon className="size-3.5" />
        <T>Attach a project</T>
      </MenuTrigger>
      <MenuPopup align="start" side="top">
        {projects.map((project) => (
          <MenuItem
            key={project.id}
            onClick={() => {
              void setProject({
                environmentId: props.environmentId,
                threadId: props.threadId,
                projectId: project.id,
              }).then((result) => {
                if (result._tag !== "Success") {
                  toastManager.add({
                    type: "error",
                    title: gt("Could not attach {project}", { project: project.title }),
                  });
                }
              });
            }}
          >
            <FolderGitIcon className="size-4" />
            <span>{project.title}</span>
          </MenuItem>
        ))}
      </MenuPopup>
    </Menu>
  );
}
