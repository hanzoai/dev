import type { DraftId } from "~/composerDraftStore";
import { useComposerDraftStore } from "~/composerDraftStore";
import { type ScopedProjectRef } from "@hanzodev/contracts";
import { isScratchProject } from "@hanzodev/client-runtime/state/projects";
import { isTriagePrompt } from "@hanzodev/shared/triagePrompt";
import { T } from "gt-react";

import { openCommandPalette } from "~/commandPaletteBus";
import { useScratchProject } from "~/hooks/useScratchProject";
import { useProjects } from "~/state/entities";
import { useEnvironments } from "~/state/environments";
import { DraftHeroProjectHeading } from "./DraftHeroProjectHeading";
import { TriageDraftHero } from "./TriageDraftHero";

interface DraftHeroHeadlineProps {
  readonly draftId: DraftId | null;
  readonly activeProjectRef: ScopedProjectRef | null;
}

/** The new-thread headline. The project is chosen in the composer's project chip. */
export function DraftHeroHeadline({ draftId, activeProjectRef }: DraftHeroHeadlineProps) {
  const projects = useProjects();
  const { environments } = useEnvironments();
  const { scratchWorkspaceRootFor } = useScratchProject();
  // A boolean, so typing in a draft does not re-render the hero.
  const isTriageDraftPrompt = useComposerDraftStore((store) =>
    draftId ? isTriagePrompt(store.getComposerDraft(draftId)?.prompt ?? "") : false,
  );
  const activeProject =
    activeProjectRef === null
      ? null
      : (projects.find(
          (project) =>
            project.environmentId === activeProjectRef.environmentId &&
            project.id === activeProjectRef.projectId,
        ) ?? null);
  const isScratchDraft =
    activeProject !== null &&
    isScratchProject(activeProject, scratchWorkspaceRootFor(activeProject.environmentId));

  if (isScratchDraft && isTriageDraftPrompt) {
    const environment = environments.find(
      (entry) => entry.environmentId === activeProject.environmentId,
    );
    return (
      <TriageDraftHero
        machineLabel={environment?.label ?? null}
        serverVersion={environment?.serverConfig?.environment.serverVersion ?? null}
      />
    );
  }

  return (
    <div className="mx-auto flex w-full max-w-5xl flex-col items-center">
      {activeProject === null && projects.length === 0 ? (
        <h1 className="w-full text-center font-normal text-2xl text-foreground tracking-tight sm:text-3xl">
          <button
            type="button"
            onClick={() => openCommandPalette({ open: "add-project" })}
            className="cursor-pointer rounded-sm focus-visible:outline-hidden focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-ring"
          >
            <T>Add a project to start</T>
          </button>
        </h1>
      ) : (
        <DraftHeroProjectHeading />
      )}
    </div>
  );
}
