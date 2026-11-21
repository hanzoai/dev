import { resolveEnvironmentMachineKind, type ScopedProjectRef } from "@hanzodev/contracts";
import { scopedProjectKey, scopeProjectRef } from "@hanzodev/client-runtime/environment";
import { isScratchProject } from "@hanzodev/client-runtime/state/projects";
import { pickableProjectGroups } from "@hanzodev/client-runtime/state/project-grouping";
import { useAtomValue } from "@effect/atom-react";
import { T, useGT, useMessages } from "gt-react";
import { FolderGit2Icon, FolderGitIcon, FolderIcon, FolderPlusIcon } from "lucide-react";
import { Fragment, useRef, useState } from "react";

import { openCommandPalette } from "~/commandPaletteBus";
import {
  composerDraftHasUserContent,
  useComposerDraftStore,
  type DraftId,
} from "~/composerDraftStore";
import { useNewThreadHandler } from "~/hooks/useHandleNewThread";
import { useOrderWhileOpen } from "~/hooks/useOrderWhileOpen";
import { useProjectPin } from "~/hooks/useProjectGroupUpdate";
import { useScratchProject } from "~/hooks/useScratchProject";
import { shortcutLabelForCommand } from "~/keybindings";
import { seedDraftModelSelection } from "~/lib/seedDraftModelSelection";
import {
  buildSidebarProjectPickerEntries,
  projectGroupsSpanEnvironments,
} from "~/sidebarProjectGrouping";
import { useProjects } from "~/state/entities";
import { useEnvironments, usePrimaryEnvironmentId } from "~/state/environments";
import { primaryServerKeybindingsAtom } from "~/state/server";
import {
  resolveCurrentWorkspaceLabel,
  resolveEnvModeLabel,
  type EnvMode,
} from "../BranchToolbar.logic";
import { useSortedProjectGroups } from "../CommandPaletteLookups";
import { ComposerContextLabel } from "../ComposerContextLabel";
import { PreviousWorktreeItemContent } from "../PreviousWorktreeItemContent";
import { ProjectEnvironmentBadge } from "../ProjectEnvironmentBadge";
import { ProjectFavicon } from "../ProjectFavicon";
import { isProjectPinShortcut, ProjectPinButton } from "../ProjectPinButton";
import {
  Combobox,
  ComboboxEmpty,
  ComboboxItem,
  ComboboxList,
  ComboboxPopup,
  ComboboxSearchInput,
  ComboboxTrigger,
  useComboboxFilter,
} from "../ui/combobox";
import { Tooltip, TooltipPopup, TooltipTrigger } from "../ui/tooltip";
import { useComposerMenuProps } from "./composerEventScope";
import { ComposerControl } from "./ComposerControl";
import { DraftProjectName } from "./DraftProjectName";

const ADD_PROJECT_VALUE = "add-project";
const WORKSPACE_PREFIX = "workspace:";
const PREVIOUS_WORKTREE_VALUE = `${WORKSPACE_PREFIX}previous`;

interface MenuItem {
  readonly value: string;
  readonly label: string;
}

interface ProjectWorkspaceMenuProps {
  readonly draftId: DraftId;
  readonly activeProjectRef: ScopedProjectRef;
  readonly activeProjectTitle: string;
  /** Workspace modes need a Git repository; without one the menu only picks the project. */
  readonly showWorkspace: boolean;
  readonly effectiveEnvMode: EnvMode;
  readonly activeWorktreePath: string | null;
  readonly onEnvModeChange: (mode: EnvMode) => void;
  readonly previousWorktreeLabel: string | null;
  readonly previousWorktreeBranch: string | null;
  readonly onUsePreviousWorktree: () => void;
}

/**
 * One composer control for where a new thread runs: the project, then its
 * workspace (current checkout or a new worktree).
 */
export function ProjectWorkspaceMenu({
  draftId,
  activeProjectRef,
  activeProjectTitle,
  showWorkspace,
  effectiveEnvMode,
  activeWorktreePath,
  onEnvModeChange,
  previousWorktreeLabel,
  previousWorktreeBranch,
  onUsePreviousWorktree,
}: ProjectWorkspaceMenuProps) {
  const gt = useGT();
  const m = useMessages();
  const composerFloatingLayerProps = useComposerMenuProps();
  const projects = useProjects();
  const { environments } = useEnvironments();
  const primaryEnvironmentId = usePrimaryEnvironmentId();
  const keybindings = useAtomValue(primaryServerKeybindingsAtom);
  const setLogicalProjectDraftThreadId = useComposerDraftStore(
    (store) => store.setLogicalProjectDraftThreadId,
  );
  const getComposerDraft = useComposerDraftStore((store) => store.getComposerDraft);
  const { scratchWorkspaceRootFor } = useScratchProject();
  const openProjectDraft = useNewThreadHandler();
  const { canPinProjectGroup, toggleProjectGroupPin } = useProjectPin();
  const [open, setOpen] = useState(false);
  const [query, setQuery] = useState("");
  const filter = useComboboxFilter();
  const highlightedValueRef = useRef<string | null>(null);

  const environmentLabelById = new Map(
    environments.map((environment) => [environment.environmentId, environment.label] as const),
  );
  const projectGroups = useSortedProjectGroups(environmentLabelById);
  const showProjectEnvironments = projectGroupsSpanEnvironments(projectGroups);
  const environmentMachineById = new Map(
    environments.map(
      (environment) =>
        [
          environment.environmentId,
          resolveEnvironmentMachineKind(environment.serverConfig),
        ] as const,
    ),
  );
  const entries = buildSidebarProjectPickerEntries({
    groups: pickableProjectGroups(projectGroups),
    preferredProjectRef: activeProjectRef,
  }).filter(
    ({ targetProject }) =>
      !isScratchProject(targetProject, scratchWorkspaceRootFor(targetProject.environmentId)),
  );
  const entryByKey = new Map(entries.map((entry) => [entry.group.projectKey, entry] as const));
  const activeGroup =
    projectGroups.find((group) =>
      group.memberProjectRefs.some(
        (projectRef) => scopedProjectKey(projectRef) === scopedProjectKey(activeProjectRef),
      ),
    ) ?? null;
  const activeProjectKey = activeGroup?.projectKey ?? "";

  const workspaceItems: readonly MenuItem[] = showWorkspace
    ? [
        {
          value: `${WORKSPACE_PREFIX}local`,
          label: resolveCurrentWorkspaceLabel(activeWorktreePath, m),
        },
        { value: `${WORKSPACE_PREFIX}worktree`, label: resolveEnvModeLabel("worktree", m) },
        ...(previousWorktreeLabel
          ? [{ value: PREVIOUS_WORKTREE_VALUE, label: previousWorktreeLabel }]
          : []),
      ]
    : [];

  // Pinning from a row keeps rows in place while the menu is open.
  const projectItems: readonly MenuItem[] = useOrderWhileOpen(
    [
      ...entries.map(({ group }) => ({ value: group.projectKey, label: group.displayName })),
      { value: ADD_PROJECT_VALUE, label: gt("Add project") },
    ],
    (item) => item.value,
    open,
  );
  const search = query.trim();
  const filteredProjectItems =
    search.length === 0
      ? projectItems
      : projectItems.filter(
          (item) =>
            item.value !== ADD_PROJECT_VALUE &&
            filter.contains(item, search, (candidate) => candidate.label),
        );
  const allItems = [...projectItems, ...workspaceItems];
  const visibleItems =
    search.length === 0 ? [...filteredProjectItems, ...workspaceItems] : filteredProjectItems;
  const selectedItem = projectItems.find((item) => item.value === activeProjectKey) ?? null;

  // Retargeting an empty draft would overwrite the project's draft and strand browser tabs.
  const selectProject = (project: (typeof projects)[number], logicalProjectKey: string) => {
    const projectRef = scopeProjectRef(project.environmentId, project.id);
    if (!composerDraftHasUserContent(getComposerDraft(draftId))) {
      void openProjectDraft(projectRef);
      return;
    }
    setLogicalProjectDraftThreadId(logicalProjectKey, projectRef, draftId);
    seedDraftModelSelection(draftId, projectRef);
  };

  const workspaceLabel =
    effectiveEnvMode === "worktree"
      ? resolveEnvModeLabel("worktree", m)
      : resolveCurrentWorkspaceLabel(activeWorktreePath, m);
  const WorkspaceIcon =
    effectiveEnvMode === "worktree"
      ? FolderGit2Icon
      : activeWorktreePath
        ? FolderGitIcon
        : FolderIcon;
  const shortcutLabel = shortcutLabelForCommand(keybindings, "composer.workspace");
  const triggerTitle = showWorkspace
    ? `${activeProjectTitle} · ${workspaceLabel}`
    : activeProjectTitle;

  return (
    <Combobox
      items={allItems}
      filteredItems={visibleItems}
      autoHighlight
      itemToStringLabel={(item) => item.label}
      isItemEqualToValue={(a, b) => a.value === b.value}
      open={open}
      onItemHighlighted={(item) => {
        highlightedValueRef.current = item?.value ?? null;
      }}
      onOpenChange={(next) => {
        setOpen(next);
        setQuery("");
      }}
      value={selectedItem}
      onValueChange={(item) => {
        if (!item) return;
        setOpen(false);
        if (item.value === ADD_PROJECT_VALUE) {
          openCommandPalette({ open: "add-project" });
          return;
        }
        if (item.value === PREVIOUS_WORKTREE_VALUE) {
          onUsePreviousWorktree();
          return;
        }
        if (item.value.startsWith(WORKSPACE_PREFIX)) {
          onEnvModeChange(item.value === `${WORKSPACE_PREFIX}worktree` ? "worktree" : "local");
          return;
        }
        const entry = entryByKey.get(item.value);
        if (!entry || item.value === activeProjectKey) return;
        selectProject(entry.targetProject, entry.group.projectKey);
      }}
    >
      <Tooltip>
        <TooltipTrigger
          render={
            <ComboboxTrigger
              render={<ComposerControl size="xs" />}
              className="min-w-0 shrink"
              aria-label={gt("Project and workspace: {context}", { context: triggerTitle })}
              data-composer-shortcut="composer.workspace"
              data-composer-context-control
              data-draft-project-trigger=""
            />
          }
        >
          <WorkspaceIcon className="size-3" />
          <ComposerContextLabel>
            <DraftProjectName value={activeProjectTitle} singleLine />
          </ComposerContextLabel>
        </TooltipTrigger>
        <TooltipPopup>
          {triggerTitle}
          {shortcutLabel ? ` · ${shortcutLabel}` : null}
        </TooltipPopup>
      </Tooltip>
      <ComboboxPopup
        align="start"
        side="top"
        className="w-72 overflow-hidden"
        {...composerFloatingLayerProps}
      >
        <ComboboxSearchInput
          aria-label={gt("Search projects")}
          placeholder={gt("Search projects...")}
          value={query}
          onChange={(event) => setQuery(event.target.value)}
          onKeyDown={(event) => {
            const highlighted = highlightedValueRef.current;
            const group = highlighted ? entryByKey.get(highlighted)?.group : undefined;
            if (!group || !canPinProjectGroup(group) || !isProjectPinShortcut(event, keybindings)) {
              return;
            }
            event.preventDefault();
            event.stopPropagation();
            toggleProjectGroupPin(group);
          }}
        />
        <ComboboxEmpty>
          <T>No matching projects.</T>
        </ComboboxEmpty>
        <ComboboxList>
          {(item: MenuItem) => {
            const entry = entryByKey.get(item.value);
            const isWorkspace = item.value.startsWith(WORKSPACE_PREFIX);
            const isFirstWorkspace = item.value === workspaceItems[0]?.value;
            return (
              <Fragment key={item.value}>
                {isFirstWorkspace ? (
                  <div role="separator" className="mx-1 my-1 h-px bg-border" />
                ) : null}
                <ComboboxItem hideIndicator value={item}>
                  {item.value === ADD_PROJECT_VALUE ? (
                    <FolderPlusIcon className="size-4 shrink-0" />
                  ) : entry ? (
                    <ProjectFavicon project={entry.group} className="size-4 shrink-0" />
                  ) : item.value === `${WORKSPACE_PREFIX}worktree` ? (
                    <FolderGit2Icon className="size-4 shrink-0" />
                  ) : item.value === PREVIOUS_WORKTREE_VALUE ? null : (
                    <FolderIcon className="size-4 shrink-0" />
                  )}
                  {item.value === PREVIOUS_WORKTREE_VALUE ? (
                    <PreviousWorktreeItemContent branch={previousWorktreeBranch} />
                  ) : (
                    <Tooltip>
                      <TooltipTrigger render={<span className="min-w-0 flex-1 text-sm" />}>
                        <DraftProjectName value={item.label} />
                      </TooltipTrigger>
                      <TooltipPopup side="top">{item.label}</TooltipPopup>
                    </Tooltip>
                  )}
                  {isWorkspace &&
                  (item.value === `${WORKSPACE_PREFIX}worktree`
                    ? effectiveEnvMode === "worktree"
                    : item.value === `${WORKSPACE_PREFIX}local` &&
                      effectiveEnvMode !== "worktree") ? (
                    <span aria-hidden className="size-1.5 shrink-0 rounded-full bg-current" />
                  ) : null}
                  {entry && canPinProjectGroup(entry.group) ? (
                    <ProjectPinButton
                      name={entry.group.displayName}
                      pinned={entry.group.pinned}
                      shortcutLabel={shortcutLabelForCommand(keybindings, "thread.pin")}
                      onToggle={() => toggleProjectGroupPin(entry.group)}
                    />
                  ) : null}
                  {entry && showProjectEnvironments ? (
                    <ProjectEnvironmentBadge
                      group={entry.group}
                      primaryEnvironmentId={primaryEnvironmentId}
                      machineByEnvironmentId={environmentMachineById}
                    />
                  ) : null}
                </ComboboxItem>
              </Fragment>
            );
          }}
        </ComboboxList>
      </ComboboxPopup>
    </Combobox>
  );
}
