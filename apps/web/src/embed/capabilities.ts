import { scopeProjectRef } from "@hanzodev/client-runtime/environment";
import { isScratchProject } from "@hanzodev/client-runtime/state/projects";
import type { EnvironmentId, ScopedThreadRef, ThreadId } from "@hanzodev/contracts";

import { useEnvironments } from "../state/environments";
import { useProject, useThreadShell } from "../state/entities";

/** `<environmentId>/<threadId>` as the core's address for a thread. */
export function threadRefOf(ref: string | null): ScopedThreadRef | null {
  if (ref === null) return null;
  const at = ref.indexOf("/");
  return at < 1
    ? null
    : { environmentId: ref.slice(0, at) as EnvironmentId, threadId: ref.slice(at + 1) as ThreadId };
}

/**
 * Whether the open thread has a workspace, which is what the code tools hang on. A thread on
 * the scratch project is a conversation: no checkout, branch, access chip or dock. With no
 * thread open there is nothing to act on either.
 */
export function useHasWorkspace(ref: string | null): boolean {
  const threadRef = threadRefOf(ref);
  const shell = useThreadShell(threadRef);
  const project = useProject(
    threadRef && shell ? scopeProjectRef(threadRef.environmentId, shell.projectId) : null,
  );
  const { environments } = useEnvironments();
  if (!threadRef || !shell || !project) return false;
  const scratchRoot =
    environments.find((environment) => environment.environmentId === threadRef.environmentId)
      ?.serverConfig?.scratchWorkspaceRoot ?? null;
  return !isScratchProject(project, scratchRoot);
}
