import { scopeProjectRef } from "@hanzodev/client-runtime/environment";
import {
  getAddProjectInitialQuery,
  getCloneDestinationPath,
  getCloneDirectoryPath,
  normalizePastedCloneUrl,
} from "@hanzodev/client-runtime/operations/projects";
import { runAtomCommand } from "@hanzodev/client-runtime/state/runtime";

import { createNewThreadHandler } from "../hooks/useHandleNewThread";
import { waitForClone } from "../lib/addProjectToMachine";
import { newProjectId } from "../lib/utils";
import { appAtomRegistry } from "../rpc/atomRegistry";
import { environmentServerConfigsAtom } from "../state/server";
import { silenceProjectCloneToasts } from "../state/projectClones";
import { sourceControlEnvironment } from "../state/sourceControl";
import type { EmbedState } from "./context";
import { whenPaired } from "./DevProvider";

export interface OpenRepository {
  /** The repository, as a host-qualified address: `github.com/<owner>/<name>`. */
  readonly repo: string;
  /** The branch to start on, or "" for the repository's default. */
  readonly ref: string;
}

/**
 * Puts a repository in the runtime as a project (cloned once; an existing clone is reused),
 * starts a thread on it at `ref`, and answers the thread's `<environmentId>/<threadId>`-style
 * address (`draft/<id>` until its first message). The composer of a new thread takes focus, so
 * `edit` needs nothing more. This is what a host's "Open / Edit in Hanzo" deep link calls.
 */
export async function openRepository(embed: EmbedState, input: OpenRepository): Promise<string> {
  if (!embed.runtime) throw new Error("No machine runtime is connected.");
  const environmentId = await whenPaired(embed.runtime.id);
  const remoteUrl = normalizePastedCloneUrl(input.repo);
  const settings = appAtomRegistry.get(environmentServerConfigsAtom).get(environmentId)?.settings;
  const destinationPath = getCloneDestinationPath(
    getAddProjectInitialQuery(settings?.addProjectBaseDirectory),
    getCloneDirectoryPath(remoteUrl, settings?.ghqCloneLayout === true),
  );
  const requested = newProjectId();
  silenceProjectCloneToasts({ environmentId, projectId: requested });
  const started = await runAtomCommand(
    appAtomRegistry,
    sourceControlEnvironment.startProjectClone,
    {
      environmentId,
      input: {
        projectId: requested,
        title: input.repo.split("/").at(-1) ?? input.repo,
        createdAt: new Date().toISOString(),
        remoteUrl,
        destinationPath,
      },
    },
    { label: "Open repository", reportFailure: false },
  );
  if (started._tag === "Failure") throw new Error(`${input.repo} could not be cloned.`);
  // The server clones for the project that already owns the destination, when there is one.
  const projectId = started.value.projectId;
  const cloned = await waitForClone(environmentId, projectId);
  if (!cloned) throw new Error(`${input.repo} could not be cloned.`);
  const thread = await createNewThreadHandler(embed.router)(
    scopeProjectRef(environmentId, projectId),
    { branch: input.ref || null },
  );
  if (!thread) throw new Error(`A thread on ${input.repo} could not be started.`);
  return `draft/${thread.draftId}`;
}
