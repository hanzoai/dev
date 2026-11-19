import { HANZO_PROJECT_FILE_NAME, type EnvironmentId, type HanzoProjectFile } from "@hanzodev/contracts";
import { parseHanzoProjectFile } from "@hanzodev/shared/hanzoProjectFile";
import { executeAtomQuery } from "@hanzodev/client-runtime/state/runtime";

import {
  getProjectFileQueryAtom,
  resolveProjectFileQueryData,
} from "~/components/files/projectFilesQueryState";
import { appAtomRegistry } from "~/rpc/atomRegistry";

export async function readHanzoProjectFile(
  environmentId: EnvironmentId,
  workspaceRoot: string,
): Promise<HanzoProjectFile | null> {
  const result = await executeAtomQuery(
    appAtomRegistry,
    getProjectFileQueryAtom(environmentId, workspaceRoot, HANZO_PROJECT_FILE_NAME),
    { reportDefect: false, reportFailure: false },
  );
  const data = resolveProjectFileQueryData(
    environmentId,
    workspaceRoot,
    HANZO_PROJECT_FILE_NAME,
    result._tag === "Success" ? result.value : null,
  );
  if (data === null || data.exists === false || data.truncated) return null;
  return parseHanzoProjectFile(data.contents);
}
