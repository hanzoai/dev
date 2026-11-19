import {
  HANZO_PROJECT_FILE_NAME,
  type EnvironmentId,
  type HanzoProjectFile,
  type HanzoProjectFileScript,
} from "@hanzodev/contracts";
import { parseHanzoProjectFile } from "@hanzodev/shared/hanzoProjectFile";

import { useProjectFileQuery } from "~/components/files/projectFilesQueryState";

const NO_SCRIPTS: ReadonlyArray<HanzoProjectFileScript> = [];

export interface HanzoProjectFileState {
  status: "loading" | "missing" | "invalid" | "valid";
  file: HanzoProjectFile | null;
  scripts: ReadonlyArray<HanzoProjectFileScript>;
}

export function useHanzoProjectFileState(
  environmentId: EnvironmentId,
  cwd: string | null,
): HanzoProjectFileState {
  const query = useProjectFileQuery(
    environmentId,
    cwd ?? "",
    HANZO_PROJECT_FILE_NAME,
    cwd !== null,
  );
  const contents = query.data && !query.data.truncated ? query.data.contents : null;
  const isPending = query.isPending;
  if (contents === null) {
    return {
      status: isPending ? "loading" : "missing",
      file: null,
      scripts: NO_SCRIPTS,
    } as const;
  }
  const file = parseHanzoProjectFile(contents);
  if (file === null) {
    return { status: "invalid", file: null, scripts: NO_SCRIPTS } as const;
  }
  return { status: "valid", file, scripts: file.scripts ?? NO_SCRIPTS } as const;
}

export function useHanzoProjectFileScripts(
  environmentId: EnvironmentId,
  cwd: string | null,
): ReadonlyArray<HanzoProjectFileScript> {
  return useHanzoProjectFileState(environmentId, cwd).scripts;
}
