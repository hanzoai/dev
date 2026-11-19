import * as Option from "effect/Option";

export type JoinPath = (first: string, ...segments: string[]) => string;

function normalizeConfiguredBaseDir(hanzoHome: Option.Option<string>): Option.Option<string> {
  if (Option.isNone(hanzoHome)) {
    return Option.none();
  }
  const trimmed = hanzoHome.value.trim();
  return trimmed.length > 0 ? Option.some(trimmed) : Option.none();
}

export function resolveDesktopBaseDir(input: {
  readonly homeDirectory: string;
  readonly joinPath: JoinPath;
  readonly hanzoHome: Option.Option<string>;
}): string {
  return Option.getOrElse(normalizeConfiguredBaseDir(input.hanzoHome), () =>
    input.joinPath(input.homeDirectory, ".hanzo-dev"),
  );
}

export function resolveDesktopStateDir(input: {
  readonly baseDir: string;
  readonly isDevelopment: boolean;
  readonly joinPath: JoinPath;
  readonly hanzoHome: Option.Option<string>;
}): string {
  const useDevSubdir =
    input.isDevelopment && Option.isNone(normalizeConfiguredBaseDir(input.hanzoHome));
  return input.joinPath(input.baseDir, useDevSubdir ? "dev" : "userdata");
}
