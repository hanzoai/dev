import type { PullRequestChangeBreakdown } from "@hanzodev/contracts";
import { summarizeChangedFiles } from "@hanzodev/shared/changedFileKind";
import type { ProviderFileStats } from "@hanzodev/source-control-core/server/PullRequestProvider";

export function toChangeBreakdown(stats: ProviderFileStats): PullRequestChangeBreakdown {
  if (stats.truncated) return null;
  return summarizeChangedFiles(stats.files);
}
