import * as AzureDevOps from "@hanzodev/source-control-azure-devops/shared/definition";
import * as Bitbucket from "@hanzodev/source-control-bitbucket/shared/definition";
import { makeSourceControlDefinitionRegistry } from "@hanzodev/source-control-core/shared/definition";
import * as CursorOrigin from "@hanzodev/source-control-cursor-origin/shared/definition";
import * as Forgejo from "@hanzodev/source-control-forgejo/shared/definition";
import * as GitCafe from "@hanzodev/source-control-gitcafe/shared/definition";
import * as GitHub from "@hanzodev/source-control-github/shared/definition";
import * as GitLab from "@hanzodev/source-control-gitlab/shared/definition";

export { canonicalizePullRequestUrl as canonicalizeGitCafePullRequestUrl } from "@hanzodev/source-control-gitcafe/shared/definition";

/**
 * GitHub comes first to preserve the client default for repositories with an unknown host.
 * GitCafe and Cursor Origin claim URLs first because GitHub and Forgejo accept the same routes on
 * any hostname.
 * Remote recognition needs its own order because host labels can overlap (`gitea.github.example`).
 */
export const builtInSourceControlDefinitions = makeSourceControlDefinitionRegistry(
  [
    GitHub.definition,
    CursorOrigin.definition,
    GitLab.definition,
    GitCafe.definition,
    Forgejo.definition,
    Bitbucket.definition,
    AzureDevOps.definition,
  ],
  {
    claimChangeRequestUrlsFirst: [
      GitCafe.definition.kind,
      CursorOrigin.definition.kind,
      AzureDevOps.definition.kind,
    ],
    recognizeRemotesIn: [
      GitCafe.definition.kind,
      CursorOrigin.definition.kind,
      Forgejo.definition.kind,
      GitHub.definition.kind,
      GitLab.definition.kind,
      AzureDevOps.definition.kind,
      Bitbucket.definition.kind,
    ],
  },
);
