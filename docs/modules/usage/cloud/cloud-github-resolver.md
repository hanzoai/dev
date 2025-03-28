# Cloud GitHub Resolver

The GitHub Resolver automates code fixes and provides intelligent assistance for your repositories.

## Setup

The Cloud GitHub Resolver is available automatically when you
[grant Dev Cloud repository access](./dev-cloud.md#adding-repository-access).

## Usage

After granting Dev Cloud repository access, you can use the Cloud GitHub Resolver on the issues and pull requests
on the repository.

### Issues

On your repository, label an issue with `dev`. Dev will:
1. Comment on the issue to let you know it is working on it.
    - You can click on the link to track the progress on Dev Cloud.
2. Open a pull request if it determines that the issue has been successfully resolved.
3. Comment on the issue with a summary of the performed tasks and a link to the pull request.


### Pull Requests

To get Dev to work on pull requests, use `@dev` in top level or inline comments to:
     - Ask questions
     - Request updates
     - Get code explanations

Dev will:
1. Comment on the PR to let you know it is working on it.
2. Perform the task.
