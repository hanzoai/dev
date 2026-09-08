#!/usr/bin/env python3

import json
import subprocess
import sys
import argparse
from pathlib import Path


def main() -> int:
    parser = argparse.ArgumentParser(
        description="""Stage a release for the npm module.

Run this after the GitHub Release has been created and use
`--release-version` to specify the version to release.

Optionally pass `--tmp` to control the temporary staging directory used by
release CI to build the npm package.
"""
    )
    parser.add_argument(
        "--release-version", required=True, help="Version to release, e.g., 0.3.0"
    )
    parser.add_argument("--tmp", help="Optional path to stage the npm package")
    args = parser.parse_args()
    version = args.release_version

    gh_run = subprocess.run(
        [
            "gh",
            "run",
            "list",
            "--branch",
            f"rust-v{version}",
            "--json",
            "workflowName,url,headSha",
            "--jq",
            'first(.[] | select(.workflowName == "rust-release"))',
        ],
        stdout=subprocess.PIPE,
        check=True,
    )
    gh_run.check_returncode()
    workflow = json.loads(gh_run.stdout)
    sha = workflow["headSha"]

    print(f"should `git checkout {sha}`")

    # The release pipeline is handled by GitHub Actions (release.yml). This
    # helper now only locates the rust-release workflow run for the given
    # version and prints the checkout hint. Staging and npm publish are driven
    # by CI.
    print("Release staging is handled by release.yml; no local staging step.")

    return 0


if __name__ == "__main__":
    sys.exit(main())
