#!/usr/bin/env bash
# Poll a GitHub Actions run until it completes, printing status updates.
#
# Usage examples:
#   scripts/wait-for-gh-run.sh --run 17901972778
#   scripts/wait-for-gh-run.sh --workflow Release --branch main
#
# Dependencies: gh (GitHub CLI), jq.

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: wait-for-gh-run.sh [OPTIONS]

Options:
  -r, --run ID           Run ID to monitor.
  -w, --workflow NAME    Workflow name or filename to pick the latest run.
  -b, --branch BRANCH    Branch to filter when selecting a run (default: main).
  -i, --interval SECONDS Polling interval in seconds (default: 8).
  -h, --help             Show this help message.

Either --run or --workflow must be supplied.
EOF
}

require_binary() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "error: '$1' not found in PATH" >&2
    exit 1
  fi
}

RUN_ID=""
WORKFLOW=""
BRANCH="main"
INTERVAL="8"

while [[ $# -gt 0 ]]; do
  case "$1" in
    -r|--run)
      RUN_ID="${2:-}"
      shift 2
      ;;
    -w|--workflow)
      WORKFLOW="${2:-}"
      shift 2
      ;;
    -b|--branch)
      BRANCH="${2:-}"
      shift 2
      ;;
    -i|--interval)
      INTERVAL="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown option '$1'" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ -z "$RUN_ID" && -z "$WORKFLOW" ]]; then
  echo "error: either --run or --workflow must be specified" >&2
  usage >&2
  exit 1
fi

require_binary gh
require_binary jq

select_latest_run() {
  local workflow="$1"
  local branch="$2"
  local json
  if ! json=$(gh run list --workflow "$workflow" --branch "$branch" --limit 1 --json databaseId,status,conclusion,displayTitle,workflowName,headBranch 2>/dev/null); then
    echo "error: failed to list runs for workflow '$workflow'" >&2
    exit 1
  fi

  if [[ $(jq 'length' <<<"$json") -eq 0 ]]; then
    echo "error: no runs found for workflow '$workflow' on branch '$branch'" >&2
    exit 1
  fi

  jq -r '.[0].databaseId' <<<"$json"
}

if [[ -z "$RUN_ID" ]]; then
  RUN_ID=$(select_latest_run "$WORKFLOW" "$BRANCH")
fi

if [[ -z "$RUN_ID" ]]; then
  echo "error: unable to determine run ID" >&2
  exit 1
fi

echo "Waiting for GitHub Actions run $RUN_ID..." >&2

last_status=""
last_jobs_snapshot=""

while true; do
  json=""
  if ! json=$(gh run view "$RUN_ID" --json status,conclusion,displayTitle,workflowName,headBranch,url,startedAt,updatedAt,jobs 2>/dev/null); then
    echo "$(date '+%Y-%m-%d %H:%M:%S') failed to fetch run info; retrying in $INTERVAL s" >&2
    sleep "$INTERVAL"
    continue
  fi

  status=$(jq -r '.status' <<<"$json")
  conclusion=$(jq -r '.conclusion // ""' <<<"$json")
  workflow_name=$(jq -r '.workflowName // "(unknown workflow)"' <<<"$json")
  display_title=$(jq -r '.displayTitle // "(no title)"' <<<"$json")
  branch_name=$(jq -r '.headBranch // "(unknown branch)"' <<<"$json")
  run_url=$(jq -r '.url // ""' <<<"$json")

  if [[ "$status" != "$last_status" ]]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') [$workflow_name] $display_title on branch '$branch_name' -> status: $status${conclusion:+, conclusion: $conclusion}" >&2
    [[ -n "$run_url" ]] && echo "  $run_url" >&2
    last_status="$status"
  fi

  jobs_snapshot=$(jq -r '.jobs[]? | "\(.name // "(no name)")|\(.status)//\(.conclusion // "")"' <<<"$json" | sort)

  if [[ "$jobs_snapshot" != "$last_jobs_snapshot" ]]; then
    if [[ -n "$jobs_snapshot" ]]; then
      echo "$(date '+%Y-%m-%d %H:%M:%S') job summary:" >&2
      jq -r '.jobs[]? | "  - " + (.name // "(no name)") + ": " + (.status // "?") + (if .status == "completed" and .conclusion != null then " (" + .conclusion + ")" else "" end)' <<<"$json" >&2
    fi
    last_jobs_snapshot="$jobs_snapshot"
  fi

  if [[ "$status" == "completed" ]]; then
    if [[ "$conclusion" == "success" ]]; then
      echo "Run $RUN_ID succeeded." >&2
      exit 0
    else
      echo "Run $RUN_ID finished with conclusion '$conclusion'." >&2
      exit 1
    fi
  fi

  sleep "$INTERVAL"
done
