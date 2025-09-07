#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<USAGE
PR Cleanup (local)

Removes cache directories and stray .gitignore files from in-repo PR branches and pushes a cleanup commit.

Usage: scripts/pr-cleanup.sh [--dry-run] [--patterns PATTERNS]

Options:
  --dry-run              Report what would change, do not push
  --patterns PATTERNS    Comma-separated list (default: .cargo-home/**,.cargo-cache/**,**/.gitignore)

Environment:
  GITHUB_TOKEN           Token with repo push permissions (uses gh if logged in)
USAGE
}

DRY_RUN=false
PATTERNS=".cargo-home/**,.cargo-cache/**,**/.gitignore"
while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run) DRY_RUN=true ; shift ;;
    --patterns) PATTERNS="$2" ; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown arg: $1" >&2; usage; exit 1 ;;
  esac
done

if ! command -v gh >/dev/null 2>&1; then
  echo "gh CLI is required (https://cli.github.com)." >&2
  exit 1
fi

# Ensure we are at repo root
REPO_ROOT=$(git rev-parse --show-toplevel)
cd "$REPO_ROOT"

OWNER=${OWNER:-$(git config --get remote.origin.url | sed -n 's#.*github.com[:/]\([^/]*\)/.*#\1#p')}
REPO=${REPO:-$(git config --get remote.origin.url | sed -n 's#.*github.com[:/][^/]*/\([^/.]*\).*#\1#p')}

echo "Repo: $OWNER/$REPO"

PR_JSON=$(gh api repos/$OWNER/$REPO/pulls?state=open\&per_page=100)
echo "Open PRs: $(echo "$PR_JSON" | jq 'length')"

mapfile -t PRS < <(echo "$PR_JSON" | jq -cr '.[] | {number, head_branch: .head.ref, head_repo: .head.repo.full_name, author: .user.login} | @base64')

workdir=".pr-clean"
mkdir -p "$workdir"

changes=()
skipped=()

for enc in "${PRS[@]}"; do
  pr=$(printf '%s' "$enc" | base64 -d)
  num=$(jq -r '.number' <<<"$pr")
  head_branch=$(jq -r '.head_branch' <<<"$pr")
  head_repo=$(jq -r '.head_repo' <<<"$pr")
  author=$(jq -r '.author' <<<"$pr")
  echo "-- PR #$num ($author) head=$head_branch repo=$head_repo"
  if [ "$head_repo" != "$OWNER/$REPO" ]; then
    echo "   skip: fork PR"
    skipped+=("#$num fork")
    continue
  fi

  files=$(gh api repos/$OWNER/$REPO/pulls/$num/files?per_page=100)
  needs=$(jq -r 'any(.[]; (.filename|startswith(".cargo-home/") or startswith(".cargo-cache/") or endswith(".gitignore")))' <<<"$files")
  if [ "$needs" != "true" ]; then
    echo "   no cleanup needed"
    continue
  fi

  wt="$workdir/$head_branch"
  rm -rf "$wt"
  git worktree add -f "$wt" "origin/$head_branch"
  pushd "$wt" >/dev/null

  IFS=',' read -r -a pats <<<"$PATTERNS"
  for p in "${pats[@]}"; do
    case "$p" in
      **/.gitignore)
        # remove newly added .gitignore files anywhere except root
        mapfile -t rmfiles < <(git diff --name-only --diff-filter=A | grep -E '\.gitignore$' | grep -v '^\.gitignore$' || true)
        ;;
      .cargo-home/**)
        mapfile -t rmfiles < <(git ls-files -z | tr '\0' '\n' | grep '^.cargo-home/' || true)
        ;;
      .cargo-cache/**)
        mapfile -t rmfiles < <(git ls-files -z | tr '\0' '\n' | grep '^.cargo-cache/' || true)
        ;;
      *) rmfiles=() ;;
    esac
    if [ ${#rmfiles[@]} -gt 0 ]; then
      git rm -r --cached --ignore-unmatch -- "${rmfiles[@]}" || true
    fi
  done

  # Ensure root .gitignore has entries
  touch .gitignore
  grep -q '^/.cargo-home/' .gitignore || echo '/.cargo-home/' >> .gitignore
  grep -q '^/.cargo-cache/' .gitignore || echo '/.cargo-cache/' >> .gitignore
  git add .gitignore || true

  if git diff --cached --quiet; then
    echo "   nothing staged after filtering"
    popd >/dev/null
    git worktree remove -f "$wt"
    continue
  fi

  if [ "$DRY_RUN" = true ]; then
    echo "   DRY-RUN: would commit cleanup on $head_branch"
  else
    git -c user.email="github-actions[bot]@users.noreply.github.com" -c user.name="github-actions[bot]" \
      commit -m "chore(cleanup): remove local caches and stray .gitignore; add ignores"
    git push origin "$head_branch"
    changes+=("#$num $head_branch cleaned")
  fi

  popd >/dev/null
  git worktree remove -f "$wt"
done

echo "--- Summary ---"
printf 'changed: %s\n' "${changes[@]:-none}"
printf 'skipped: %s\n' "${skipped[@]:-none}"

