#!/usr/bin/env bash
set -euo pipefail

if [[ ${1:-} == "" ]]; then
  echo "Usage: preview-get.sh <GITHUB_RUN_ID> [owner/repo]" >&2
  echo "Example: preview-get.sh 123456789 just-every/code" >&2
  exit 2
fi

RID="$1"
REPO="${2:-${GITHUB_REPOSITORY:-just-every/code}}"

uname_s=$(uname -s)
uname_m=$(uname -m)
case "${uname_s}-${uname_m}" in
  Linux-x86_64)  T="x86_64-unknown-linux-musl" ;;
  Linux-aarch64|Linux-arm64) T="aarch64-unknown-linux-musl" ;;
  Darwin-x86_64) T="x86_64-apple-darwin" ;;
  Darwin-arm64)  T="aarch64-apple-darwin" ;;
  *) echo "Unsupported platform: ${uname_s}/${uname_m}" >&2; exit 1 ;;
esac

owner="${REPO%%/*}"
repo="${REPO##*/}"

workdir="code-preview-${T}"
rm -rf "$workdir" && mkdir -p "$workdir"
cd "$workdir"

download_with_gh() {
  gh run download "$RID" -R "$owner/$repo" -n "preview-$T" -D . >/dev/null
}

download_with_api() {
  : "${GH_TOKEN:?Set GH_TOKEN to a GitHub token with actions:read}"
  api="https://api.github.com/repos/${owner}/${repo}"
  arts_json=$(curl -fsSL -H "Authorization: Bearer $GH_TOKEN" -H 'Accept: application/vnd.github+json' "$api/actions/runs/$RID/artifacts?per_page=100")
  art_id=$(printf '%s' "$arts_json" | awk -v name="preview-$T" -v RS=',\{|' -v FS=',' '/"name":"[^"\n]*"/ { if ($0 ~ name) { if (match($0, /"id":[0-9]+/)) { id=substr($0,RSTART+5,RLENGTH-5); print id; exit } } }')
  if [[ -z "${art_id:-}" ]]; then
    echo "Could not find artifact preview-$T for run $RID" >&2; exit 3
  fi
  curl -fSL -H "Authorization: Bearer $GH_TOKEN" "$api/actions/artifacts/$art_id/zip" -o artifact.zip
  unzip -q artifact.zip
}

if command -v gh >/dev/null 2>&1; then
  download_with_gh || download_with_api
else
  download_with_api
fi

# Prefer .tar.gz on Unix (no zstd dependency)
if ls code-*.tar.gz >/dev/null 2>&1; then
  tgz=$(ls code-*.tar.gz | head -n1)
  bname=$(tar -tzf "$tgz" | head -n1 | xargs basename)
  tar -xzf "$tgz"
  chmod +x "$bname"
  echo "Ready: $(pwd)/$bname"
  echo "Running: ./$bname --help"
  ./"$bname" --help || true
  exit 0
fi

if ls code-*.zst >/dev/null 2>&1; then
  zst=$(ls code-*.zst | head -n1)
  if ! command -v zstd >/dev/null 2>&1; then
    echo "Found Zstandard archive but 'zstd' is not installed. Install zstd or re-run with gh to auto-extract." >&2
    exit 4
  fi
  zstd -d "$zst" -o code
  chmod +x code
  echo "Ready: $(pwd)/code"
  echo "Running: ./code --help"
  ./code --help || true
  exit 0
fi

echo "No recognized artifact content found." >&2
exit 5

