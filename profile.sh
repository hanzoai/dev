#!/usr/bin/env bash
# Profile Codex with Apple's Instruments via cargo-instruments.
# Defaults to the same build profile as ./build-fast.sh (dev-fast).
set -euo pipefail

usage() {
  cat <<USAGE
Usage: ./profile.sh [cli|tui] [-- program-args]

Runs Time Profiler against the requested binary using Cargo's custom
profile. By default, builds and profiles with the dev-fast profile
(same as ./build-fast.sh).

Positional:
  cli           Profile the main CLI binary (code) [default]
  tui           Profile the TUI binary (code-tui)

Env vars:
  PROFILE=dev-fast|dev|release|release-prod   Cargo profile (default: dev-fast)
  TEMPLATE="Time Profiler"                    Instruments template name

Examples:
  ./profile.sh                 # build dev-fast and profile CLI
  ./profile.sh tui             # profile TUI
  PROFILE=dev ./profile.sh -- --help   # pass args to program after --
USAGE
}

case "${1:-}" in
  -h|--help) usage; exit 0 ;;
esac

# Resolve repo root reliably relative to this script
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
REPO_ROOT="${SCRIPT_DIR}"

# Which target to run (cli is default)
TARGET="${1:-cli}"
if [[ "$TARGET" != "cli" && "$TARGET" != "tui" ]]; then
  echo "Unknown target '$TARGET'" >&2
  echo "Use 'cli' or 'tui' (or --help)." >&2
  exit 2
fi

# Split off program args after --
PROG_ARGS=()
shift $(( $# > 0 ? 1 : 0 )) || true
if [[ "$#" -gt 0 ]]; then
  if [[ "$1" == "--" ]]; then
    shift
    PROG_ARGS=("$@")
  fi
fi

# Defaults that match build-fast.sh
PROFILE="${PROFILE:-dev-fast}"
TEMPLATE="${TEMPLATE:-Time Profiler}"

# Build first exactly like the normal fast path
"${REPO_ROOT}/build-fast.sh"

# Select package/bin per target
if [[ "$TARGET" == "cli" ]]; then
  PKG="codex-cli"
  BIN="code"
else
  PKG="codex-tui"
  BIN="code-tui"
fi

# Ensure cargo-instruments exists
if ! command -v cargo >/dev/null 2>&1; then
  echo "cargo not found in PATH" >&2
  exit 1
fi
if ! cargo instruments -V >/dev/null 2>&1; then
  echo "cargo-instruments is required (brew install cargo-instruments)" >&2
  exit 1
fi

# Ensure Xcode's xctrace is available (part of full Xcode, not just CLT)
if ! xcrun -f xctrace >/dev/null 2>&1; then
  DEV_DIR="$(xcode-select -p 2>/dev/null || true)"
  echo "xctrace not found via xcrun." >&2
  echo "• Install full Xcode from the App Store (not just Command Line Tools)." >&2
  echo "• If Xcode is already installed, point xcode-select at it:" >&2
  echo "    sudo xcode-select -s /Applications/Xcode.app/Contents/Developer" >&2
  echo "Current developer dir: ${DEV_DIR:-<none>}" >&2
  exit 1
fi

# Check license acceptance (xctrace will fail if license not accepted)
if ! xcrun xctrace list templates >/dev/null 2>&1; then
  echo "xctrace is present but failed to run. If this mentions the Xcode license," >&2
  echo "accept it with:  sudo xcodebuild -license accept" >&2
  exit 1
fi

# Run Instruments with the selected template and profile
cd "${REPO_ROOT}/codex-rs"
echo "Profiling ${PKG}::${BIN} with profile='${PROFILE}', template='${TEMPLATE}'"
set -x
# Optional controls:
#   TIME_LIMIT_MS=<millis> or TIME_LIMIT=<e.g., 10s|15000ms>
#   NO_OPEN=1 to avoid launching Instruments UI
EXTRA_ARGS=()
if [[ -n "${TIME_LIMIT_MS:-}" ]]; then
  EXTRA_ARGS+=(--time-limit "${TIME_LIMIT_MS}")
elif [[ -n "${TIME_LIMIT:-}" ]]; then
  case "${TIME_LIMIT}" in
    *ms) EXTRA_ARGS+=(--time-limit "${TIME_LIMIT%ms}") ;;
    *s)  EXTRA_ARGS+=(--time-limit "$(( ${TIME_LIMIT%s} * 1000 ))") ;;
    *m)  EXTRA_ARGS+=(--time-limit "$(( ${TIME_LIMIT%m} * 60 * 1000 ))") ;;
    *h)  EXTRA_ARGS+=(--time-limit "$(( ${TIME_LIMIT%h} * 60 * 60 * 1000 ))") ;;
    *)   # Treat plain integer as seconds
         EXTRA_ARGS+=(--time-limit "$(( TIME_LIMIT * 1000 ))") ;;
  esac
fi
if [[ "${NO_OPEN:-}" = "1" ]]; then
  EXTRA_ARGS+=(--no-open)
fi
if ((${#PROG_ARGS[@]})); then
  cargo instruments \
    -t "${TEMPLATE}" \
    -p "${PKG}" \
    --bin "${BIN}" \
    --profile "${PROFILE}" \
    "${EXTRA_ARGS[@]}" \
    --open -- \
    "${PROG_ARGS[@]}"
else
  cargo instruments \
    -t "${TEMPLATE}" \
    -p "${PKG}" \
    --bin "${BIN}" \
    --profile "${PROFILE}" \
    "${EXTRA_ARGS[@]}" \
    --open
fi
