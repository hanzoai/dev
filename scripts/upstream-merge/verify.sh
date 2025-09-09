#!/usr/bin/env bash
set -euo pipefail

# Unified, fast verification for upstream-merge runs.
# - Runs build-fast.sh (treat warnings as failures via repo policy)
# - Compiles API-surface tests for codex-core (no test execution)
# - Emits a JSON summary to .github/auto/VERIFY.json

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd)"
cd "$ROOT_DIR"

mkdir -p .github/auto

status_build="ok"
status_api="ok"

{
  echo "[verify] START $(date -u +%FT%TZ)"
  echo "[verify] repo: $ROOT_DIR"
  echo "[verify] STEP 1: build-fast.sh"
}

# Use the same environment as the job (including sccache) for consistency
export KEEP_ENV=1
if ! ./build-fast.sh 2>&1 | tee .github/auto/VERIFY_build-fast.log; then
  status_build="fail"
fi

{
  echo "[verify] STEP 2: cargo check (core tests compile)"
}
# Respect pre-set CARGO_HOME/TARGET_DIR to share caches across steps
export CARGO_HOME="${CARGO_HOME:-$ROOT_DIR/.cargo-home}"
export CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-$ROOT_DIR/codex-rs/target}"
mkdir -p "$CARGO_HOME" "$CARGO_TARGET_DIR" >/dev/null 2>&1 || true
if ! (cd codex-rs && cargo check -p codex-core --test api_surface --quiet) 2>&1 | tee .github/auto/VERIFY_api-check.log; then
  status_api="fail"
fi

rc=0
if [[ "$status_build" != ok || "$status_api" != ok ]]; then
  rc=1
fi

cat > .github/auto/VERIFY.json <<JSON
{
  "build_fast": "$status_build",
  "api_check": "$status_api"
}
JSON

echo "[verify] SUMMARY: build_fast=$status_build api_check=$status_api"
exit $rc
