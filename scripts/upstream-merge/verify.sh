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
status_guards="ok"

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

#
# STEP 3: Static guards for fork-specific functionality
# - Ensure browser/agent tools are still registered (not just handlers present)
# - Ensure version handling remains via codex_version in default_client
# - Ensure web_fetch and web_search tool presence is consistent with fork policy
{
  echo "[verify] STEP 3: static guards (tools + UA/version)"
}
guards_log=.github/auto/VERIFY_guards.log
: > "$guards_log"

# Guard A: openai_tools must include browser and agent tool registration (schemas)
if ! rg -n "name:\s*\"browser_open\"" codex-rs/core/src/openai_tools.rs >/dev/null 2>&1; then
  echo "[guards] missing browser_open tool schema in openai_tools.rs" | tee -a "$guards_log"
  status_guards="fail"
fi
if ! rg -n "name:\s*\"agent_run\"" codex-rs/core/src/openai_tools.rs >/dev/null 2>&1; then
  echo "[guards] missing agent_run tool schema in openai_tools.rs" | tee -a "$guards_log"
  status_guards="fail"
fi
# Guard B: get_openai_tools should push web_fetch or web_search per fork policy
if ! rg -n "web_fetch\"|WebSearch" codex-rs/core/src/openai_tools.rs >/dev/null 2>&1; then
  echo "[guards] neither web_fetch nor web_search tools found in openai_tools.rs" | tee -a "$guards_log"
  status_guards="fail"
fi
# Guard C: default_client should reference codex_version::version for UA
if ! rg -n "codex_version::version\(\)" codex-rs/core/src/default_client.rs >/dev/null 2>&1; then
  echo "[guards] codex_version::version() not referenced in core/default_client.rs" | tee -a "$guards_log"
  status_guards="fail"
fi

# Summarize guards
echo "guards=${status_guards}" >> .github/auto/VERIFY_guards.log

rc=0
if [[ "$status_build" != ok || "$status_api" != ok || "$status_guards" != ok ]]; then
  rc=1
fi

cat > .github/auto/VERIFY.json <<JSON
{
  "build_fast": "$status_build",
  "api_check": "$status_api",
  "guards": "$status_guards"
}
JSON

echo "[verify] SUMMARY: build_fast=$status_build api_check=$status_api"
exit $rc
