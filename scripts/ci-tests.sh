#!/usr/bin/env bash
set -euo pipefail

# Run a small, curated set of fast integration tests + CLI smokes.
# Assumes `./build-fast.sh` was run already so the host binary exists.

echo "[ci-tests] Running curated integration tests..."
pushd codex-rs >/dev/null

# Keep quiet output; fail fast on first error.
cargo test -p codex-login --test all   -q
cargo test -p codex-chatgpt --test all -q
cargo test -p codex-apply-patch --test all -q
cargo test -p codex-execpolicy --tests -q
cargo test -p mcp-types --tests -q

popd >/dev/null

echo "[ci-tests] CLI smokes with host binary..."
BIN=./codex-rs/target/dev-fast/code
"${BIN}" --version >/dev/null
"${BIN}" completion --shell bash >/dev/null
"${BIN}" doctor >/dev/null || true

echo "[ci-tests] Done."

