#!/usr/bin/env bash
set -euo pipefail

echo "[ci-tests] Running curated integration tests..."
pushd code-rs >/dev/null

cargo test -p code-login --tests -q
cargo test -p code-chatgpt --tests -q
cargo test -p code-apply-patch --tests -q
cargo test -p code-execpolicy --tests -q
cargo test -p mcp-types --tests -q

popd >/dev/null

echo "[ci-tests] CLI smokes with host binary..."
BIN=./code-rs/target/dev-fast/code
"${BIN}" --version >/dev/null
"${BIN}" completion bash >/dev/null
"${BIN}" doctor >/dev/null || true

echo "[ci-tests] Done."
