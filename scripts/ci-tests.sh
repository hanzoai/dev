#!/usr/bin/env bash
set -euo pipefail

echo "[ci-tests] Running curated integration tests..."
if [[ "${SKIP_CARGO_TESTS:-0}" != "1" ]]; then
  pushd hanzo-dev >/dev/null

  cargo test -p hanzo-login --tests -q
  cargo test -p hanzo-chatgpt --tests -q
  cargo test -p hanzo-apply-patch --tests -q
  cargo test -p hanzo-execpolicy --tests -q
  cargo test -p mcp-types --tests -q

  popd >/dev/null
fi


echo "[ci-tests] CLI smokes with host binary..."
BIN="${CI_CLI_BIN:-}"
if [[ -z "${BIN}" ]]; then
  if [[ -x ./hanzo-dev/target/dev-fast/dev ]]; then
    BIN=./hanzo-dev/target/dev-fast/dev
  elif [[ -x ./hanzo-dev/target/debug/dev ]]; then
    BIN=./hanzo-dev/target/debug/dev
  fi
fi

if [[ -z "${BIN}" || ! -x "${BIN}" ]]; then
  echo "[ci-tests] CLI binary not found; building debug binary..."
  pushd hanzo-dev >/dev/null
  cargo build --bin dev -q
  popd >/dev/null
  BIN=./hanzo-dev/target/debug/dev
fi

"${BIN}" --version >/dev/null
"${BIN}" completion bash >/dev/null
"${BIN}" doctor >/dev/null || true

echo "[ci-tests] Done."
