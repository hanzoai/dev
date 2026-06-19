#!/usr/bin/env bash
set -euo pipefail

echo "[ci-tests] Running curated integration tests..."
if [[ "${SKIP_CARGO_TESTS:-0}" != "1" ]]; then
  pushd codex-rs >/dev/null

  cargo test -p codex-login --tests -q
  cargo test -p codex-chatgpt --tests -q
  cargo test -p codex-apply-patch --tests -q
  cargo test -p codex-execpolicy --tests -q

  popd >/dev/null
fi


echo "[ci-tests] CLI smokes with host binary..."
BIN="${CI_CLI_BIN:-}"
if [[ -z "${BIN}" ]]; then
  if [[ -x ./codex-rs/target/dev-fast/dev ]]; then
    BIN=./codex-rs/target/dev-fast/dev
  elif [[ -x ./codex-rs/target/debug/dev ]]; then
    BIN=./codex-rs/target/debug/dev
  fi
fi

if [[ -z "${BIN}" || ! -x "${BIN}" ]]; then
  echo "[ci-tests] CLI binary not found; building debug binary..."
  pushd codex-rs >/dev/null
  cargo build --bin dev -q
  popd >/dev/null
  BIN=./codex-rs/target/debug/dev
fi

"${BIN}" --version >/dev/null
"${BIN}" completion bash >/dev/null
"${BIN}" doctor >/dev/null || true

echo "[ci-tests] Done."
