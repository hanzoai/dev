#!/usr/bin/env bash
set -euo pipefail

# Single build entrypoint: delegate to Makefile
# Keeps ./build-fast.sh for tooling compatibility while enforcing one way to build.

if command -v make >/dev/null 2>&1; then
  echo "Delegating build to Makefile (make build-rust)..."
  exec make -s build-rust
else
  echo "make is required. Please install make and retry." >&2
  exit 1
fi

