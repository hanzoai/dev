CARGO_HOME ?= $(HOME)/.cargo
export PATH := $(CARGO_HOME)/bin:$(PATH)
export NEXTEST_HIDE_PROGRESS_BAR := true

CARGO := RUST_MIN_STACK=8388608 cargo

.PHONY: all build release install test test-upstream bump v8 fmt clean help
.DEFAULT_GOAL := build

all: build

## build: build the dev binary
build:
	@$(CARGO) build --locked -p hanzo-dev --bin dev

## release: build the optimized dev binary
release:
	@$(CARGO) build --locked --release -p hanzo-dev --bin dev

## install: put the release binary on PATH
install: release
	@$(CARGO) install --locked --path crates/hanzo-dev --bin dev

## test: run the Hanzo test suite
test:
	@$(CARGO) nextest run --locked --no-fail-fast $(ARGS)

## test-upstream: run hanzoai/codex's own suite against the pinned checkout
# voice-host wants GStreamer >= 1.28, which no current distribution ships, and
# nothing else in the workspace depends on it. The fork resolves on its own, so
# its lock holds.
test-upstream: v8
	@. target/v8-env.sh && $(CARGO) nextest run --locked --no-fail-fast \
		--manifest-path upstream/codex/codex-rs/Cargo.toml \
		--workspace --exclude codex-voice-host $(ARGS)

## bump: say how to move upstream/codex forward (it runs nothing)
bump:
	@echo 'upstream/codex is hanzoai/codex: its main is openai/codex plus our commits.'
	@echo '  git -C upstream/codex remote add upstream https://github.com/openai/codex.git   # once'
	@echo '  git -C upstream/codex switch main && git -C upstream/codex pull --ff-only origin main'
	@echo '  git -C upstream/codex fetch upstream && git -C upstream/codex merge upstream/main'
	@echo '  make build test        # then push main to hanzoai/codex and commit the gitlink here'

## v8: download and verify the V8 build the upstream suite links
v8:
	@scripts/v8.sh

## fmt: format the crates Hanzo owns
fmt:
	@$(CARGO) fmt -p hanzo-dev -p hanzo-config -p hanzo-tui -p hanzo-kai

## clean: discard build output
clean:
	@$(CARGO) clean

help:
	@grep -hE '^## ' $(MAKEFILE_LIST) | sed 's/## /  make /' | sort
