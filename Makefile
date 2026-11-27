CARGO_HOME ?= $(HOME)/.cargo
export PATH := $(CARGO_HOME)/bin:$(PATH)
export NEXTEST_HIDE_PROGRESS_BAR := true

# Canonical Hanzo CLI & wasm2go compiler (`hanzoai/wasm2go`).
HANZO ?= hanzo
WASM2GO ?= wasm2go

.PHONY: up down start stop start-vm stop-vm dev dev-app dev-desktop debug logs clean build build-desktop test wasm wasm-check wasm2go frames-check fmt help
.DEFAULT_GOAL := test

# ---------------------------------------------------------------------------
# Canonical Hanzo Lifecycle Targets (§6)
#   make up / down           -> hanzo up / hanzo down       (environment)
#   make start / stop        -> start / stop this repo app  (workload only; never kills shared runtime)
#   make start-vm / stop-vm  -> hanzo start / hanzo stop    (local k3s runtime)
#   make dev / debug         -> hanzo dev / hanzo dev --debug
#   make logs                -> project-scoped logs (hanzo app logs dev)
#   make clean               -> build artifacts only (never touches ~/.hanzo)
# ---------------------------------------------------------------------------

## up: start or reconcile the local Hanzo environment (hanzo up)
up:
	@$(HANZO) up

## down: stop environment workloads while keeping warm runtime (hanzo down)
down:
	@$(HANZO) down

## start: start this repository's Hanzo Dev workload without restarting unrelated services
start:
	@$(HANZO) app start dev

## stop: stop ONLY this repository's Hanzo Dev workload (never terminates shared runtime)
stop:
	@$(HANZO) app stop dev

## start-vm: start the local Hanzo runtime (microVM / native k3s + containerd)
start-vm:
	@$(HANZO) start

## stop-vm: gracefully shut down the local Hanzo runtime
stop-vm:
	@$(HANZO) stop

## dev: run canonical Hanzo Dev interactive session (hanzo dev)
dev:
	@$(HANZO) dev

## dev-app: run local monorepo dev runner (web + server hot reload)
dev-app:
	@pnpm dev

## dev-desktop: run local Tauri desktop shell in dev mode
dev-desktop:
	@pnpm dev:desktop

## debug: run canonical Hanzo Dev with verbose debug inspector (hanzo dev --debug)
debug:
	@$(HANZO) dev --debug

## logs: stream project-scoped logs for Hanzo Dev (hanzo app logs dev)
logs:
	@$(HANZO) app logs dev

## build: build standalone web + server bundles
build:
	@pnpm build

## build-desktop: build standalone web + server + Tauri desktop release binary
build-desktop:
	@pnpm build:desktop

## clean: remove local build artifacts only (never touches ~/.hanzo or committed go/dev.wasm)
clean:
	@rm -rf apps/web/dist apps/server/dist apps/server/dist-exe \
		apps/desktop/src-tauri/resources apps/desktop/src-tauri/target \
		native/resource-monitor/target go/devwasm target/frames.jsonl

## test: run the Rust tests and the Go host's tests on the committed module
test:
	@cargo nextest run --locked --no-fail-fast $(ARGS)
	@cd go && GOWORK=off go test -race ./...

## frames-check: decode every frame the chat-mode adapter writes against packages/contracts
frames-check:
	@cd go && GOWORK=off go test ./frame -run TestChat -corpus $(CURDIR)/target/frames.jsonl
	@node go/frame/e2e/validate.mjs $(CURDIR) target/frames.jsonl

## wasm: build the reasoning half for wasm32-wasip1
# The loop as a wasm module: target/wasm32-wasip1/release/dev.wasm, copied to
# go/dev.wasm, which the Go host embeds.
#
# protocol and core carry no effects, so they run wherever a wasm host runs — and
# so does ffi, which is what a host actually calls. Its C ABI is the module's
# export list unchanged; only the memory differs, and dev_alloc lends the host a
# buffer inside it. What the module may IMPORT is the proof that the core touches
# nothing: environ_get, environ_sizes_get, fd_write and proc_exit, and no file,
# socket, clock or random source. A change that grows that list has given the
# loop an effect of its own.
#
# The copy is committed, so no path of the machine that built it goes in: no
# DWARF, which spells out the build directory, and the cargo home written as
# /cargo in the panic locations the dependencies carry. Two builds on one kind
# of host are then the same bytes, whatever their directories. Two kinds of
# host are not: cargo hashes the host into every crate that has a build script
# or uses a proc macro, and the module differs with those hashes. macOS arm64,
# Linux arm64 and Linux x86_64 each build a different one.
# The name section stays, so a trap still says which function it stopped in.
#
# The remap goes in CARGO_ENCODED_RUSTFLAGS because cargo takes that over every
# other source of rustflags. The target directory is named too: CARGO_TARGET_DIR
# would send the build elsewhere, and the copy would be whatever an older build
# left here.
#
# CI builds on another kind of host, so it cannot rebuild the committed bytes;
# it checks what they were built from instead. Beside the copy goes
# go/dev.wasm.sum, the sha256 of every tracked file the build reads and of the
# copy itself. wasm-check hashes the tree again and fails on any difference.
WASM_SUM = git ls-files -z -- Makefile Cargo.toml Cargo.lock rust-toolchain.toml \
    crates/protocol crates/core crates/orchestrator crates/ffi go/dev.wasm | xargs -0 sha256sum
wasm:
	@CARGO_ENCODED_RUSTFLAGS='--remap-path-prefix=$(CARGO_HOME)=/cargo' \
		cargo build --locked --release --target wasm32-wasip1 --target-dir target -p dev-ffi \
		--config 'profile.release.debug=false' \
		--config 'profile.release.strip="debuginfo"'
	@install -m 644 target/wasm32-wasip1/release/dev.wasm go/dev.wasm
	@$(WASM_SUM) > go/dev.wasm.sum

## wasm-check: fail unless go/dev.wasm is make wasm of the tree beside it
wasm-check:
	@$(WASM_SUM) | diff go/dev.wasm.sum - || { \
		echo 'go/dev.wasm was not built from this tree: run make wasm, and commit go/dev.wasm and go/dev.wasm.sum' >&2; \
		exit 1; }

## wasm2go: compile go/dev.wasm to the pure-Go package go/devwasm
# Cloud links the loop as plain Go, with no wasm runtime. The package is about
# 7 MB of source, so it is generated where it is built and never committed.
wasm2go:
	@rm -rf go/devwasm && mkdir -p go/devwasm
	@$(WASM2GO) -i go/dev.wasm -pkg devwasm -import github.com/hanzoai/dev/go/devwasm -pure -o go/devwasm/devwasm.go
	@cd go && GOWORK=off go build ./devwasm/

## fmt: format the crates
fmt:
	@cargo fmt --all

help:
	@grep -hE '^## ' $(MAKEFILE_LIST) | sed 's/## /  make /' | sort
