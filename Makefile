CARGO_HOME ?= $(HOME)/.cargo
export PATH := $(CARGO_HOME)/bin:$(PATH)
export NEXTEST_HIDE_PROGRESS_BAR := true

# The wasm2go compiler: build it from hanzoai/wasm2go (go build ./cmd/wasm2go).
WASM2GO ?= wasm2go

.PHONY: test wasm wasm-check wasm2go frames-check fmt help
.DEFAULT_GOAL := test

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
