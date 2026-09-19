TOOL := cargo run --quiet --manifest-path crates/hanzo-upstream/Cargo.toml --
CARGO := RUST_MIN_STACK=8388608 cargo

.PHONY: all prepare build release install test test-upstream wasm bump v8 fmt clean reset help
.DEFAULT_GOAL := help

all: build

## prepare: apply the owned edits to the pinned upstream checkout
prepare:
	@$(TOOL) prepare

## build: build the dev binary
build: prepare
	@$(CARGO) build --locked -p hanzo-dev --bin dev

## release: build the optimized dev binary
release: prepare
	@$(CARGO) build --locked --release -p hanzo-dev --bin dev

## install: put the release binary on PATH
install: release
	@$(CARGO) install --locked --path crates/hanzo-dev --bin dev

## test: run the Hanzo test suite
# hanzo-upstream is a detached workspace, so the root run cannot see it — and it
# is the code that edits upstream in place, which is the last thing to leave
# untested.
test: prepare
	@$(CARGO) nextest run --locked --no-fail-fast $(ARGS)
	@$(CARGO) nextest run --no-fail-fast --manifest-path crates/hanzo-upstream/Cargo.toml $(ARGS)

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
# The copy is committed, so it is built to be the same bytes wherever it is
# built: no DWARF, which spells out the build directory, and the cargo home
# written as /cargo in the panic locations the dependencies carry. The name
# section stays, so a trap still says which function it stopped in.
CARGO_HOME ?= $(HOME)/.cargo
wasm:
	@$(CARGO) build --locked --release --target wasm32-wasip1 -p dev-ffi \
		--config 'profile.release.debug=false' \
		--config 'profile.release.strip="debuginfo"' \
		--config 'target.wasm32-wasip1.rustflags=["--remap-path-prefix=$(CARGO_HOME)=/cargo"]'
	@install -m 644 target/wasm32-wasip1/release/dev.wasm go/dev.wasm

## test-upstream: run the upstream suite against the pinned submodule
# voice-host wants GStreamer >= 1.28, which no current distribution ships, and
# nothing else in the workspace depends on it.
test-upstream: v8
	@. target/v8-env.sh && $(CARGO) nextest run --no-fail-fast \
		--manifest-path upstream/codex/codex-rs/Cargo.toml \
		--workspace --exclude codex-voice-host $(ARGS)

## bump: move the upstream submodules to newer revisions
bump:
	@$(TOOL) bump $(ARGS)
	@$(TOOL) prepare

## v8: download and verify the V8 build the JavaScript runtime links
v8:
	@$(TOOL) v8

## fmt: format the crates Hanzo owns
fmt:
	@$(CARGO) fmt -p hanzo-dev -p hanzo-config -p hanzo-tui -p dev-protocol -p dev-core -p dev-ffi
	@$(CARGO) fmt --manifest-path crates/hanzo-upstream/Cargo.toml

## clean: discard build output
clean:
	@$(CARGO) clean

## reset: return the upstream checkout to its pinned revision
reset:
	@git submodule foreach --quiet 'git reset --quiet --hard'

help:
	@grep -hE '^## ' $(MAKEFILE_LIST) | sed 's/## /  make /' | sort
