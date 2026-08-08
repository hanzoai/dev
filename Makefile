# Hanzo Dev.
#
# Builds code-rs/, the workspace that ships. vendor/codex/ is a vendored mirror of
# openai/codex and is deliberately not built here — only its models.json is
# compiled in, via code-rs/code-version.
#
# Every target delegates to build-fast.sh so there is one build and one cache.

PREFIX  ?= $(HOME)/.local
PROFILE ?= dev-fast

bin   := code-rs/bin/dev
cargo := $(HOME)/.cargo/bin/cargo

export PATH            := $(HOME)/.cargo/bin:$(PATH)
export BUILD_FAST_BINS := dev
export RUST_MIN_STACK  := 8388608

.PHONY: all build install uninstall run test clippy clean help

all: build

## build: compile dev — the required check, warnings included
build:
	@PROFILE=$(PROFILE) ./build-fast.sh

## install: put dev on PATH, with code as an alias to the same program
install: build
	@install -Dm755 $(bin) $(PREFIX)/bin/dev
	@ln -sf dev $(PREFIX)/bin/code
	@$(PREFIX)/bin/dev --version

## uninstall: take dev and the code alias off PATH
uninstall:
	@rm -f $(PREFIX)/bin/dev $(PREFIX)/bin/code

## run: build, then start the TUI
run: build
	@./$(bin)

## test: run the workspace suite
test:
	@cd code-rs && $(cargo) nextest run --no-fail-fast

## clippy: lint the workspace
clippy:
	@cd code-rs && $(cargo) clippy --tests

## clean: drop build artifacts
clean:
	@cd code-rs && $(cargo) clean

help:
	@sed -n 's/^## //p' $(MAKEFILE_LIST)
