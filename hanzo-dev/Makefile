.PHONY: build fast-build install clean

# Release build (slow, optimized, LTO)
build:
	cargo build --release -p hanzo-dev
	@echo "Built release binary at target/release/dev"

# Fast development build (quick iteration)
fast-build:
	cargo build --profile dev-fast -p hanzo-dev
	@echo "Built dev-fast binary at target/dev-fast/dev"

# Install to ~/.cargo/bin (primary PATH location)
install: fast-build
	cp target/dev-fast/dev ~/.cargo/bin/dev
	@~/.cargo/bin/dev --version

# Install release build
install-release: build
	cp target/release/dev ~/.cargo/bin/dev
	@~/.cargo/bin/dev --version

# Clean build artifacts
clean:
	cargo clean

# Run tests
test:
	cargo test --workspace

# Check without building
check:
	cargo check --workspace
