# ZAP Development Makefile
# Zero-copy Agent Protocol

.PHONY: all build test bench lint fmt clean docs examples

# Default target
all: build test

# Build the crate
build:
	cargo build --all-features

# Build release
release:
	cargo build --release --all-features

# Run all tests
test:
	cargo test --all-features

# Run tests with output
test-verbose:
	cargo test --all-features -- --nocapture

# Run polyglot E2E tests
test-polyglot:
	cargo test --test polyglot -- --ignored --nocapture

# Run benchmarks
bench:
	cargo bench --bench zap_vs_mcp

# Lint with clippy
lint:
	cargo clippy --all-targets --all-features -- -D warnings

# Format code
fmt:
	cargo fmt

# Check formatting
fmt-check:
	cargo fmt -- --check

# Clean build artifacts
clean:
	cargo clean

# Build documentation
docs:
	cargo doc --no-deps --all-features
	@echo "Documentation built at target/doc/hanzo_zap/index.html"

# Open documentation in browser
docs-open:
	cargo doc --no-deps --all-features --open

# Run Rust example
example-rust:
	cargo run --example rust_agent

# Run Python example
example-python:
	cd examples/python && python agent.py

# Run Node.js example
example-node:
	cd examples/node && npx ts-node agent.ts

# Run Go example
example-go:
	cd examples/go && go run agent.go

# Run Ruby example
example-ruby:
	cd examples/ruby && ruby agent.rb

# Run all examples (that can run standalone)
examples:
	@echo "Running Rust example..."
	-cargo run --example rust_agent
	@echo "\nRunning Python example..."
	-cd examples/python && python agent.py
	@echo "\nRunning Ruby example..."
	-cd examples/ruby && ruby agent.rb

# Development workflow: format, lint, test
dev: fmt lint test

# CI workflow: format check, lint, test, bench
ci: fmt-check lint test bench

# Pre-commit hook
pre-commit: fmt-check lint test

# Watch for changes and run tests
watch:
	cargo watch -x test

# Install development dependencies
setup:
	cargo install cargo-watch
	cargo install cargo-nextest
	@echo "Setup complete!"

# Check for security vulnerabilities
audit:
	cargo audit

# Update dependencies
update:
	cargo update

# Generate code coverage
coverage:
	cargo tarpaulin --all-features --out Html
	@echo "Coverage report at tarpaulin-report.html"

# Help
help:
	@echo "ZAP Development Commands"
	@echo "========================"
	@echo ""
	@echo "Build:"
	@echo "  make build       - Build debug"
	@echo "  make release     - Build release"
	@echo ""
	@echo "Test:"
	@echo "  make test        - Run all tests"
	@echo "  make test-polyglot - Run polyglot E2E tests"
	@echo "  make bench       - Run benchmarks"
	@echo ""
	@echo "Quality:"
	@echo "  make lint        - Run clippy"
	@echo "  make fmt         - Format code"
	@echo "  make dev         - Format, lint, test"
	@echo ""
	@echo "Docs:"
	@echo "  make docs        - Build documentation"
	@echo "  make docs-open   - Build and open docs"
	@echo ""
	@echo "Examples:"
	@echo "  make example-rust   - Run Rust example"
	@echo "  make example-python - Run Python example"
	@echo "  make examples       - Run all examples"
