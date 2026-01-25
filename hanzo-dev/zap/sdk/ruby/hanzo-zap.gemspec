# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name = "hanzo-zap"
  spec.version = "0.6.0"
  spec.authors = ["Hanzo AI"]
  spec.email = ["dev@hanzo.ai"]

  spec.summary = "Zero-copy Agent Protocol (ZAP) SDK - 1000x faster than MCP"
  spec.description = "ZAP provides a high-performance binary protocol for AI agent tool execution, " \
                     "achieving 1000x faster throughput than MCP/JSON-RPC through zero-copy serialization."
  spec.homepage = "https://github.com/hanzoai/dev/tree/main/zap"
  spec.license = "MIT"
  spec.required_ruby_version = ">= 3.1.0"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = "https://github.com/hanzoai/dev"
  spec.metadata["changelog_uri"] = "https://github.com/hanzoai/dev/blob/main/zap/CHANGELOG.md"
  spec.metadata["rubygems_mfa_required"] = "true"

  spec.files = Dir["{lib}/**/*", "LICENSE", "README.md"]
  spec.require_paths = ["lib"]

  spec.add_dependency "async", "~> 2.0"
  spec.add_dependency "async-io", "~> 1.0"
end
