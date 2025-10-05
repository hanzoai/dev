# Upstream Parity Maintenance Checklist

Quick reference for maintaining parity with codex-rs MCP components.

## Weekly Monitoring (Every Monday)

### Code Changes
- [ ] Check [codex-rs commits](https://github.com/openai/codex-rs/commits/main) for changes to:
  - [ ] `mcp-client/`
  - [ ] `responses-api-proxy/`
  - [ ] `process-hardening/`
- [ ] Review commit messages for breaking changes or new features
- [ ] Check if changes affect code-rs usage patterns

### Testing
- [ ] Run `cargo test -p core -- mcp::` with latest upstream
- [ ] Run `cargo test -p cli -- proxy::` if proxy changes detected
- [ ] Verify no new test failures introduced by upstream changes

### Issues/PRs
- [ ] Review open issues in codex-rs relevant to MCP
- [ ] Check open PRs that may affect our components
- [ ] Comment on issues affecting code-rs use cases

## Monthly Deep Dive

### Performance
- [ ] Benchmark MCP client with large responses (1MB, 2MB, 5MB)
- [ ] Measure proxy latency and throughput
- [ ] Compare performance vs previous month baseline
- [ ] Document any regressions or improvements

### Security
- [ ] Run `cargo audit` on full dependency tree
- [ ] Verify process hardening on all platforms:
  - [ ] Linux: `cat /proc/$(pgrep code-responses)/status | grep Dumpable`
  - [ ] macOS: Attempt lldb attach (should fail)
  - [ ] Windows: (TODO: Define verification)
- [ ] Review upstream security advisories
- [ ] Check for CVEs in MCP component dependencies

### Code Review
- [ ] Review all upstream changes since last month
- [ ] Identify opportunities for code-rs improvements
- [ ] Identify code-rs features worth contributing upstream
- [ ] Update fork divergence documentation if needed

### Documentation
- [ ] Update upstream-mcp-reuse-strategy.md with new findings
- [ ] Document any new workarounds or patches needed
- [ ] Review and update implementation guide if API changed
- [ ] Update this checklist with new monitoring needs

## Per Upstream Release

### Pre-Upgrade
- [ ] Read full changelog for breaking changes
- [ ] Identify deprecated APIs used by code-rs
- [ ] Plan migration for breaking changes
- [ ] Create test branch for upgrade

### Testing
- [ ] Run full test suite: `cargo test --workspace`
- [ ] Integration tests with real MCP servers
- [ ] Performance regression tests
- [ ] Security hardening validation
- [ ] Cross-platform testing (Linux, macOS, Windows)

### Documentation
- [ ] Update dependency versions in docs
- [ ] Document migration steps for breaking changes
- [ ] Update examples if API changed
- [ ] Add release notes to code-rs changelog

### Deployment
- [ ] Update Cargo.toml with new version pins
- [ ] Create PR with upgrade changes
- [ ] Run CI/CD pipeline
- [ ] Deploy to staging environment
- [ ] Monitor for issues post-deployment

## Critical Alerts (Immediate Action)

### Security Advisory
- [ ] Assess impact on code-rs
- [ ] Determine if affected components are used
- [ ] Create hotfix branch if needed
- [ ] Apply patches immediately
- [ ] Notify team of security update
- [ ] Deploy emergency update

### Upstream API Breakage
- [ ] Identify affected code in code-rs
- [ ] Assess migration effort
- [ ] Choose mitigation strategy:
  - [ ] Pin to last working version (temporary)
  - [ ] Implement adapter/wrapper
  - [ ] Migrate to new API
  - [ ] Fork and maintain (last resort)
- [ ] Create migration plan
- [ ] Execute migration or pin version

### Upstream Abandonment
- [ ] Assess recent activity level
- [ ] Contact upstream maintainers
- [ ] Evaluate fork maintenance cost
- [ ] Decision matrix:
  - [ ] Wait and monitor (if temporary)
  - [ ] Take over maintenance
  - [ ] Fork permanently
  - [ ] Replace with alternative
- [ ] Document decision and rationale

## Quick Commands Reference

```bash
# Check upstream changes
cd codex-rs
git fetch origin
git log origin/main --since="1 week ago" -- mcp-client/ responses-api-proxy/ process-hardening/

# Run MCP-specific tests
cd code-rs
cargo test -p core -- mcp::
cargo test -p cli -- proxy::

# Security audit
cargo audit

# Performance benchmark
cargo bench --bench mcp_client_bench

# Check process hardening (Linux)
ps aux | grep code-responses
cat /proc/$(pgrep code-responses)/status | grep Dumpable

# Check process hardening (macOS)
ps aux | grep code-responses
lldb -p $(pgrep code-responses)  # Should fail

# Diff implementations (if maintaining fork)
diff -u codex-rs/mcp-client/src/mcp_client.rs code-rs/mcp-client/src/mcp_client.rs
diff -u codex-rs/responses-api-proxy/src/lib.rs code-rs/responses-api-proxy/src/lib.rs
```

## GitHub Notifications Setup

1. Go to: https://github.com/openai/codex-rs
2. Click "Watch" → "Custom"
3. Enable notifications for:
   - [x] Issues
   - [x] Pull requests
   - [x] Releases
   - [x] Security alerts
4. Set up custom rules:
   - Path filter: `mcp-client/**`
   - Path filter: `responses-api-proxy/**`
   - Path filter: `process-hardening/**`

## Metrics to Track

| Metric | Target | Current | Trend |
|--------|--------|---------|-------|
| Upstream divergence (lines) | <10 | 1 | ↓ |
| Test coverage | >90% | - | - |
| Large response handling (2MB) | <100ms | - | - |
| Security audit issues | 0 | - | - |
| Days since upstream sync | <7 | - | - |

## Contact

- **Upstream Issues**: https://github.com/openai/codex-rs/issues
- **Upstream Discussions**: https://github.com/openai/codex-rs/discussions
- **Security**: security@openai.com (for vulnerabilities)

## Archive

When upstream reuse is complete and stable, archive this checklist with:
- Final divergence report
- Lessons learned
- Recommendations for future component reuse

---

**Last Updated**: 2025-10-05
**Next Review**: 2025-10-12
