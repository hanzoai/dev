# Upstream Parity Maintenance Checklist

Quick reference for maintaining parity with codex-rs MCP components.

**Status (2025-10-06):** MCP crates re-export upstream `codex-rs` crates. Monitoring now focuses on upstream commits and wrapper glue.

## Monthly Cadence

We follow a two-step cycle aligned with the broader upstream diff process (see `docs/maintenance/upstream-diff.md`).

### First Monday — Quick Diff
- [ ] Fetch latest upstream: `git fetch upstream`
- [ ] Inspect new commits: `git log HEAD..upstream/main --oneline`
- [ ] Run structural diff: `./scripts/upstream-merge/diff-crates.sh --all`
- [ ] Review summary: `cat .github/auto/upstream-diffs/SUMMARY.md`
- [ ] Check key crates for breaking changes:
  - [ ] `mcp-client/`
  - [ ] `responses-api-proxy/`
  - [ ] `process-hardening/`
  - [ ] `mcp-types/`
- [ ] Decide whether a merge plan is required this month

### Second Monday — Merge Planning (if needed)
- [ ] Highlight critical changes: `./scripts/upstream-merge/highlight-critical-changes.sh --all`
- [ ] Review critical summary: `cat .github/auto/upstream-diffs/critical-changes/CRITICAL-SUMMARY.md`
- [ ] Initialize merge log: `./scripts/upstream-merge/log-merge.sh init upstream/main`
- [ ] Categorize each change (adopt/adapt/preserve) and document decisions
- [ ] Identify wrapper updates required in `code-rs`
- [ ] Queue follow-up work items (smoke tests, documentation)

### Standing Checks
- [ ] Run `cargo build --workspace`
- [ ] Run `cargo test -p mcp-types`
- [ ] Review relevant upstream issues/PRs affecting MCP components
- [ ] Note manual validation required for MCP client/proxy flows (legacy integration suite removed)

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

### Code Review (Re-Export Model)
- [ ] Review all upstream changes since last month
- [ ] Verify re-export wrappers (`code-mcp-client`, `code-responses-api-proxy`, `code-process-hardening`, `code-mcp-types`) remain thin/minimal
- [ ] Identify opportunities for code-rs usage patterns to simplify
- [ ] Identify code-rs integration improvements worth contributing upstream
- [ ] **Note:** Divergence now minimal; focus on ensuring re-exports stay up-to-date

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

### Testing (Post-Phase 1 Cleanup)
- [ ] Run full build: `cargo build --workspace`
- [ ] Run minimal test suite: `cargo test -p mcp-types -p code-linux-sandbox -p code-cloud-tasks`
- [ ] Manual integration validation with real MCP servers (no automated tests currently)
- [ ] Security hardening validation (landlock tests in `linux-sandbox`)
- [ ] Cross-platform build verification (Linux, macOS, Windows)
- [ ] **Note:** Full integration test suite removed; rely on build verification and manual validation

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

## Quick Commands Reference (Updated for Re-Export Model)

```bash
# Check upstream changes
cd codex-rs
git fetch origin
git log origin/main --since="1 week ago" -- mcp-client/ responses-api-proxy/ process-hardening/ mcp-types/

# Verify re-exports build with upstream changes
cd code-rs
cargo build --workspace

# Run minimal test suite
cargo test -p mcp-types
cargo test -p code-linux-sandbox
cargo test -p code-cloud-tasks

# Security audit
cargo audit

# Check process hardening (Linux)
ps aux | grep code-responses
cat /proc/$(pgrep code-responses)/status | grep Dumpable

# Check process hardening (macOS)
ps aux | grep code-responses
lldb -p $(pgrep code-responses)  # Should fail

# Verify re-export wrappers are minimal (no local forks to diff)
# code-mcp-client, code-responses-api-proxy, code-process-hardening, code-mcp-types
# should all be thin re-export wrappers pointing to codex-rs counterparts
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
   - Path filter: `mcp-types/**`

## Metrics to Track (Updated for Re-Export Model)

| Metric | Target | Current (2025-10-05) | Trend |
|--------|--------|----------------------|-------|
| Upstream divergence (wrapper LOC) | <50 | ~0 (re-exports only) | ✅ |
| Re-export wrapper complexity | Minimal | Thin wrappers | ✅ |
| Build success with latest upstream | 100% | ✅ | — |
| Security audit issues | 0 | TBD | — |
| Days since upstream sync check | <7 | 0 | — |

**Note:** With re-export model, "divergence" is effectively zero. Focus shifts to monitoring upstream API stability and ensuring wrappers remain minimal.

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
**Model**: Re-export wrappers (`code-mcp-client`, `code-responses-api-proxy`, `code-process-hardening`, `code-mcp-types` all re-export from `codex-rs`)
