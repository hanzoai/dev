Upstream Merge Report

Base: upstream/main → Branch: upstream-merge
Mode: by-bucket

Incorporated
- Adopted upstream changes outside protected areas where non-conflicting.
- Auto-merges retained for Rust workspace crates not covered by prefer‑ours.
- Build verified via `scripts/upstream-merge/verify.sh` (build_fast=ok, api_check=ok).

Dropped / Kept Ours
- `.github/workflows/**`: kept our deletions (ci.yml, rust-release.yml) per policy.
- `codex-cli/**`: kept our forked CLI setup; resolved `package.json` to ours; removed upstream-added files to avoid behavior drift:
  - Removed `codex-cli/bin/rg`.
  - Removed `codex-cli/scripts/build_npm_package.py`.
  - Removed `codex-cli/scripts/install_native_deps.py`.
  - Kept our `install_native_deps.sh` and `stage_rust_release.py`.

Fork Invariants Preserved
- Tool families: browser_*, agent_*, and web_fetch handlers remain registered with gating; no upstream changes conflicted.
- Screenshot queuing and TUI rendering untouched.
- Version/UA: continued use of `codex_version::version()` and `get_codex_user_agent_default()`.
- Public exports intact in codex-core: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`, and `codex_core::models` alias.

Other Notes
- No purged assets (`.github/codex-cli-*.{png,jpg,jpeg,webp}`) were reintroduced.
- No ICU/sys-locale dependency removal attempted (still in use per workspace build).

Verification
- `./build-fast.sh`: success, zero warnings.
- Static guards: tools + UA/version checks passed.
