//! Hanzo Cloud defaults and an isolated product home.
use std::ffi::OsString;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

pub mod kai;
mod provider;
pub use provider::activate_provider;
pub use provider::clear_hanzo_api_key;
pub use provider::hanzo_credential;
pub use provider::hanzo_session_expired;
pub use provider::save_hanzo_api_key;
pub use provider::set_feature;
pub use provider::set_memory_provider;
pub use provider::Provider;

/// The active product home. Meaningful once [`initialize`] has run.
pub fn home() -> PathBuf {
    std::env::var_os("CODEX_HOME")
        .map(PathBuf::from)
        .expect("initialize runs before any command")
}

/// Called once by the synchronous entry point, before the async runtime starts.
pub fn initialize() -> std::io::Result<()> {
    let home = product_home(
        std::env::var_os("DEV_HOME"),
        std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE")),
    )?;
    initialize_home(&home)?;
    provider::default_to_hanzo(&home);
    provider::upgrade_auth(&home);
    provider::upgrade_effort(&home);
    share_credential(&home);
    provider::load_hanzo_credentials(&home);
    // This function is only invoked before upstream starts threads.
    std::env::set_var("CODEX_HOME", home);
    Ok(())
}

fn product_home(explicit: Option<OsString>, user: Option<OsString>) -> std::io::Result<PathBuf> {
    if let Some(home) = explicit.filter(|home| !home.is_empty()) {
        return Ok(PathBuf::from(home));
    }
    user.filter(|home| !home.is_empty())
        .map(|home| PathBuf::from(home).join(".hanzo/dev"))
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "set DEV_HOME or HOME to a Hanzo profile directory",
            )
        })
}

/// One account across the Hanzo tools, so signing in to any of them signs in to
/// all of them. Dev keeps its own sessions and settings; only the credential is
/// shared, and only when the profile is the default one under `~/.hanzo`.
///
/// At startup the profile links to the shared credential only when it exists; a
/// link to nothing is removed, so the profile never holds a dangling link.
fn share_credential(home: &Path) {
    link_shared(home, false);
}

/// Point the profile's credential at the shared one before a sign-in saves it,
/// so the sign-in writes the one credential every Hanzo tool reads. The link may
/// name a file the sign-in is about to create; a sign-in that does not finish
/// leaves it for the next start to remove.
pub fn share_for_login(home: &Path) {
    link_shared(home, true);
}

fn link_shared(home: &Path, for_login: bool) {
    let (Some(hanzo), Some(name)) = (home.parent(), home.file_name()) else {
        return;
    };
    if name != "dev" || !hanzo.ends_with(".hanzo") {
        return; // An explicit DEV_HOME is its own island.
    }
    let shared = hanzo.join("auth.json");
    let ours = home.join("auth.json");
    if !for_login && !shared.exists() {
        // Nothing to share yet: a link here would name a file that is not there.
        if std::fs::symlink_metadata(&ours).is_ok_and(|metadata| metadata.is_symlink()) {
            let _ = std::fs::remove_file(&ours);
        }
        return;
    }
    match std::fs::symlink_metadata(&ours) {
        // A real file here predates the shared credential; leave the user's alone.
        Ok(metadata) if !metadata.is_symlink() => return,
        Ok(_) => {
            if std::fs::read_link(&ours).is_ok_and(|target| target == shared) {
                return;
            }
            let _ = std::fs::remove_file(&ours);
        }
        Err(_) => {}
    }
    let _ = link(&shared, &ours);
}

#[cfg(unix)]
fn link(target: &Path, at: &Path) -> std::io::Result<()> {
    std::os::unix::fs::symlink(target, at)
}

#[cfg(windows)]
fn link(target: &Path, at: &Path) -> std::io::Result<()> {
    std::os::windows::fs::symlink_file(target, at)
}

fn initialize_home(home: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(home)?;
    initialize_default_skills(home)?;
    initialize_default_kai(home)?;
    let config = home.join("config.toml");
    if config.exists() {
        return Ok(());
    }
    let mut file = tempfile::NamedTempFile::new_in(home)?;
    file.write_all(DEFAULT_CONFIG.as_bytes())?;
    file.as_file().sync_all()?;
    match file.persist_noclobber(config) {
        Ok(_) => Ok(()),
        Err(error) if error.error.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
        Err(error) => Err(error.error),
    }
}

fn initialize_default_kai(home: &Path) -> std::io::Result<()> {
    let kai_path = home.join(kai::FILE);
    if kai_path.exists() {
        return Ok(());
    }
    let mut file = tempfile::NamedTempFile::new_in(home)?;
    file.write_all(DEFAULT_KAI.as_bytes())?;
    file.as_file().sync_all()?;
    match file.persist_noclobber(kai_path) {
        Ok(_) => Ok(()),
        Err(error) if error.error.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
        Err(error) => Err(error.error),
    }
}

fn initialize_default_skills(home: &Path) -> std::io::Result<()> {
    for (name, content) in DEFAULT_SKILLS {
        let dir = home.join("skills").join(name);
        let skill_file = dir.join("SKILL.md");
        if skill_file.exists() {
            continue;
        }
        std::fs::create_dir_all(&dir)?;
        let mut file = tempfile::NamedTempFile::new_in(&dir)?;
        file.write_all(content.as_bytes())?;
        file.as_file().sync_all()?;
        match file.persist_noclobber(skill_file) {
            Ok(_) => {}
            Err(error) if error.error.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(error) => return Err(error.error),
        }
    }
    Ok(())
}

pub const DEFAULT_SKILLS: &[(&str, &str)] = &[
    (
        "code-review",
        r#"---
name: code-review
description: "Hanzo standard code quality, simplification, and correctness review: structural simplicity, 1,000-line file discipline, blast-radius verification, runtime proof, deslop, and cross-boundary agreement checks. Enabled by default across Hanzo Dev, Hanzo MCP, and Hanzo AI."
source: hanzoai/skills
---

# Code Review (`code-review`)

You are the senior engineer who gets paged at 3 AM when this code breaks, and the custodian of codebase simplicity who will still own this repository a year from now.

Every line you let through becomes everyone's problem. Every helper, mode flag, and ad-hoc branch you fail to challenge compounds into spaghetti. Your job is to enforce **structural simplicity**, **cross-boundary correctness**, **runtime-verified safety**, and **zero slop**.

## 1. Priority Order (Never Invert)
1. **Structural Simplicity** — Can a slightly different framing delete whole branches, helpers, flags, or layers?
2. **Correctness across Boundaries** — Do callers, callees, producers, consumers, and async state transitions agree under every reachable execution?
3. **Safe** — Are auth, tenant boundaries, secrets, untrusted inputs, and injection vectors locked down?
4. **Holds under Real Load** — Does it survive 10× traffic, partial outages, retries, out-of-order completion, and bounded memory?
5. **Tested & Runtime-Proven** — Are failure modes tested and proven by running code, not asserted from reading?
6. **Fast & Atomic** — Is independent async work parallelized and are multi-step state transitions atomic?
7. **Lean & Deslopped** — Does every remaining line, type, and comment earn its keep?

## 2. Simplification Rules
- **Rule 0 (Restructure Before Patching):** Delete whole branches by normalizing data once at the boundary; collapse modes/flags; push invariants into types or state machines.
- **Rule 1 (<1,000-Line Files):** Do not let a change push a file past 1,000 lines. Split by domain responsibility or lifecycle boundary.
- **Rule 2 (No Spaghetti Growth):** Reject ad-hoc `if` branches, boolean mode flags, and special cases bolted onto unrelated flows.
- **Rule 3 (Delete Moving Pieces):** Challenge every new class, wrapper, hook, manager, or DTO: what breaks if it is deleted and inlined?
- **Rule 4 (Strict Boundaries):** Parse and validate at the boundary once; trust strong types inside the core.
- **Rule 5 (Canonical Layer & Helpers):** Reuse canonical helpers instead of adding parallel utilities.
- **Rule 6 (Parallelize & Atomic State):** Parallelize independent async work; make multi-step state updates atomic or idempotent.

## 3. Blast Radius, Runtime Proof & Deslop
- **Blast Radius:** Grep every caller and consumer whenever a signature, return shape, payload, or column changes.
- **Prove It Works:** Construct the violating execution; run tests where available; require failure-path tests.
- **Comment Discipline & Deslop:** Strip comments that restate what code does; keep only 1-line `why` comments and explicit `// shortcut: ...` notes.

## 4. Cross-Boundary Agreement
Explicitly probe across every boundary (Caller ↔ Callee, Client ↔ API, Writer ↔ Reader, Proposal ↔ Authority):
1. **Authority Reconciliation:** Does any downstream state read the requested/proposal value when the authority committed or returned a normalized/different one?
2. **Identity & Correlation Under Concurrency:** Under overlap, out-of-order completion, or re-upload, does an operation mutate or delete the wrong version/target?
3. **Absorption vs. Prevention:** Never drop a finding because a cache, retry, or default usually hides it; drop only when an enclosing mechanism makes the bad execution impossible.

## 5. Review Output Format
Start with `Verdict` and counts (`Must Fix: X · Should Fix: Y · Nice to Have: Z`). Group into **Must Fix**, **Should Fix**, and **Nice to Have**:
```markdown
### [Must Fix | Should Fix | Nice to Have] `path/to/file:line` — Short Title
- **What this is:** Plain-English explanation of the code path and participants.
- **Problem:** Exact execution, input, concurrency interleaving, or structural entanglement that breaks it (cite both sides).
- **Fix:** Concrete restructuring or minimal code change.
- **If we skip it:** Realistic blast radius in production.
```
"#,
    ),
    (
        "dev",
        r#"---
name: dev
description: "Hanzo Dev Mode: the smallest change that fully solves the task, and a reply a busy human understands in one read (-53% code, -41% time, -26% cost, -45% tokens, 98% of risky logic ships with a test)."
license: MIT
---

# Dev Mode

You are a pragmatic senior developer. The best code is the code never written. Solve the whole problem with the least new code. End your reply with one or two lines: what you skipped or did not check, and any risk the user must know.

1. Read the task and the code it touches. List every caller, test, fixture, config, and export your change must reach.
2. Take the smallest option that fully works: reuse existing codebase helpers, standard library, or installed dependencies before writing new abstractions.
3. No wrapper, option, config, or boilerplate nobody asked for. Deletion beats addition.
4. Comment only the `why` the code cannot show, in one line.
5. Before fixing a bug, grep every caller of the function you touch and fix the root cause once in shared code.
6. New non-trivial logic (branches, loops, parsers, money, security) ships with one focused test.
"#,
    ),
    (
        "ui",
        r#"---
name: ui
description: "Design, build, or port user interfaces on the Hanzo stack (@hanzo/ui, @hanzo/gui, @hanzo/design tokens) while achieving measured visual parity across mobile, duo, laptop, and desktop viewports."
license: MIT
---

# UI (@hanzo/ui + Visual Parity by Measurement)

1. Import layout and controls from `@hanzo/ui` and `@hanzo/gui` (`YStack`, `XStack`, `Box`, `View`, `Text`, `Button`, `Input`, `Textarea`, `Card`, `Separator`).
2. Use semantic design tokens (`$ink`, `$soft`, `$faint`, `$edge`, `$borderColor`, `$sunken`, `$hover`, `$1`–`$8`) and `Zen` / `Zen Mono` typography.
3. Converge in dependency order: shell geometry → typography → spacing/primitives → topology → compound components → states (`loading · empty · failure · populated`) → responsive viewports (`mobile · duo · laptop · desktop`).
4. Measure DOM geometry (`getBoundingClientRect()`, `getComputedStyle()`) and pixel diffs; fix token/spacing/radius root causes rather than element-specific overrides.
"#,
    ),
];

pub const DEFAULT_KAI: &str = r#"model = "kai"

[ops.model]
mode = "enforced"

[ops.reasoning]
mode = "enforced"
"#;

pub const DEFAULT_CONFIG: &str = r#"model_provider = "hanzo"
model = "enso-auto"

[tui]
show_tooltips = false

[features]
memories = true

# Background learning uses the local engine independently of the foreground
# model. An unavailable local engine leaves retryable work, never a billed fallback.
[memories]
model_provider = "local_memory"
extract_model = "halogen-qwen3.8-flash-next"
consolidation_model = "halogen-qwen3.8-flash-next"
max_rollouts_per_startup = 4
min_rollout_idle_hours = 1

[model_providers.local_memory]
name = "Local memory"
base_url = "http://127.0.0.1:8731/v1"
wire_api = "responses"
requires_openai_auth = false
request_max_retries = 0
stream_max_retries = 0

[model_providers.hanzo]
name = "Hanzo"
base_url = "https://api.hanzo.ai/v1"
wire_api = "responses"
requires_openai_auth = false

# The Hanzo sign-in is a short-lived token, so the agent asks `dev token` for it
# again before it ages out instead of reading it once at startup.
[model_providers.hanzo.auth]
command = "dev"
args = ["token"]
refresh_interval_ms = 600000

[mcp_servers.hanzo]
command = "hanzo-mcp"
args = []
"#;

#[cfg(test)]
#[path = "config_tests.rs"]
mod tests;
