//! The product identity of `dev`, expressed as assertions.
//!
//! This repo tracks an upstream (`openai/codex`) and `upstream-merge.yml` merges
//! from it every 30 minutes. A one-time edit to a default is therefore not a
//! decision — it is a suggestion that survives until the next merge. These tests
//! are how the decision is made durable: a merge that reverts any of them turns
//! the build red instead of quietly shipping someone else's defaults.
//!
//! This file has no counterpart upstream, so a merge cannot conflict with it or
//! carry away the guard along with the thing it guards.
//!
//! If you are here because this test failed after an upstream merge: the merge
//! reverted our identity. Re-apply it — do not delete the test.

use code_core::HANZO_PROVIDER_ID;
use code_core::WireApi;
use code_core::built_in_model_providers;
use code_core::config::HANZO_DEFAULT_MODEL;
use code_core::config::HANZO_DEFAULT_REVIEW_MODEL;

/// Every model `dev` may pick by default belongs to the Zen family. Naming an
/// upstream model in our own copy is the thing this guards against.
fn is_zen_model(slug: &str) -> bool {
    slug.starts_with("zen") || slug.starts_with("enso")
}

#[test]
fn the_hanzo_cloud_is_a_built_in_provider() {
    let providers = built_in_model_providers(None);
    let hanzo = providers
        .get(HANZO_PROVIDER_ID)
        .expect("the Hanzo Cloud must be a built-in provider");

    assert_eq!(hanzo.name, "Hanzo");
    assert_eq!(
        hanzo.base_url.as_deref(),
        Some("https://api.hanzo.ai/v1"),
        "the Hanzo Cloud answers at api.hanzo.ai/v1"
    );
    assert_eq!(
        hanzo.env_key.as_deref(),
        Some("HANZO_USER_KEY"),
        "`hanzo code` sets HANZO_USER_KEY on the child; the provider must read it"
    );
    assert!(
        matches!(hanzo.wire_api, WireApi::Responses),
        "verified against the live gateway: /v1/responses serves our models"
    );
    assert!(
        !hanzo.requires_openai_auth,
        "this account is ours; the ChatGPT login flow never applies to it"
    );
}

#[test]
fn the_default_model_is_ours() {
    assert!(
        is_zen_model(HANZO_DEFAULT_MODEL),
        "default model must be a Zen model, got `{HANZO_DEFAULT_MODEL}`"
    );
    assert!(
        is_zen_model(HANZO_DEFAULT_REVIEW_MODEL),
        "default review model must be a Zen model, got `{HANZO_DEFAULT_REVIEW_MODEL}`"
    );
}

/// The installer contract, `hanzo code`, and the Homebrew formula all resolve a
/// binary named `dev` off PATH. A workspace that builds no such binary means a
/// release ships nothing anyone can run.
#[test]
fn the_cli_builds_a_binary_named_dev() {
    let manifest = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../cli/Cargo.toml"
    ))
    .expect("read code-rs/cli/Cargo.toml");

    let bins: Vec<&str> = manifest
        .split("[[bin]]")
        .skip(1)
        .filter_map(|section| {
            section
                .lines()
                .find_map(|l| l.trim().strip_prefix("name = "))
                .map(|n| n.trim().trim_matches('"'))
        })
        .collect();

    assert!(
        bins.contains(&"dev"),
        "code-rs/cli must build a binary named `dev`; it builds {bins:?}"
    );
    assert_eq!(
        bins.first(),
        Some(&"dev"),
        "`dev` is the product name and must be declared first; the rest are aliases"
    );
}

/// We publish our releases to namespaces we own. Publishing into a third party's
/// namespace is not a style question — it hands them our distribution.
#[test]
fn the_release_pipeline_publishes_to_our_namespaces() {
    let release_yml = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../.github/workflows/release.yml"
    ))
    .expect("read .github/workflows/release.yml");

    for (needle, why) in [
        ("@just-every/", "npm packages must publish under @hanzo/"),
        (
            "just-every/homebrew-tap",
            "the Homebrew formula belongs in hanzoai/homebrew-tap",
        ),
        (
            "just-every/code/releases",
            "release assets must be served from hanzoai/dev",
        ),
    ] {
        assert!(
            !release_yml.contains(needle),
            "release.yml still references `{needle}` — {why}"
        );
    }

    assert!(
        release_yml.contains("--bin dev"),
        "the release must build the `dev` binary"
    );
}

/// A SERVICE HAS NO HUMAN SESSION, and reading only `HANZO_USER_KEY` made that
/// case impossible: `dev` inside a sandbox pod reported "Authentication expired.
/// Run `code login` to continue" for a caller that was perfectly authenticated,
/// because the one variable it would look at is the one no service ever has.
#[test]
fn the_hanzo_provider_accepts_a_machine_credential() {
    let p = code_core::create_hanzo_provider();
    for want in ["HANZO_API_KEY", "HANZO_MACHINE_TOKEN"] {
        assert!(
            p.alt_env_keys.iter().any(|k| k == want),
            "a service run carries {want}; the provider must read it"
        );
    }
    assert_eq!(
        p.env_key.as_deref(),
        Some("HANZO_USER_KEY"),
        "the human session stays the declared key"
    );
}

/// MACHINE FIRST, and deliberately. Where both are present the process was
/// started BY something, and the credential it was handed is the one that should
/// be billed and audited — a stale human key left in an environment must not
/// quietly outrank the identity the caller actually presented.
#[test]
fn a_machine_credential_outranks_a_stale_human_one() {
    let p = code_core::create_hanzo_provider();
    let idx = |name: &str| {
        p.alt_env_keys
            .iter()
            .chain(p.env_key.iter())
            .position(|k| k == name)
            .expect("declared")
    };
    assert!(
        idx("HANZO_API_KEY") < idx("HANZO_USER_KEY"),
        "the machine credential must be tried before the human session"
    );
}

/// A repo-relative path, read from the test's own manifest location.
fn repo_file(rel: &str) -> String {
    let path = format!("{}/../../{rel}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

/// Auto Drive drives the same gateway everything else does. Its defaults were
/// `gpt-5.1` for the model and `gpt-5.5`/`gpt-5.4-mini` for routing — ids the
/// Hanzo Cloud answers with `400 model is not available`, so the feature was
/// broken out of the box for the default provider rather than merely misnamed.
#[test]
fn auto_drive_defaults_to_models_our_gateway_serves() {
    let settings = code_core::config_types::AutoDriveSettings::default();

    assert!(
        is_zen_model(&settings.model),
        "Auto Drive defaults to `{}`, which the Hanzo Cloud does not serve",
        settings.model
    );

    for entry in code_core::config_types::default_auto_drive_model_routing_entries() {
        assert!(
            is_zen_model(&entry.model),
            "Auto Drive routes to `{}`, which the Hanzo Cloud does not serve",
            entry.model
        );
    }
}

/// `brew upgrade code` names a formula that does not exist; ours is
/// `Formula/hanzo-dev.rb`. This string is not merely displayed — the TUI runs it.
#[test]
fn the_homebrew_upgrade_command_names_our_formula() {
    let updates = repo_file("code-rs/tui/src/updates.rs");

    assert!(
        updates.contains("brew upgrade hanzo-dev"),
        "the guided Homebrew upgrade must name the `hanzo-dev` formula"
    );
    assert!(
        !updates.contains(r#""upgrade".to_string(),
                        "code".to_string(),"#),
        "the Homebrew upgrade must not target a `code` formula"
    );
}

/// The first screen a new user sees. It greeted them as OpenAI's product for
/// months after the rest of the identity had been fixed, because nothing here
/// looked at rendered text — only at structural defaults.
#[test]
fn the_onboarding_screen_introduces_our_product() {
    let welcome = repo_file("code-rs/tui/src/onboarding/welcome.rs");
    assert!(
        welcome.contains("Welcome to Hanzo Dev"),
        "the welcome screen must greet the user as Hanzo Dev"
    );

    // The screen serves both providers, so the OpenAI copy — including a link to
    // ChatGPT's settings — legitimately survives on that branch. What must hold
    // is that the Hanzo path exists and is the one a default install is offered.
    let auth = repo_file("code-rs/tui/src/onboarding/auth.rs");
    for needle in [
        "Sign in with Hanzo",
        "Sign in with Hanzo to use your Hanzo Cloud account",
    ] {
        assert!(
            auth.contains(needle),
            "the sign-in screen must offer the Hanzo path (`{needle}` is missing)"
        );
    }
}
