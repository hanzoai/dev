// Forbid accidental stdout/stderr writes in the *library* portion of the TUI.
// The standalone `codex-tui` binary prints a short help message before the
// alternate‑screen mode starts; that file opts‑out locally via `allow`.
#![deny(clippy::print_stdout, clippy::print_stderr)]
#![deny(clippy::disallowed_methods)]
use app::App;
use codex_core::AuthManager;
use codex_core::BUILT_IN_OSS_MODEL_PROVIDER_ID;
use codex_core::CodexAuth;
use codex_core::config::Config;
use codex_core::config::ConfigOverrides;
use codex_core::config::ConfigToml;
use codex_core::config::find_codex_home;
use codex_core::config::load_config_as_toml_with_cli_overrides;
use codex_core::protocol::AskForApproval;
use codex_core::protocol::SandboxPolicy;
use codex_login::AuthMode;
use codex_login::CodexAuth;
use codex_ollama::DEFAULT_OSS_MODEL;
use codex_protocol::config_types::SandboxMode;
use codex_protocol::mcp_protocol::AuthMode;
use std::fs::OpenOptions;
use std::path::PathBuf;
use tracing_appender::non_blocking;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::prelude::*;

// Colorize strings printed to stderr for the release‑mode update banner.
// Gate the import so we don't trigger an unused‑import warning in debug builds.
#[cfg(not(debug_assertions))]
use color_eyre::owo_colors::OwoColorize;

mod app;
mod app_event;
mod app_event_sender;
mod bottom_pane;
mod chatwidget;
mod citation_regex;
mod cli;
mod common;
mod colors;
mod diff_render;
mod exec_command;
mod file_search;
mod get_git_diff;
mod glitch_animation;
mod history_cell;
mod insert_history;
pub mod live_wrap;
mod markdown;
mod markdown_renderer;
mod markdown_stream;
mod syntax_highlight;
pub mod onboarding;
mod pager_overlay;
mod render;
// mod scroll_view; // Orphaned after trait-based HistoryCell migration
mod session_log;
mod shimmer;
mod slash_command;
mod resume;
mod streaming;
mod terminal_info;
// mod text_block; // Orphaned after trait-based HistoryCell migration
mod text_formatting;
mod text_processing;
mod theme;
mod tui;
mod user_approval_widget;
mod height_manager;
mod transcript_app;
mod clipboard_paste;
// Upstream introduced a standalone status indicator widget. Our fork renders
// status within the composer title; keep the module private unless tests need it.
mod status_indicator_widget;

// Internal vt100-based replay tests live as a separate source file to keep them
// close to the widget code. Include them in unit tests.
#[cfg(all(test, feature = "legacy_tests"))]
mod chatwidget_stream_tests;

#[cfg(not(debug_assertions))]
mod updates;

pub use cli::Cli;

// (tests access modules directly within the crate)

pub async fn run_main(
    cli: Cli,
    codex_linux_sandbox_exe: Option<PathBuf>,
) -> std::io::Result<codex_core::protocol::TokenUsage> {
    let (sandbox_mode, approval_policy) = if cli.full_auto {
        (
            Some(SandboxMode::WorkspaceWrite),
            Some(AskForApproval::OnFailure),
        )
    } else if cli.dangerously_bypass_approvals_and_sandbox {
        (
            Some(SandboxMode::DangerFullAccess),
            Some(AskForApproval::Never),
        )
    } else {
        (
            cli.sandbox_mode.map(Into::<SandboxMode>::into),
            cli.approval_policy.map(Into::into),
        )
    };

    // When using `--oss`, let the bootstrapper pick the model (defaulting to
    // gpt-oss:20b) and ensure it is present locally. Also, force the built‑in
    // `oss` model provider.
    let model = if let Some(model) = &cli.model {
        Some(model.clone())
    } else if cli.oss {
        Some(DEFAULT_OSS_MODEL.to_owned())
    } else {
        None // No model specified, will use the default.
    };

    let model_provider_override = if cli.oss {
        Some(BUILT_IN_OSS_MODEL_PROVIDER_ID.to_owned())
    } else {
        None
    };

    // canonicalize the cwd
    let cwd = cli.cwd.clone().map(|p| p.canonicalize().unwrap_or(p));

    let overrides = ConfigOverrides {
        model,
        approval_policy,
        sandbox_mode,
        cwd,
        model_provider: model_provider_override,
        config_profile: cli.config_profile.clone(),
        codex_linux_sandbox_exe,
        base_instructions: None,
        include_plan_tool: Some(true),
        disable_response_storage: cli.oss.then_some(true),
        show_raw_agent_reasoning: cli.oss.then_some(true),
        debug: Some(cli.debug),
        // Enable web search by default (no CLI flag).
        tools_web_search_request: Some(true),
    };

    // Parse `-c` overrides from the CLI.
    let cli_kv_overrides = match cli.config_overrides.parse_overrides() {
        Ok(v) => v,
        #[allow(clippy::print_stderr)]
        Err(e) => {
            eprintln!("Error parsing -c overrides: {e}");
            std::process::exit(1);
        }
    };

    let mut config = {
        // Load configuration and support CLI overrides.

        #[allow(clippy::print_stderr)]
        match Config::load_with_cli_overrides(cli_kv_overrides.clone(), overrides) {
            Ok(config) => config,
            Err(err) => {
                eprintln!("Error loading configuration: {err}");
                std::process::exit(1);
            }
        }
    };

    // we load config.toml here to determine project state.
    #[allow(clippy::print_stderr)]
    let config_toml = {
        let codex_home = match find_codex_home() {
            Ok(codex_home) => codex_home,
            Err(err) => {
                eprintln!("Error finding codex home: {err}");
                std::process::exit(1);
            }
        };

        match load_config_as_toml_with_cli_overrides(&codex_home, cli_kv_overrides) {
            Ok(config_toml) => config_toml,
            Err(err) => {
                eprintln!("Error loading config.toml: {err}");
                std::process::exit(1);
            }
        }
    };

    let should_show_trust_screen = determine_repo_trust_state(
        &mut config,
        &config_toml,
        approval_policy,
        sandbox_mode,
        cli.config_profile.clone(),
    )?;

    let log_dir = codex_core::config::log_dir(&config)?;
    std::fs::create_dir_all(&log_dir)?;
    // Open (or create) your log file, appending to it.
    let mut log_file_opts = OpenOptions::new();
    log_file_opts.create(true).append(true);

    // Ensure the file is only readable and writable by the current user.
    // Doing the equivalent to `chmod 600` on Windows is quite a bit more code
    // and requires the Windows API crates, so we can reconsider that when
    // Codex CLI is officially supported on Windows.
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        log_file_opts.mode(0o600);
    }

    let log_file = log_file_opts.open(log_dir.join("codex-tui.log"))?;

    // Wrap file in non‑blocking writer.
    let (non_blocking, _guard) = non_blocking(log_file);

    // use RUST_LOG env var, default to info for codex crates.
    let env_filter = || {
        EnvFilter::try_from_default_env().unwrap_or_else(|_| {
            EnvFilter::new("codex_core=info,codex_tui=info,codex_browser=info")
        })
    };

    // Build layered subscriber:
    let file_layer = tracing_subscriber::fmt::layer()
        .with_writer(non_blocking)
        .with_target(false)
        .with_filter(env_filter());

    if cli.oss {
        codex_ollama::ensure_oss_ready(&config)
            .await
            .map_err(|e| std::io::Error::other(format!("OSS setup failed: {e}")))?;
    }

    let _ = tracing_subscriber::registry().with(file_layer).try_init();

    #[allow(clippy::print_stderr)]
    #[cfg(not(debug_assertions))]
    if let Some(latest_version) = updates::get_upgrade_version(&config) {
        let current_version = option_env!("CODE_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"));
        let exe = std::env::current_exe()?;
        let managed_by_npm = std::env::var_os("CODEX_MANAGED_BY_NPM").is_some();

        eprintln!(
            "{} {current_version} -> {latest_version}.",
            "Code update available!".blue()
        );

        if managed_by_npm {
            let npm_cmd = "npm install -g @just-every/code@latest";
            eprintln!("Run {} to update.", npm_cmd.cyan().on_black());
        } else if cfg!(target_os = "macos")
            && (exe.starts_with("/opt/homebrew") || exe.starts_with("/usr/local"))
        {
            let brew_cmd = "brew upgrade code";
            eprintln!("Run {} to update.", brew_cmd.cyan().on_black());
        } else {
            eprintln!(
                "See {} for the latest releases and installation options.",
                "https://github.com/just-every/code/releases/latest"
                    .cyan()
                    .on_black()
            );
        }

        eprintln!("");
    }

    run_ratatui_app(cli, config, should_show_trust_screen)
        .map_err(|err| std::io::Error::other(err.to_string()))
}

fn run_ratatui_app(
    cli: Cli,
    config: Config,
    should_show_trust_screen: bool,
) -> color_eyre::Result<codex_core::protocol::TokenUsage> {
    color_eyre::install()?;

    // Forward panic reports through tracing so they appear in the UI status
    // line, but do not swallow the default/color-eyre panic handler.
    // Chain to the previous hook so users still get a rich panic report
    // (including backtraces) after we restore the terminal.
    let prev_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        tracing::error!("panic: {info}");
        prev_hook(info);
    }));
    let (mut terminal, terminal_info) = tui::init(&config)?;
    terminal.clear()?;

    // Show update banner in terminal history (instead of stderr) so it is visible
    // within the TUI scrollback. Building spans keeps styling consistent.
    #[cfg(not(debug_assertions))]
    if let Some(latest_version) = updates::get_upgrade_version(&config) {
        use ratatui::style::Stylize as _;
        use ratatui::text::Line;

        let current_version = option_env!("CODE_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"));
        let exe = std::env::current_exe()?;
        let managed_by_npm = std::env::var_os("CODEX_MANAGED_BY_NPM").is_some();

        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::from(vec![
            "✨⬆️ Update available!".bold().cyan(),
            " ".into(),
            format!("{current_version} -> {latest_version}.").into(),
        ]));

        if managed_by_npm {
            let npm_cmd = "npm install -g @openai/codex@latest";
            lines.push(Line::from(vec![
                "Run ".into(),
                npm_cmd.cyan(),
                " to update.".into(),
            ]));
        } else if cfg!(target_os = "macos")
            && (exe.starts_with("/opt/homebrew") || exe.starts_with("/usr/local"))
        {
            let brew_cmd = "brew upgrade codex";
            lines.push(Line::from(vec![
                "Run ".into(),
                brew_cmd.cyan(),
                " to update.".into(),
            ]));
        } else {
            lines.push(Line::from(vec![
                "See ".into(),
                "https://github.com/openai/codex/releases/latest".cyan(),
                " for the latest releases and installation options.".into(),
            ]));
        }

        lines.push(Line::from(""));
        crate::insert_history::insert_history_lines(&mut terminal, lines);
    }

    // Initialize high-fidelity session event logging if enabled.
    session_log::maybe_init(&config);

    let Cli {
        prompt,
        images,
        debug,
        order,
        ..
    } = cli;
    let mut app = App::new(
        config.clone(),
        prompt,
        images,
        should_show_trust_screen,
        debug,
        order,
        terminal_info,
    );

    let app_result = app.run(&mut terminal);
    let usage = app.token_usage();

    restore();
    // Mark the end of the recorded session.
    session_log::log_session_end();
    // ignore error when collecting usage – report underlying error instead
    app_result.map(|_| usage)
}

#[expect(
    clippy::print_stderr,
    reason = "TUI should no longer be displayed, so we can write to stderr."
)]
fn restore() {
    if let Err(err) = tui::restore() {
        eprintln!(
            "failed to restore terminal. Run `reset` or restart your terminal to recover: {err}"
        );
    }
}

/// Minimal login status indicator for onboarding flow.
#[derive(Debug, Clone, Copy)]
pub enum LoginStatus {
    NotAuthenticated,
    AuthMode(AuthMode),
}

/// Determine current login status based on auth.json presence.
pub fn get_login_status(config: &Config) -> LoginStatus {
    let codex_home = config.codex_home.clone();
    match CodexAuth::from_codex_home(&codex_home, AuthMode::ApiKey) {
        Ok(Some(auth)) => LoginStatus::AuthMode(auth.mode),
        _ => LoginStatus::NotAuthenticated,
    }
}

/// Determine if user has configured a sandbox / approval policy,
/// or if the current cwd project is trusted, and updates the config
/// accordingly.
fn determine_repo_trust_state(
    config: &mut Config,
    config_toml: &ConfigToml,
    approval_policy_overide: Option<AskForApproval>,
    sandbox_mode_override: Option<SandboxMode>,
    config_profile_override: Option<String>,
) -> std::io::Result<bool> {
    let config_profile = config_toml.get_config_profile(config_profile_override)?;

    if approval_policy_overide.is_some() || sandbox_mode_override.is_some() {
        // if the user has overridden either approval policy or sandbox mode,
        // skip the trust flow
        Ok(false)
    } else if config_profile.approval_policy.is_some() {
        // if the user has specified settings in a config profile, skip the trust flow
        // todo: profile sandbox mode?
        Ok(false)
    } else if config_toml.approval_policy.is_some() || config_toml.sandbox_mode.is_some() {
        // if the user has specified either approval policy or sandbox mode in config.toml
        // skip the trust flow
        Ok(false)
    } else if config_toml.is_cwd_trusted(&config.cwd) {
        // if the current cwd project is trusted and no config has been set
        // skip the trust flow and set the approval policy and sandbox mode
        config.approval_policy = AskForApproval::OnRequest;
        config.sandbox_policy = SandboxPolicy::new_workspace_write_policy();
        Ok(false)
    } else {
        // if none of the above conditions are met, show the trust screen
        Ok(true)
    }
}
