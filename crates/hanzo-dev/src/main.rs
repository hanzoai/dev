//! Hanzo Dev.
//!
//! This is the whole of the command surface. Every subcommand hands off to a
//! library — the engines are upstream's, the front door is ours. Nothing here
//! is generated, and no upstream source is compiled through an `include!`.

use anyhow::Result;
use clap::Parser;
use clap::Subcommand;
use codex_arg0::Arg0DispatchPaths;
use codex_arg0::arg0_dispatch_or_else;
use codex_config::LoaderOverrides;
use codex_utils_cli::CliConfigOverrides;

mod login;

#[derive(Parser)]
#[command(
    name = "dev",
    bin_name = "dev",
    version = hanzo_tui::VERSION,
    subcommand_negates_reqs = true,
    override_usage = "dev [OPTIONS] [PROMPT]\n       dev [OPTIONS] <COMMAND> [ARGS]"
)]
/// Hanzo Dev — the Hanzo coding agent.
///
/// With no subcommand the options below open the interactive session.
struct Dev {
    #[clap(flatten)]
    interactive: codex_tui::Cli,

    // Upstream's own type, not a copy of it: several subcommands flatten this
    // same struct, and clap only tolerates the repeat because the argument id
    // matches. Declaring an identically-named `--config` of our own makes every
    // one of those subcommands fail to build its parser.
    #[clap(flatten)]
    overrides: CliConfigOverrides,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Run a task to completion without the interactive interface.
    #[command(visible_alias = "e")]
    Exec(codex_exec::Cli),

    /// Sign in to Hanzo, or to ChatGPT.
    Login(login::Login),

    /// Discard stored credentials.
    Logout,

    /// Speak the agent protocol on stdin and stdout.
    Serve,

    /// Continue an earlier session.
    Resume(Pick),

    /// Start a new session from an earlier one, leaving the original alone.
    Fork(Pick),

    /// Review the changes in this repository.
    Review(codex_exec::ReviewArgs),

    /// Switch between the sessions running here.
    Agents,

    /// Send a message to a session that is already running.
    Queue {
        /// Session id or name.
        #[arg(long)]
        thread: String,
        /// Message text.
        #[arg(long)]
        message: String,
    },

    /// Move a session out of the way.
    Archive { session: String },

    /// Bring an archived session back.
    Unarchive { session: String },

    /// Remove a session for good.
    Delete {
        session: String,
        /// Skip the confirmation. The session must be named by id.
        #[arg(long)]
        force: bool,
    },

    /// Browse cloud tasks and apply their changes here.
    Cloud(codex_cloud_tasks::Cli),

    /// Manage the tool servers the agent can reach.
    Mcp(codex_cli::mcp_cmd::McpCli),

    /// Install and remove plugins, and the marketplaces they come from.
    Plugin(codex_cli::plugin_cmd::PluginCli),

    /// Report on this installation and its configuration.
    Doctor(codex_cli::doctor::DoctorCommand),

    /// Inspect and change which features are on.
    Features {
        #[command(subcommand)]
        command: FeatureCommand,
    },

    /// Open this workspace in the desktop application.
    #[cfg(any(target_os = "macos", target_os = "windows"))]
    App(codex_cli::app_cmd::AppCommand),

    /// Run a command inside the agent sandbox.
    Sandbox(HostSandboxArgs),

    /// Print a shell completion script.
    Completion {
        #[arg(value_enum, default_value_t = clap_complete::Shell::Bash)]
        shell: clap_complete::Shell,
    },
}

#[derive(Subcommand)]
enum FeatureCommand {
    /// Show every feature, its stage, and whether it is on.
    List,
    /// Turn a feature on.
    Enable { feature: String },
    /// Turn a feature off.
    Disable { feature: String },
}

/// Which earlier session to act on. With neither, a picker opens.
#[derive(Parser)]
struct Pick {
    /// Session id or name.
    session: Option<String>,

    /// Take the most recent one.
    #[arg(long, conflicts_with = "session")]
    last: bool,

    /// Include sessions from other directories.
    #[arg(long)]
    all: bool,
}

fn main() -> Result<()> {
    codex_build_info::initialize!();
    // Resolve the Hanzo home and provider before any thread or runtime starts.
    hanzo_config::initialize()?;
    arg0_dispatch_or_else(|paths: Arg0DispatchPaths| async move { run(paths).await })
}

async fn run(paths: Arg0DispatchPaths) -> Result<()> {
    let Dev {
        mut interactive,
        overrides,
        command,
    } = Dev::parse();
    let sandbox = paths.codex_linux_sandbox_exe.clone();

    match command {
        None => {
            interactive.config_overrides = overrides;
            codex_tui::run_main(interactive, paths, LoaderOverrides::default(), None).await?;
        }
        Some(Command::Exec(mut cli)) => {
            cli.config_overrides = overrides;
            codex_exec::run_main(cli, paths).await?;
        }
        Some(Command::Login(login)) => login::run(login, overrides).await?,
        Some(Command::Logout) => login::logout(overrides).await?,
        Some(Command::Serve) => {
            codex_app_server::run_main(paths, overrides, LoaderOverrides::default(), false, false)
                .await?;
        }
        Some(Command::Cloud(mut cli)) => {
            cli.config_overrides = overrides;
            codex_cloud_tasks::run_main(cli, sandbox).await?;
        }
        Some(Command::Resume(pick)) => {
            interactive.config_overrides = overrides;
            interactive.resume_picker = pick.session.is_none() && !pick.last;
            interactive.resume_last = pick.last;
            interactive.resume_session_id = pick.session;
            interactive.resume_show_all = pick.all;
            codex_tui::run_main(interactive, paths, LoaderOverrides::default(), None).await?;
        }
        Some(Command::Fork(pick)) => {
            interactive.config_overrides = overrides;
            interactive.fork_picker = pick.session.is_none() && !pick.last;
            interactive.fork_last = pick.last;
            interactive.fork_session_id = pick.session;
            interactive.fork_show_all = pick.all;
            codex_tui::run_main(interactive, paths, LoaderOverrides::default(), None).await?;
        }
        Some(Command::Agents) => {
            interactive.config_overrides = overrides;
            interactive.agents_overview = true;
            codex_tui::run_main(interactive, paths, LoaderOverrides::default(), None).await?;
        }
        Some(Command::Review(args)) => {
            // A review is one shape of non-interactive run, and its options all
            // carry defaults, so the run is described by parsing an empty line.
            let mut cli = codex_exec::Cli::parse_from(["dev"]);
            cli.command = Some(codex_exec::Command::Review(args));
            cli.config_overrides = overrides;
            codex_exec::run_main(cli, paths).await?;
        }
        Some(Command::Queue { thread, message }) => {
            let options = session_options(interactive, overrides, paths);
            say(codex_tui::run_session_queue_command(thread, message, options).await)?;
        }
        Some(Command::Archive { session }) => {
            let options = session_options(interactive, overrides, paths);
            let action = codex_tui::SessionArchiveAction::Archive;
            say(codex_tui::run_session_archive_command(action, session, options).await)?;
        }
        Some(Command::Unarchive { session }) => {
            let options = session_options(interactive, overrides, paths);
            let action = codex_tui::SessionArchiveAction::Unarchive;
            say(codex_tui::run_session_archive_command(action, session, options).await)?;
        }
        Some(Command::Delete { session, force }) => {
            let options = session_options(interactive, overrides, paths);
            let confirmation = if force {
                codex_tui::DeleteConfirmation::Skip
            } else {
                codex_tui::DeleteConfirmation::Prompt
            };
            let action = codex_tui::SessionArchiveAction::Delete(confirmation);
            say(codex_tui::run_session_archive_command(action, session, options).await)?;
        }
        Some(Command::Mcp(mut cli)) => {
            cli.config_overrides = overrides;
            cli.run(LoaderOverrides::default()).await?;
        }
        Some(Command::Plugin(cli)) => run_plugin(cli, overrides).await?,
        Some(Command::Features { command }) => run_features(command, overrides).await?,
        Some(Command::Doctor(cli)) => {
            codex_cli::doctor::run_doctor(cli, overrides, &interactive, &paths).await?;
        }
        #[cfg(any(target_os = "macos", target_os = "windows"))]
        Some(Command::App(cli)) => codex_cli::app_cmd::run_app(cli).await?,
        Some(Command::Sandbox(args)) => {
            run_sandbox(args, sandbox, overrides).await?;
        }
        Some(Command::Completion { shell }) => {
            clap_complete::generate(
                shell,
                &mut <Dev as clap::CommandFactory>::command(),
                "dev",
                &mut std::io::stdout(),
            );
        }
    }
    Ok(())
}

async fn run_features(command: FeatureCommand, overrides: CliConfigOverrides) -> Result<()> {
    use codex_features::FEATURES;
    let set = |feature: &str, on: bool| -> Result<()> {
        if !codex_features::is_known_feature_key(feature) {
            anyhow::bail!("no such feature: {feature}");
        }
        hanzo_config::set_feature(&hanzo_config::home(), feature, on)?;
        println!("{feature} is now {}", if on { "on" } else { "off" });
        Ok(())
    };
    match command {
        FeatureCommand::List => {
            let config =
                codex_cli::cloud_config::load_config(&overrides, LoaderOverrides::default())
                    .await?;
            let width = FEATURES
                .iter()
                .map(|spec| spec.key.len())
                .max()
                .unwrap_or_default();
            for spec in FEATURES {
                let state = if config.features.enabled(spec.id) {
                    "on"
                } else {
                    "off"
                };
                println!("{:width$}  {:<17}  {state}", spec.key, stage(spec.stage));
            }
        }
        FeatureCommand::Enable { feature } => set(&feature, true)?,
        FeatureCommand::Disable { feature } => set(&feature, false)?,
    }
    Ok(())
}

fn stage(stage: codex_features::Stage) -> &'static str {
    use codex_features::Stage;
    match stage {
        Stage::UnderDevelopment => "under development",
        Stage::Experimental { .. } => "experimental",
        Stage::Stable => "stable",
        Stage::Deprecated => "deprecated",
        Stage::Removed => "removed",
    }
}

/// Report what a session command did. These report errors in their own dialect,
/// which does not convert on its own.
fn say<E: std::fmt::Display>(outcome: Result<String, E>) -> Result<()> {
    println!("{}", outcome.map_err(|error| anyhow::anyhow!("{error}"))?);
    Ok(())
}

/// The session commands reach a running app server rather than starting a
/// session of their own, but they resolve configuration the same way.
fn session_options(
    mut interactive: codex_tui::Cli,
    overrides: CliConfigOverrides,
    paths: Arg0DispatchPaths,
) -> codex_tui::SessionArchiveCommandOptions {
    interactive.config_overrides = overrides;
    codex_tui::SessionArchiveCommandOptions {
        cli: interactive,
        arg0_paths: paths,
        explicit_remote_endpoint: None,
    }
}

async fn run_plugin(
    cli: codex_cli::plugin_cmd::PluginCli,
    overrides: CliConfigOverrides,
) -> Result<()> {
    use codex_cli::plugin_cmd::PluginSubcommand;
    let parsed = || {
        overrides
            .clone()
            .parse_overrides()
            .map_err(anyhow::Error::msg)
    };
    match cli.subcommand {
        PluginSubcommand::Add(args) => codex_cli::plugin_cmd::run_plugin_add(parsed()?, args).await,
        PluginSubcommand::List(args) => {
            codex_cli::plugin_cmd::run_plugin_list(parsed()?, args).await
        }
        PluginSubcommand::Remove(args) => {
            codex_cli::plugin_cmd::run_plugin_remove(parsed()?, args).await
        }
        PluginSubcommand::Marketplace(mut marketplace) => {
            marketplace.config_overrides = overrides;
            marketplace.run().await
        }
    }
}

// Each host backend takes a different option set, so the command's arguments
// are the backend's arguments.
#[cfg(target_os = "macos")]
type HostSandboxArgs = codex_cli::SeatbeltCommand;
#[cfg(target_os = "linux")]
type HostSandboxArgs = codex_cli::LandlockCommand;
#[cfg(target_os = "windows")]
type HostSandboxArgs = codex_cli::WindowsCommand;

async fn run_sandbox(
    mut args: HostSandboxArgs,
    sandbox: Option<std::path::PathBuf>,
    overrides: CliConfigOverrides,
) -> Result<()> {
    args.config_overrides = overrides;
    let loader = LoaderOverrides::default();
    #[cfg(target_os = "macos")]
    return codex_cli::run_command_under_seatbelt(args, sandbox, loader).await;
    #[cfg(target_os = "linux")]
    return codex_cli::run_command_under_landlock(args, sandbox, loader).await;
    #[cfg(target_os = "windows")]
    return codex_cli::run_command_under_windows_sandbox(args, sandbox, loader).await;
}
