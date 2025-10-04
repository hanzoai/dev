use clap::Parser;
use code_common::CliConfigOverrides;

#[derive(Debug, Clone, clap::Subcommand)]
pub enum Command {
    /// Submit a new task non-interactively and print the created id
    Submit(SubmitArgs),
}

#[derive(Parser, Debug, Default, Clone)]
pub struct SubmitArgs {
    /// The task prompt to submit to Codex Cloud
    #[arg(value_name = "PROMPT")]
    pub prompt: String,

    /// Optional environment id (falls back to auto-detect when omitted)
    #[arg(long = "env", value_name = "ENV_ID")]
    pub env: Option<String>,

    /// Best-of-N attempts for the assistant (default: 1)
    #[arg(long = "best-of", default_value_t = 1)]
    pub best_of: usize,

    /// Enable QA/review mode when creating the task
    #[arg(long = "qa", default_value_t = false)]
    pub qa: bool,

    /// Git ref to associate with the task (default: main)
    #[arg(long = "git-ref", default_value = "main")]
    pub git_ref: String,

    /// Wait for completion and print final results
    #[arg(long = "wait", default_value_t = false)]
    pub wait: bool,
}

#[derive(Parser, Debug, Default)]
#[command(version)]
pub struct Cli {
    #[clap(skip)]
    pub config_overrides: CliConfigOverrides,

    #[clap(subcommand)]
    pub cmd: Option<Command>,
}
