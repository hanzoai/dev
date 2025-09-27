use clap::Parser;
use codex_arg0::arg0_dispatch_or_else;
use codex_common::CliConfigOverrides;
use codex_tui::Cli;
use codex_tui::ExitSummary;
use codex_tui::run_main;
use codex_tui::RESUME_COMMAND_NAME;

#[derive(Parser, Debug)]
struct TopCli {
    #[clap(flatten)]
    config_overrides: CliConfigOverrides,

    #[clap(flatten)]
    inner: Cli,
}

fn main() -> anyhow::Result<()> {
    arg0_dispatch_or_else(|codex_linux_sandbox_exe| async move {
        let top_cli = TopCli::parse();
        let mut inner = top_cli.inner;
        inner.finalize_defaults();
        inner
            .config_overrides
            .raw_overrides
            .splice(0..0, top_cli.config_overrides.raw_overrides);
        let ExitSummary {
            token_usage,
            session_id,
        } = run_main(inner, codex_linux_sandbox_exe).await?;
        if !token_usage.is_zero() {
            println!(
                "{}",
                codex_core::protocol::FinalOutput::from(token_usage.clone())
            );
        }
        if let Some(session_id) = session_id {
            println!(
                "To continue this session, run {} resume {}.",
                RESUME_COMMAND_NAME,
                session_id
            );
        }
        Ok(())
    })
}
