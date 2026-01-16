use clap::Parser;
use hanzo_arg0::arg0_dispatch_or_else;
use hanzo_common::CliConfigOverrides;
use hanzo_tui::Cli;
use hanzo_tui::ExitSummary;
use hanzo_tui::resume_command_name;
use hanzo_tui::run_main;

#[derive(Parser, Debug)]
struct TopCli {
    #[clap(flatten)]
    config_overrides: CliConfigOverrides,

    #[clap(flatten)]
    inner: Cli,
}

fn main() -> anyhow::Result<()> {
    arg0_dispatch_or_else(|code_linux_sandbox_exe| async move {
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
        } = run_main(inner, code_linux_sandbox_exe).await?;
        if !token_usage.is_zero() {
            println!(
                "{}",
                hanzo_core::protocol::FinalOutput::from(token_usage)
            );
        }
        if let Some(session_id) = session_id {
            println!(
                "To continue this session, run {} resume {}",
                resume_command_name(),
                session_id
            );
        }
        Ok(())
    })
}
