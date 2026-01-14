use hanzo_arg0::arg0_dispatch_or_else;
use hanzo_common::CliConfigOverrides;
use hanzo_mcp_server::run_main;

fn main() -> anyhow::Result<()> {
    arg0_dispatch_or_else(|code_linux_sandbox_exe| async move {
        run_main(code_linux_sandbox_exe, CliConfigOverrides::default()).await?;
        Ok(())
    })
}
