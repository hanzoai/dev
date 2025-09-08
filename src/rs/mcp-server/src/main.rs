use dev_arg0::arg0_dispatch_or_else;
use dev_common::CliConfigOverrides;
use dev_mcp_server::run_main;

fn main() -> anyhow::Result<()> {
    arg0_dispatch_or_else(|dev_linux_sandbox_exe| async move {
        run_main(dev_linux_sandbox_exe, CliConfigOverrides::default()).await?;
        Ok(())
    })
}
