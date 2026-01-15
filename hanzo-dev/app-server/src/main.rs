use hanzo_app_server::run_main;
use hanzo_arg0::arg0_dispatch_or_else;
use hanzo_common::CliConfigOverrides;

fn main() -> anyhow::Result<()> {
    arg0_dispatch_or_else(|code_linux_sandbox_exe| async move {
        run_main(code_linux_sandbox_exe, CliConfigOverrides::default()).await?;
        Ok(())
    })
}
