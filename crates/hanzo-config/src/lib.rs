//! Hanzo Cloud defaults and an isolated product home.
use std::ffi::OsString;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

mod provider;
pub use provider::Provider;
pub use provider::activate_provider;
pub use provider::hanzo_credential;
pub use provider::set_feature;

/// The active product home. Meaningful once [`initialize`] has run.
pub fn home() -> PathBuf {
    std::env::var_os("CODEX_HOME")
        .map(PathBuf::from)
        .expect("initialize runs before any command")
}

pub const API_BASE: &str = "https://api.hanzo.ai/v1";
pub const DEFAULT_MODEL: &str = "enso-auto";

/// Called once by the synchronous entry point, before the async runtime starts.
pub fn initialize() -> std::io::Result<()> {
    let home = product_home(
        std::env::var_os("DEV_HOME"),
        std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE")),
    )?;
    initialize_home(&home)?;
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
        .map(|home| PathBuf::from(home).join(".hanzo/dev2"))
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "set DEV_HOME or HOME to a Hanzo profile directory"))
}

fn initialize_home(home: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(home)?;
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

pub const DEFAULT_CONFIG: &str = r#"model_provider = "hanzo"
model = "enso-auto"

[model_providers.hanzo]
name = "Hanzo"
base_url = "https://api.hanzo.ai/v1"
wire_api = "responses"
env_key = "HANZO_USER_KEY"
env_key_instructions = "Sign in with `hanzo auth login`, then launch with `hanzo code --backend dev`, or set HANZO_USER_KEY."
requires_openai_auth = false

[mcp_servers.hanzo]
command = "hanzo-mcp"
args = []
"#;

#[cfg(test)]
#[path = "config_tests.rs"]
mod tests;
