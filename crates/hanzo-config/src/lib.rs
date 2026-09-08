//! Hanzo Cloud defaults and an isolated product home.
use std::ffi::OsString;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

mod provider;
pub use provider::activate_provider;
pub use provider::clear_hanzo_api_key;
pub use provider::hanzo_credential;
pub use provider::save_hanzo_api_key;
pub use provider::set_feature;
pub use provider::Provider;

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
    share_credential(&home);
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
        .map(|home| PathBuf::from(home).join(".hanzo/dev"))
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "set DEV_HOME or HOME to a Hanzo profile directory",
            )
        })
}

/// One account across the Hanzo tools, so signing in to any of them signs in to
/// all of them. Dev keeps its own sessions and settings; only the credential is
/// shared, and only when the profile is the default one under `~/.hanzo`.
fn share_credential(home: &Path) {
    let (Some(hanzo), Some(name)) = (home.parent(), home.file_name()) else {
        return;
    };
    if name != "dev" || !hanzo.ends_with(".hanzo") {
        return; // An explicit DEV_HOME is its own island.
    }
    let shared = hanzo.join("auth.json");
    let ours = home.join("auth.json");
    match std::fs::symlink_metadata(&ours) {
        // A real file here predates the shared credential; leave the user's alone.
        Ok(metadata) if !metadata.is_symlink() => return,
        Ok(_) => {
            if std::fs::read_link(&ours).is_ok_and(|target| target == shared) {
                return;
            }
            let _ = std::fs::remove_file(&ours);
        }
        Err(_) => {}
    }
    let _ = link(&shared, &ours);
}

#[cfg(unix)]
fn link(target: &Path, at: &Path) -> std::io::Result<()> {
    std::os::unix::fs::symlink(target, at)
}

#[cfg(windows)]
fn link(target: &Path, at: &Path) -> std::io::Result<()> {
    std::os::windows::fs::symlink_file(target, at)
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
env_key_instructions = "Run `dev login` to sign in to Hanzo or paste a Hanzo API key, or set HANZO_USER_KEY."
requires_openai_auth = false

[mcp_servers.hanzo]
command = "hanzo-mcp"
args = []
"#;

#[cfg(test)]
#[path = "config_tests.rs"]
mod tests;
