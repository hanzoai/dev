use std::io;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use toml_edit::DocumentMut;

/// Credential and model routing selected by a successful login.
#[derive(Clone, Copy)]
pub enum Provider {
    Hanzo,
    OpenAi,
}

/// Select a provider without changing unrelated user settings or storing tokens.
pub fn activate_provider(home: &Path, provider: Provider) -> io::Result<()> {
    edit(home, |config| match provider {
        Provider::Hanzo => {
            config["model_provider"] = toml_edit::value("hanzo");
            config["model"] = toml_edit::value(super::DEFAULT_MODEL);
        }
        Provider::OpenAi => {
            config["model_provider"] = toml_edit::value("openai");
            // Allow the authenticated model catalog to choose its current default.
            if config.get("model").and_then(toml_edit::Item::as_str) == Some(super::DEFAULT_MODEL) {
                config.remove("model");
            }
        }
    })
}

/// Turn one feature on or off for this profile.
pub fn set_feature(home: &Path, key: &str, on: bool) -> io::Result<()> {
    edit(home, |config| config["features"][key] = toml_edit::value(on))
}

/// Rewrite the product config, leaving every setting the change does not name
/// — and every comment around them — exactly as the user left it.
fn edit(home: &Path, change: impl FnOnce(&mut DocumentMut)) -> io::Result<()> {
    let path = home.join("config.toml");
    let text = std::fs::read_to_string(&path)?;
    let mut config = text.parse::<DocumentMut>().map_err(io::Error::other)?;
    change(&mut config);
    let mut file = tempfile::NamedTempFile::new_in(home)?;
    file.write_all(config.to_string().as_bytes())?;
    file.as_file().sync_all()?;
    file.persist(path).map_err(|error| error.error)?;
    Ok(())
}

/// The Hanzo account credential, once the native CLI has been consulted.
pub fn hanzo_credential() -> Option<String> {
    std::env::var("HANZO_USER_KEY").ok().filter(|key| !key.is_empty())
}

// Called only from the synchronous entry point, before any threads start.
pub(super) fn load_hanzo_credentials(home: &Path) {
    if std::env::var_os("HANZO_USER_KEY").is_some_and(|key| !key.is_empty()) {
        return;
    }
    // Help and version must work without consulting any credential service.
    if std::env::args_os().skip(1).any(|arg| matches!(arg.to_str(), Some("--help" | "-h" | "--version" | "-V" | "login" | "logout"))) {
        return;
    }
    let Ok(text) = std::fs::read_to_string(home.join("config.toml")) else { return };
    let Ok(config) = text.parse::<DocumentMut>() else { return };
    if config.get("model_provider").and_then(toml_edit::Item::as_str) != Some("hanzo") {
        return;
    }
    // The native Hanzo CLI owns storage and refresh of the shared account token.
    // Capture both streams so credentials and child diagnostics never reach logs.
    let Ok(output) = Command::new("hanzo").args(["auth", "token"]).output() else { return };
    if !output.status.success() || output.stdout.len() > 16_384 {
        return;
    }
    let Ok(token) = String::from_utf8(output.stdout) else { return };
    let token = token.trim();
    if !token.is_empty() && !token.contains(char::is_whitespace) {
        std::env::set_var("HANZO_USER_KEY", token);
    }
}

#[cfg(test)]
#[path = "provider_tests.rs"]
mod tests;
