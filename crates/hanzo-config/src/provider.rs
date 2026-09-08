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
            if config
                .get("model_providers")
                .and_then(|providers| providers.get("hanzo"))
                .is_none()
            {
                let defaults = super::DEFAULT_CONFIG
                    .parse::<DocumentMut>()
                    .expect("valid default config");
                config["model_providers"]["hanzo"] = defaults["model_providers"]["hanzo"].clone();
            }
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
    edit(home, |config| {
        config["features"][key] = toml_edit::value(on)
    })
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

/// The selected Hanzo credential: explicit environment, saved API key, or IAM.
pub fn hanzo_credential() -> Option<String> {
    credential(&super::home())
}

/// Save a pasted Hanzo API key in the profile's private credential file.
/// It is separate from both user configuration and the ChatGPT credential.
pub fn save_hanzo_api_key(home: &Path, key: &str) -> io::Result<()> {
    if key.is_empty()
        || key.len() > 16_384
        || key.chars().any(|c| c.is_whitespace() || c.is_control())
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "invalid API key",
        ));
    }
    let mut file = tempfile::NamedTempFile::new_in(home)?;
    file.write_all(key.as_bytes())?;
    file.as_file().sync_all()?;
    file.persist(home.join("hanzo-api-key"))
        .map_err(|error| error.error)?;
    Ok(())
}

/// Forget a pasted API key when signing out or switching to a Hanzo account.
pub fn clear_hanzo_api_key(home: &Path) -> io::Result<()> {
    match std::fs::remove_file(home.join("hanzo-api-key")) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error),
    }
}

fn credential(home: &Path) -> Option<String> {
    if let Some(key) = std::env::var("HANZO_USER_KEY")
        .ok()
        .filter(|key| !key.is_empty())
    {
        return Some(key);
    }
    if let Ok(key) = std::fs::read_to_string(home.join("hanzo-api-key")) {
        if !key.is_empty()
            && key.len() <= 16_384
            && !key.chars().any(|c| c.is_whitespace() || c.is_control())
        {
            return Some(key);
        }
    }
    // The native CLI owns storage and refresh of the shared Hanzo IAM account.
    // Capture both streams so credentials never reach logs.
    let output = Command::new("hanzo")
        .args(["auth", "token"])
        .output()
        .ok()?;
    if !output.status.success() || output.stdout.len() > 16_384 {
        return None;
    }
    let token = String::from_utf8(output.stdout).ok()?;
    let token = token.trim();
    (!token.is_empty() && !token.chars().any(|c| c.is_whitespace() || c.is_control()))
        .then(|| token.to_owned())
}

// Called only from the synchronous entry point, before any threads start.
pub(super) fn load_hanzo_credentials(home: &Path) {
    // Help and login commands must work without consulting a credential service.
    if std::env::args_os().skip(1).any(|arg| {
        matches!(
            arg.to_str(),
            Some("--help" | "-h" | "--version" | "-V" | "login" | "logout")
        )
    }) {
        return;
    }
    let Ok(text) = std::fs::read_to_string(home.join("config.toml")) else {
        return;
    };
    let Ok(config) = text.parse::<DocumentMut>() else {
        return;
    };
    if config
        .get("model_provider")
        .and_then(toml_edit::Item::as_str)
        != Some("hanzo")
    {
        return;
    }
    if let Some(token) = credential(home) {
        std::env::set_var("HANZO_USER_KEY", token);
    }
}

#[cfg(test)]
#[path = "provider_tests.rs"]
mod tests;
