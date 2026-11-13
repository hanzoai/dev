use std::io;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;
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
            config["model"] = toml_edit::value(hanzo::DEFAULT_MODEL);
        }
        Provider::OpenAi => {
            config["model_provider"] = toml_edit::value("openai");
            // Allow the authenticated model catalog to choose its current default.
            if config.get("model").and_then(toml_edit::Item::as_str) == Some(hanzo::DEFAULT_MODEL) {
                config.remove("model");
            }
        }
    })
}

/// A profile that names no provider runs on Hanzo. Upstream reads an unset provider as
/// OpenAI, whose first screen is a sign-in Dev does not offer.
pub(super) fn default_to_hanzo(home: &Path) {
    let Ok(text) = std::fs::read_to_string(home.join("config.toml")) else {
        return;
    };
    let Ok(config) = text.parse::<DocumentMut>() else {
        return;
    };
    if config.get("model_provider").is_none() {
        let _ = activate_provider(home, Provider::Hanzo);
    }
}

/// Turn one feature on or off for this profile.
pub fn set_feature(home: &Path, key: &str, on: bool) -> io::Result<()> {
    edit(home, |config| {
        config["features"][key] = toml_edit::value(on)
    })
}

/// Enable native learning against a separately configured provider. No credential
/// is copied from the foreground, and neither memory phase can fall back to it.
pub fn set_memory_provider(home: &Path, base_url: &str, model: &str) -> io::Result<()> {
    let endpoint = url::Url::parse(base_url).map_err(io::Error::other)?;
    if !matches!(endpoint.scheme(), "http" | "https")
        || endpoint.host_str().is_none()
        || !endpoint.username().is_empty()
        || endpoint.password().is_some()
        || endpoint.query().is_some()
        || endpoint.fragment().is_some()
        || model.is_empty()
        || model.len() > 256
        || model.chars().any(|c| c.is_whitespace() || c.is_control())
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "memory needs an HTTP(S) base URL without credentials/query and a model id",
        ));
    }
    edit(home, |config| {
        config["features"]["memories"] = toml_edit::value(true);
        config["memories"]["model_provider"] = toml_edit::value("local_memory");
        config["memories"]["extract_model"] = toml_edit::value(model);
        config["memories"]["consolidation_model"] = toml_edit::value(model);
        config["memories"]["generate_memories"] = toml_edit::value(true);
        config["memories"]["use_memories"] = toml_edit::value(true);
        config["memories"]["max_rollouts_per_startup"] = toml_edit::value(4);
        config["memories"]["min_rollout_idle_hours"] = toml_edit::value(1);
        let provider = &mut config["model_providers"]["local_memory"];
        // Replace this one dedicated provider instead of inheriting an older
        // table's auth command, env key or billed fallback settings.
        *provider = toml_edit::Item::Table(toml_edit::Table::new());
        provider["name"] = toml_edit::value("Local memory");
        provider["base_url"] = toml_edit::value(base_url.trim_end_matches('/'));
        provider["wire_api"] = toml_edit::value("responses");
        provider["requires_openai_auth"] = toml_edit::value(false);
        provider["request_max_retries"] = toml_edit::value(0);
        provider["stream_max_retries"] = toml_edit::value(0);
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

/// An IAM sign-in token rather than an API key: a JWT, which ages out within the
/// hour. An API key never does.
fn is_session_token(value: &str) -> bool {
    value.starts_with("eyJ") && value.split('.').count() == 3
}

/// When an IAM sign-in token stops working, in seconds since the epoch: its own `exp` claim.
fn session_expiry(value: &str) -> Option<u64> {
    use base64::Engine;
    let payload = value.split('.').nth(1)?.trim_end_matches('=');
    let bytes = base64::engine::general_purpose::URL_SAFE_NO_PAD
        .decode(payload)
        .ok()?;
    serde_json::from_slice::<serde_json::Value>(&bytes)
        .ok()?
        .get("exp")?
        .as_u64()
}

/// A sign-in token past its `exp`. An API key, or a token that names no expiry, never is.
pub fn is_expired(value: &str) -> bool {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |elapsed| elapsed.as_secs());
    is_session_token(value) && session_expiry(value).is_some_and(|exp| exp <= now)
}

/// The Hanzo credential, in order: `HANZO_API_KEY` as given, an API key in
/// `HANZO_USER_KEY`, a pasted API key, a fresh token from the signed-in Hanzo CLI,
/// and last a sign-in token in `HANZO_USER_KEY`. That sign-in token is whatever the
/// launching process held when it started, so a fresh one from the CLI is preferred
/// to it. `HANZO_API_KEY` is the credential a host hands a run it starts (a cloud
/// sandbox has no CLI to ask), so it is taken whatever its shape.
fn credential(home: &Path) -> Option<String> {
    let set = |name| {
        std::env::var(name)
            .ok()
            .filter(|key: &String| !key.is_empty())
    };
    choose(
        set("HANZO_API_KEY"),
        set("HANZO_USER_KEY"),
        saved_key(home),
        cli_token,
    )
}

fn choose(
    given: Option<String>,
    inherited: Option<String>,
    saved: Option<String>,
    cli: impl FnOnce() -> Option<String>,
) -> Option<String> {
    given.or_else(|| pick(inherited, saved, cli))
}

fn pick(
    inherited: Option<String>,
    saved: Option<String>,
    cli: impl FnOnce() -> Option<String>,
) -> Option<String> {
    if let Some(key) = inherited.as_ref().filter(|key| !is_session_token(key)) {
        return Some(key.clone());
    }
    saved
        .or_else(cli)
        .or(inherited.filter(|token| !is_expired(token)))
}

fn saved_key(home: &Path) -> Option<String> {
    if let Ok(key) = std::fs::read_to_string(home.join("hanzo-api-key")) {
        if !key.is_empty()
            && key.len() <= 16_384
            && !key.chars().any(|c| c.is_whitespace() || c.is_control())
        {
            return Some(key);
        }
    }
    None
}

/// A fresh token from the signed-in CLI. A token already past its expiry is no sign-in:
/// the CLI hands the stale one back when it can no longer refresh, and sending it only
/// earns a 401 that the agent retries.
fn cli_token() -> Option<String> {
    cli_token_as_given().filter(|token| !is_expired(token))
}

/// Whether the CLI holds a sign-in that has run out, as opposed to none at all.
pub fn hanzo_session_expired() -> bool {
    cli_token_as_given().is_some_and(|token| is_expired(&token))
}

fn cli_token_as_given() -> Option<String> {
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

/// The arguments that sign in to Hanzo IAM through the installed Hanzo CLI. The CLI hanzo.sh
/// installs takes `--provider hanzo` (without it, it asks which provider); the unified CLI has
/// the one provider and refuses the flag. The CLI says which it is in its own help.
pub fn hanzo_login_args() -> Vec<&'static str> {
    let takes_provider = Command::new("hanzo")
        .args(["auth", "login", "--help"])
        .output()
        .is_ok_and(|output| String::from_utf8_lossy(&output.stdout).contains("--provider"));
    if takes_provider {
        vec!["auth", "login", "--provider", "hanzo"]
    } else {
        vec!["auth", "login"]
    }
}

// Called only from the synchronous entry point, before any threads start.
pub(super) fn load_hanzo_credentials(home: &Path) {
    // Help and login commands must work without consulting a credential service.
    if std::env::args_os().skip(1).any(|arg| {
        matches!(
            arg.to_str(),
            Some("--help" | "-h" | "--version" | "-V" | "login" | "logout" | "token")
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

/// Move a profile still on the startup-only credential to the refreshing one.
///
/// Earlier releases wrote the Hanzo provider with `env_key = "HANZO_USER_KEY"`
/// (or `HANZO_API_KEY`, which nothing here ever read),
/// read once when the agent starts, so a session outlived its sign-in token and
/// failed with 401 an hour in. A provider block still holding exactly that
/// default is rewritten to ask `dev token` instead; a provider the user changed
/// is theirs and stays as it is.
pub(super) fn upgrade_auth(home: &Path) {
    let path = home.join("config.toml");
    let Ok(text) = std::fs::read_to_string(&path) else {
        return;
    };
    let Ok(config) = text.parse::<DocumentMut>() else {
        return;
    };
    let Some(provider) = config
        .get("model_providers")
        .and_then(|providers| providers.get("hanzo"))
    else {
        return;
    };
    if provider.get("auth").is_some()
        || !matches!(
            provider.get("env_key").and_then(toml_edit::Item::as_str),
            Some("HANZO_USER_KEY" | "HANZO_API_KEY")
        )
        || provider.get("base_url").and_then(toml_edit::Item::as_str) != Some(hanzo::API_BASE)
    {
        return;
    }
    let _ = edit(home, |config| {
        let defaults = super::DEFAULT_CONFIG
            .parse::<DocumentMut>()
            .expect("valid default config");
        let Some(provider) = config["model_providers"]["hanzo"].as_table_like_mut() else {
            return;
        };
        provider.remove("env_key");
        provider.remove("env_key_instructions");
        provider.insert("auth", defaults["model_providers"]["hanzo"]["auth"].clone());
    });
}

/// A reasoning effort stored under a name Hanzo's table retired (`none`, `minimal`) is
/// rewritten to its successor (`instant`), at the top of the config and in every profile.
/// The /model picker wrote `none` as the default of every Hanzo model before the table gave
/// them `auto`; every other value, and every comment, stays as the user left it.
pub(super) fn upgrade_effort(home: &Path) {
    const KEYS: &[&str] = &["model_reasoning_effort", "plan_mode_reasoning_effort"];
    let Ok(text) = std::fs::read_to_string(home.join("config.toml")) else {
        return;
    };
    let Ok(config) = text.parse::<DocumentMut>() else {
        return;
    };
    let retired = |table: &dyn toml_edit::TableLike| {
        KEYS.iter().any(|key| {
            table
                .get(key)
                .and_then(toml_edit::Item::as_str)
                .and_then(hanzo::effort::successor)
                .is_some()
        })
    };
    let profiles = config
        .get("profiles")
        .and_then(toml_edit::Item::as_table_like);
    if !retired(config.as_table())
        && !profiles.is_some_and(|profiles| {
            profiles
                .iter()
                .filter_map(|(_, profile)| profile.as_table_like())
                .any(retired)
        })
    {
        return;
    }
    let _ = edit(home, |config| {
        let upgrade = |table: &mut dyn toml_edit::TableLike| {
            for key in KEYS {
                let Some(new) = table
                    .get(key)
                    .and_then(toml_edit::Item::as_str)
                    .and_then(hanzo::effort::successor)
                else {
                    continue;
                };
                if let Some(value) = table.get_mut(key).and_then(toml_edit::Item::as_value_mut) {
                    let decor = value.decor().clone();
                    *value = toml_edit::Value::from(new);
                    *value.decor_mut() = decor;
                }
            }
        };
        upgrade(config.as_table_mut());
        if let Some(profiles) = config
            .get_mut("profiles")
            .and_then(toml_edit::Item::as_table_like_mut)
        {
            for (_, profile) in profiles.iter_mut() {
                if let Some(profile) = profile.as_table_like_mut() {
                    upgrade(profile);
                }
            }
        }
    });
}

/// Ensure an existing Hanzo profile that predates native MCP bundling has
/// `[mcp_servers.hanzo]` configured and legacy upstream `.system` bundled
/// skills disabled when not explicitly set by the user.
pub(super) fn upgrade_mcp(home: &Path) {
    let path = home.join("config.toml");
    let Ok(text) = std::fs::read_to_string(&path) else {
        return;
    };
    let Ok(config) = text.parse::<DocumentMut>() else {
        return;
    };
    if config
        .get("model_provider")
        .and_then(toml_edit::Item::as_str)
        != Some("hanzo")
        || config
            .get("model_providers")
            .and_then(|p| p.get("hanzo"))
            .is_none()
    {
        return;
    }
    let hanzo_mcp = config
        .get("mcp_servers")
        .and_then(|s| s.get("hanzo"))
        .and_then(toml_edit::Item::as_table_like);
    let needs_mcp = config.get("mcp_servers").is_none();
    let needs_slim_hanzo_mcp = hanzo_mcp.is_some_and(|mcp| {
        mcp.get("command").and_then(toml_edit::Item::as_str) == Some("hanzo-mcp")
            && (mcp
                .get("args")
                .and_then(toml_edit::Item::as_array)
                .is_some_and(toml_edit::Array::is_empty)
                || mcp.get("startup_readiness").is_none()
                || mcp.get("default_tools_approval_mode").is_none())
    });
    let needs_bundled_skills_off = config
        .get("skills")
        .and_then(|s| s.get("bundled"))
        .is_none();
    if !needs_mcp && !needs_slim_hanzo_mcp && !needs_bundled_skills_off {
        return;
    }
    let _ = edit(home, |config| {
        let defaults = super::DEFAULT_CONFIG
            .parse::<DocumentMut>()
            .expect("valid default config");
        if config.get("mcp_servers").is_none() {
            config["mcp_servers"]["hanzo"] = defaults["mcp_servers"]["hanzo"].clone();
        } else if let Some(mcp) = config
            .get_mut("mcp_servers")
            .and_then(|s| s.get_mut("hanzo"))
            .and_then(toml_edit::Item::as_table_like_mut)
        {
            if mcp.get("command").and_then(toml_edit::Item::as_str) == Some("hanzo-mcp") {
                if mcp
                    .get("args")
                    .and_then(toml_edit::Item::as_array)
                    .is_some_and(toml_edit::Array::is_empty)
                {
                    mcp.insert("args", defaults["mcp_servers"]["hanzo"]["args"].clone());
                }
                if mcp.get("startup_readiness").is_none() {
                    mcp.insert(
                        "startup_readiness",
                        defaults["mcp_servers"]["hanzo"]["startup_readiness"].clone(),
                    );
                }
                if mcp.get("default_tools_approval_mode").is_none() {
                    mcp.insert(
                        "default_tools_approval_mode",
                        defaults["mcp_servers"]["hanzo"]["default_tools_approval_mode"].clone(),
                    );
                }
            }
        }
        if config
            .get("skills")
            .and_then(|s| s.get("bundled"))
            .is_none()
        {
            config["skills"]["bundled"] = defaults["skills"]["bundled"].clone();
        }
    });
}


/// Raise an unset `tool_output_token_limit` on existing Hanzo profiles to 25000
/// tokens (~100KB) so tool reads don't fall back to the 10KB (~15-line) cap and
/// stall multi-file exploration. Any explicit user value is preserved.
pub(super) fn upgrade_tool_output_limit(home: &Path) {
    let path = home.join("config.toml");
    let Ok(text) = std::fs::read_to_string(&path) else {
        return;
    };
    let Ok(config) = text.parse::<DocumentMut>() else {
        return;
    };
    if config
        .get("model_provider")
        .and_then(toml_edit::Item::as_str)
        != Some("hanzo")
        || config.get("tool_output_token_limit").is_some()
    {
        return;
    }
    let _ = edit(home, |config| {
        if config.get("tool_output_token_limit").is_none() {
            config["tool_output_token_limit"] = toml_edit::value(25000);
        }
    });
}

#[cfg(test)]
#[path = "provider_tests.rs"]
mod tests;
