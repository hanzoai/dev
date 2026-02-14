use hanzo_app_server_protocol::AuthMode;
use hanzo_common::CliConfigOverrides;
use hanzo_core::CodexAuth;
use hanzo_core::auth::CLIENT_ID;
use hanzo_core::auth::HANZO_API_KEY_ENV_VAR;
use hanzo_core::auth::OPENAI_API_KEY_ENV_VAR;
use hanzo_core::auth::login_with_api_key;
use hanzo_core::auth::logout;
use hanzo_core::config::Config;
use hanzo_core::config::ConfigOverrides;
use hanzo_login::ServerOptions;
use hanzo_login::run_device_code_login;
use hanzo_login::run_login_server;
use std::env;
use std::io::IsTerminal;
use std::io::Read;
use std::path::PathBuf;

pub async fn login_with_oauth(
    code_home: PathBuf,
    originator: String,
    issuer: Option<String>,
    client_id: Option<String>,
) -> std::io::Result<()> {
    let mut opts = ServerOptions::new(
        code_home,
        client_id.unwrap_or_else(|| CLIENT_ID.to_string()),
        originator,
    );
    if let Some(iss) = issuer {
        opts.issuer = iss;
    }
    let server = run_login_server(opts)?;

    eprintln!("Your browser has been opened to visit:\n");
    eprintln!("    {}\n", server.auth_url);

    server.block_until_done().await
}

/// Browser-based OAuth login (default flow). Defaults to hanzo.id; use
/// `--chatgpt` to authenticate via auth.openai.com instead.
pub async fn run_login_with_oauth(
    cli_config_overrides: CliConfigOverrides,
    issuer: Option<String>,
    client_id: Option<String>,
) -> ! {
    let config = load_config_or_exit(cli_config_overrides);
    let is_chatgpt = issuer
        .as_deref()
        .is_some_and(|iss| iss.contains("openai.com"));

    match login_with_oauth(
        config.code_home.clone(),
        config.responses_originator_header.clone(),
        issuer,
        client_id,
    )
    .await
    {
        Ok(_) => {
            let email = get_logged_in_email(&config.code_home, &config.responses_originator_header);
            let provider = if is_chatgpt { "ChatGPT" } else { "Hanzo" };
            if let Some(email) = email {
                eprintln!("\nLogged in to {provider} as [{email}].");
            } else {
                eprintln!("\nLogged in to {provider}.");
            }
            eprintln!("Your credentials have been saved to: ~/.hanzo/auth.json");
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("Error logging in: {e}");
            std::process::exit(1);
        }
    }
}

pub async fn run_login_with_api_key(
    cli_config_overrides: CliConfigOverrides,
    api_key: String,
) -> ! {
    let config = load_config_or_exit(cli_config_overrides);

    match login_with_api_key(&config.code_home, &api_key) {
        Ok(_) => {
            eprintln!(
                "You are now logged in with API key {}.",
                safe_format_key(&api_key)
            );
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("Error logging in: {e}");
            std::process::exit(1);
        }
    }
}

pub fn read_api_key_from_stdin() -> String {
    let mut stdin = std::io::stdin();

    if stdin.is_terminal() {
        eprintln!(
            "--with-api-key expects the API key on stdin. Try piping it, e.g. `printenv HANZO_API_KEY | hanzo login --with-api-key`."
        );
        std::process::exit(1);
    }

    eprintln!("Reading API key from stdin...");

    let mut buffer = String::new();
    if let Err(err) = stdin.read_to_string(&mut buffer) {
        eprintln!("Failed to read API key from stdin: {err}");
        std::process::exit(1);
    }

    let api_key = buffer.trim().to_string();
    if api_key.is_empty() {
        eprintln!("No API key provided via stdin.");
        std::process::exit(1);
    }

    api_key
}

/// Login using the OAuth device code flow.
pub async fn run_login_with_device_code(
    cli_config_overrides: CliConfigOverrides,
    issuer_base_url: Option<String>,
    client_id: Option<String>,
) -> ! {
    let config = load_config_or_exit(cli_config_overrides);
    let mut opts = ServerOptions::new(
        config.code_home.clone(),
        client_id.unwrap_or_else(|| CLIENT_ID.to_string()),
        config.responses_originator_header.clone(),
    );
    if let Some(iss) = issuer_base_url {
        opts.issuer = iss;
    }
    match run_device_code_login(opts).await {
        Ok(()) => {
            let email = get_logged_in_email(&config.code_home, &config.responses_originator_header);
            if let Some(email) = email {
                eprintln!("\nYou are now logged in as [{email}].");
            } else {
                eprintln!("\nYou are now logged in.");
            }
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("Error logging in with device code: {e}");
            std::process::exit(1);
        }
    }
}

pub async fn run_login_status(cli_config_overrides: CliConfigOverrides) -> ! {
    let config = load_config_or_exit(cli_config_overrides);

    match CodexAuth::from_code_home(
        &config.code_home,
        AuthMode::ApiKey,
        &config.responses_originator_header,
    ) {
        Ok(Some(auth)) => match auth.mode {
            AuthMode::ApiKey => match auth.get_token().await {
                Ok(api_key) => {
                    eprintln!("Logged in with API key: {}", safe_format_key(&api_key));

                    if let Ok(env_api_key) = env::var(OPENAI_API_KEY_ENV_VAR)
                        && env_api_key == api_key
                    {
                        eprintln!("  (from OPENAI_API_KEY environment variable)");
                    } else if let Ok(env_api_key) = env::var(HANZO_API_KEY_ENV_VAR)
                        && env_api_key == api_key
                    {
                        eprintln!("  (from HANZO_API_KEY environment variable)");
                    }
                    std::process::exit(0);
                }
                Err(e) => {
                    eprintln!("Unexpected error retrieving API key: {e}");
                    std::process::exit(1);
                }
            },
            AuthMode::ChatGPT => {
                let email =
                    get_logged_in_email(&config.code_home, &config.responses_originator_header);
                if let Some(email) = email {
                    eprintln!("Logged in as [{email}]");
                } else {
                    eprintln!("Logged in via OAuth");
                }
                std::process::exit(0);
            }
            AuthMode::Hanzo => {
                let email =
                    get_logged_in_email(&config.code_home, &config.responses_originator_header);
                if let Some(email) = email {
                    eprintln!("Logged in to Hanzo as [{email}]");
                } else {
                    eprintln!("Logged in to Hanzo via OAuth");
                }
                std::process::exit(0);
            }
        },
        Ok(None) => {
            eprintln!("Not logged in. Run `hanzo login` to authenticate.");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("Error checking login status: {e}");
            std::process::exit(1);
        }
    }
}

pub async fn run_logout(cli_config_overrides: CliConfigOverrides) -> ! {
    let config = load_config_or_exit(cli_config_overrides);

    match logout(&config.code_home) {
        Ok(true) => {
            eprintln!("Logged out successfully.");
            std::process::exit(0);
        }
        Ok(false) => {
            eprintln!("Not logged in.");
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("Error logging out: {e}");
            std::process::exit(1);
        }
    }
}

fn load_config_or_exit(cli_config_overrides: CliConfigOverrides) -> Config {
    let cli_overrides = match cli_config_overrides.parse_overrides() {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error parsing -c overrides: {e}");
            std::process::exit(1);
        }
    };

    let config_overrides = ConfigOverrides::default();
    match Config::load_with_cli_overrides(cli_overrides, config_overrides) {
        Ok(config) => config,
        Err(e) => {
            eprintln!("Error loading configuration: {e}");
            std::process::exit(1);
        }
    }
}

/// Try to extract the logged-in user's email from the stored auth.
fn get_logged_in_email(code_home: &std::path::Path, originator: &str) -> Option<String> {
    let auth = CodexAuth::from_code_home(code_home, AuthMode::ChatGPT, originator)
        .ok()
        .flatten()?;
    // Try to get token data which contains the parsed id_token with email
    let rt = tokio::runtime::Handle::try_current().ok()?;
    let token_data = std::thread::spawn(move || rt.block_on(auth.get_token_data()))
        .join()
        .ok()?
        .ok()?;
    token_data.id_token.email
}

fn safe_format_key(key: &str) -> String {
    if key.len() <= 13 {
        return "***".to_string();
    }
    let prefix = &key[..8];
    let suffix = &key[key.len() - 5..];
    format!("{prefix}***{suffix}")
}

#[cfg(test)]
mod tests {
    use super::safe_format_key;

    #[test]
    fn formats_long_key() {
        let key = "sk-proj-1234567890ABCDE";
        assert_eq!(safe_format_key(key), "sk-proj-***ABCDE");
    }

    #[test]
    fn short_key_returns_stars() {
        let key = "sk-proj-12345";
        assert_eq!(safe_format_key(key), "***");
    }
}
