//! Interactive sign-in and provider selection for Hanzo Dev.
use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use clap::Parser;
use clap::Subcommand;
use codex_config::LoaderOverrides;
use codex_protocol::config_types::ForcedLoginMethod;
use codex_utils_cli::CliConfigOverrides;
use dialoguer::Password;
use dialoguer::Select;
use hanzo_config::Provider;
use std::io::IsTerminal;
use std::io::Read;

#[derive(Parser)]
pub(crate) struct Login {
    /// Sign in to your Hanzo account without showing the picker.
    #[arg(long, conflicts_with = "chatgpt")]
    hanzo: bool,
    /// Sign in to ChatGPT, or save an OpenAI key with --with-api-key.
    #[arg(long)]
    chatgpt: bool,
    /// Paste a hidden Hanzo API key, or read it from piped stdin.
    /// Combine with --chatgpt for an OpenAI key.
    #[arg(long = "with-api-key", visible_alias = "api-key")]
    api_key: bool,
    /// Use a device code for ChatGPT sign-in.
    #[arg(long, requires = "chatgpt", conflicts_with = "api_key")]
    device_auth: bool,
    #[command(subcommand)]
    command: Option<LoginCommand>,
}

#[derive(Subcommand)]
enum LoginCommand {
    /// Show who is signed in.
    Status,
}

#[derive(Clone, Copy)]
enum Choice {
    Hanzo,
    ChatGpt,
    HanzoKey,
    OpenAiKey,
}

pub(crate) async fn run(login: Login, mut overrides: CliConfigOverrides) -> Result<()> {
    let home = hanzo_config::home();
    if login.command.is_some() {
        let config =
            codex_cli::cloud_config::load_config(&overrides, LoaderOverrides::default()).await?;
        if login.hanzo || (!login.chatgpt && config.model_provider_id == "hanzo") {
            if hanzo_config::hanzo_credential().is_none() {
                bail!("Not signed in to Hanzo. Run `dev login`.");
            }
            println!("Signed in to Hanzo.");
            return Ok(());
        }
        codex_cli::run_login_status(overrides).await;
    }
    let choice = if login.api_key {
        if login.chatgpt {
            Choice::OpenAiKey
        } else {
            Choice::HanzoKey
        }
    } else if login.chatgpt {
        Choice::ChatGpt
    } else if login.hanzo {
        Choice::Hanzo
    } else {
        if !std::io::stdin().is_terminal() || !std::io::stderr().is_terminal() {
            bail!(
                "Choose a sign-in method: `dev login --hanzo`, `dev login --chatgpt`, or pipe a key to `dev login --with-api-key`."
            );
        }
        let choices = [
            Choice::Hanzo,
            Choice::ChatGpt,
            Choice::HanzoKey,
            Choice::OpenAiKey,
        ];
        let selection = Select::new()
            .with_prompt("Sign in to Hanzo Dev")
            .items(&[
                "Hanzo account (recommended)",
                "ChatGPT account",
                "Paste a Hanzo API key",
                "Paste an OpenAI API key",
            ])
            .default(0)
            .interact_opt()?;
        let Some(index) = selection else {
            println!("Sign-in cancelled.");
            return Ok(());
        };
        choices[index]
    };
    match choice {
        Choice::Hanzo => {
            let status = tokio::process::Command::new("hanzo")
                .args(["auth", "login", "--provider", "hanzo"])
                .status()
                .await
                .context("Could not start Hanzo sign-in. Install the Hanzo CLI with `curl -fsSL https://hanzo.sh | sh`, or choose an API key")?;
            if !status.success() {
                bail!("Hanzo sign-in did not complete ({status}).");
            }
            hanzo_config::activate_provider(&home, Provider::Hanzo)?;
            hanzo_config::clear_hanzo_api_key(&home)?;
            println!("Signed in to Hanzo.");
        }
        Choice::HanzoKey => {
            let key = read_key("Hanzo")?;
            hanzo_config::save_hanzo_api_key(&home, &key)?;
            hanzo_config::activate_provider(&home, Provider::Hanzo)?;
            println!("Hanzo API key saved.");
        }
        Choice::ChatGpt | Choice::OpenAiKey => {
            overrides
                .raw_overrides
                .push("model_provider=\"openai\"".into());
            let config =
                codex_cli::cloud_config::load_config(&overrides, LoaderOverrides::default())
                    .await?;
            let method = match choice {
                Choice::OpenAiKey => ForcedLoginMethod::Api,
                _ => ForcedLoginMethod::Chatgpt,
            };
            if !config.auth_config().is_login_method_allowed(method) {
                bail!("This sign-in method is disabled by your configuration.");
            }
            match choice {
                Choice::OpenAiKey => {
                    let key = read_key("OpenAI")?;
                    codex_login::login_with_api_key(
                        &config.codex_home,
                        &key,
                        config.cli_auth_credentials_store_mode,
                        config.auth_keyring_backend_kind(),
                    )?;
                }
                _ => {
                    let options = codex_login::ServerOptions::new(
                        config.codex_home.to_path_buf(),
                        codex_login::CLIENT_ID.to_owned(),
                        config.auth_config().effective_chatgpt_workspaces(),
                        config.cli_auth_credentials_store_mode,
                        config.auth_keyring_backend_kind(),
                        config.auth_route_config(),
                    );
                    if login.device_auth {
                        codex_login::run_device_code_login(options).await?;
                    } else {
                        let server = codex_login::run_login_server(options)?;
                        eprintln!(
                            "Continue in your browser to sign in to ChatGPT:\n{}",
                            server.auth_url
                        );
                        server.block_until_done().await?;
                    }
                }
            }
            hanzo_config::activate_provider(&home, Provider::OpenAi)?;
            println!(
                "{}",
                if matches!(choice, Choice::OpenAiKey) {
                    "OpenAI API key saved."
                } else {
                    "Signed in to ChatGPT."
                }
            );
        }
    }
    Ok(())
}

pub(crate) async fn logout(overrides: CliConfigOverrides) -> Result<()> {
    // Sign out of the provider that is signed in, and only that one. Signing out
    // of ChatGPT must not delete a Hanzo key the user pasted weeks ago: the two
    // credentials are separate accounts, and destroying the one nobody asked
    // about is a surprise nothing undoes.
    //
    // The Hanzo side goes FIRST. `run_logout` never returns — it exits the
    // process — so anything sequenced after it is unreachable, and that is how a
    // logout came to print success while leaving the credential readable.
    let home = hanzo_config::home();
    let config =
        codex_cli::cloud_config::load_config(&overrides, LoaderOverrides::default()).await?;
    if config.model_provider_id == "hanzo" {
        if home.join("hanzo-api-key").exists() {
            hanzo_config::clear_hanzo_api_key(&home)?;
            println!("Removed the saved Hanzo API key.");
        } else {
            match tokio::process::Command::new("hanzo")
                .args(["auth", "logout"])
                .status()
                .await
            {
                Ok(status) if status.success() => {}
                // Warned, not fatal: the account credential below still has to go.
                Ok(status) => eprintln!("`hanzo auth logout` did not complete ({status})."),
                Err(error) => eprintln!("Could not run `hanzo auth logout`: {error}"),
            }
        }
    }
    if std::env::var_os("HANZO_USER_KEY").is_some() {
        eprintln!("HANZO_USER_KEY is still set in your shell; unset it to stop using that key.");
    }
    // Deletes auth.json — the credential every provider reads, and the one the
    // Hanzo tools share — then exits.
    codex_cli::run_logout(overrides).await
}

fn read_key(provider: &str) -> Result<String> {
    let key = if std::io::stdin().is_terminal() {
        Password::new()
            .with_prompt(format!("Paste your {provider} API key (hidden)"))
            .interact()?
    } else {
        let mut input = String::new();
        std::io::stdin()
            .lock()
            .take(16_385)
            .read_to_string(&mut input)?;
        input
    };
    let key = key.trim();
    if key.is_empty()
        || key.len() > 16_384
        || key.chars().any(|c| c.is_whitespace() || c.is_control())
    {
        bail!(
            "Enter one nonempty API key, with no spaces or control characters (maximum 16384 bytes)."
        );
    }
    let hanzo_key = ["hk-", "pk-live-", "pk-test-", "sk-live-", "sk-test-"]
        .iter()
        .any(|prefix| key.starts_with(prefix));
    if key.starts_with("sk-ant-")
        || (provider == "OpenAI" && hanzo_key)
        || (provider == "Hanzo" && (key.starts_with("sk-proj-") || key.starts_with("sk-org-")))
    {
        bail!(
            "That key belongs to a different provider. Run `dev login` and choose the matching API-key option."
        );
    }
    Ok(key.to_owned())
}
