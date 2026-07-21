//! `dev agent` — run Hanzo Cloud agents from the terminal over the canonical
//! `/v1/agents` registry. This is the CLOUD-agent surface; it is orthogonal to
//! the local config-defined subprocess agents and shares no state with them.
//!
//! Auth reuses the signed-in hanzo.id session (the same `AuthManager` the cloud
//! subcommand uses): the bearer JWT is sent on every request and the gateway
//! derives the tenant `X-Org-Id` from it. Base URL defaults to the same Hanzo
//! API host the rest of the CLI talks to and is overridable for testing.

use anyhow::Context;
use anyhow::anyhow;
use clap::Parser;
use clap::Subcommand;
use codex_agents_client::AgentsBackend;
use codex_agents_client::HttpClient;
use codex_core::config::Config;
use codex_login::AuthManager;
use codex_login::default_client::get_codex_user_agent;
use codex_utils_cli::CliConfigOverrides;
use std::io::IsTerminal;
use std::io::Read;

const DEFAULT_BASE_URL: &str = "https://api.hanzo.ai";

#[derive(Debug, Parser)]
pub struct AgentCli {
    #[clap(skip)]
    pub config_overrides: CliConfigOverrides,

    #[command(subcommand)]
    pub command: AgentCommand,
}

#[derive(Debug, Subcommand)]
pub enum AgentCommand {
    /// List the org's cloud agents.
    #[clap(visible_alias = "ls")]
    List(ListArgs),

    /// Run a cloud agent against a prompt and print its output.
    Run(RunArgs),

    /// Show a cloud agent's configuration and recent runs.
    Show(ShowArgs),
}

#[derive(Debug, Parser)]
pub struct ListArgs {
    /// Emit the raw JSON response instead of a table.
    #[arg(long)]
    pub json: bool,
}

#[derive(Debug, Parser)]
pub struct RunArgs {
    /// Agent name (org-unique handle).
    pub name: String,

    /// Prompt to send. Reads stdin when omitted or set to `-`.
    pub prompt: Option<String>,

    /// Emit the run as JSON instead of just its output.
    #[arg(long)]
    pub json: bool,
}

#[derive(Debug, Parser)]
pub struct ShowArgs {
    /// Agent name (org-unique handle).
    pub name: String,

    /// Emit the agent detail as JSON.
    #[arg(long)]
    pub json: bool,
}

/// Entry point for the `dev agent` subcommand.
pub async fn run_main(cli: AgentCli) -> anyhow::Result<()> {
    let backend = init_backend(&cli.config_overrides).await?;
    match cli.command {
        AgentCommand::List(args) => run_list(&backend, args).await,
        AgentCommand::Run(args) => run_run(&backend, args).await,
        AgentCommand::Show(args) => run_show(&backend, args).await,
    }
}

async fn init_backend(overrides: &CliConfigOverrides) -> anyhow::Result<HttpClient> {
    let base_url =
        std::env::var("CODEX_AGENTS_BASE_URL").unwrap_or_else(|_| DEFAULT_BASE_URL.to_string());
    // The signed-in hanzo.id bearer JWT is attached to every request, so the
    // base URL is a trust boundary: an attacker who can set CODEX_AGENTS_BASE_URL
    // (poisoned profile, CI env, dotfile) could otherwise redirect the token to
    // a host of their choosing. Restrict it to HTTPS *.hanzo.ai (plus loopback
    // for local testing), mirroring `validate_api_key_remote_host` in main.rs.
    validate_base_url(&base_url)?;

    let parsed_overrides = overrides
        .parse_overrides()
        .map_err(|e| anyhow!("failed to parse -c overrides: {e}"))?;
    let config = Config::load_with_cli_overrides(parsed_overrides)
        .await
        .context("failed to load configuration")?;

    let auth_manager = AuthManager::new(
        config.codex_home.to_path_buf(),
        /*enable_codex_api_key_env*/ false,
        config.cli_auth_credentials_store_mode,
        config.forced_chatgpt_workspace_id.clone(),
        Some(config.chatgpt_base_url.clone()),
        config.auth_keyring_backend_kind(),
        config.auth_route_config(),
    )
    .await;

    let auth = auth_manager
        .auth()
        .await
        .ok_or_else(|| anyhow!("Not signed in. Run `dev login` to sign in, then retry."))?;

    let auth_provider = codex_model_provider::auth_provider_from_auth(&auth);
    Ok(HttpClient::new(base_url)?
        .with_user_agent(get_codex_user_agent())
        .with_auth_provider(auth_provider))
}

/// Restrict the agents API base URL to HTTPS `hanzo.ai` (and its subdomains),
/// or a loopback host for local testing, so the bearer token is only ever sent
/// to trusted hosts. Mirrors `validate_api_key_remote_host` in `main.rs`.
fn validate_base_url(base_url: &str) -> anyhow::Result<()> {
    let url = url::Url::parse(base_url)
        .map_err(|err| anyhow!("invalid CODEX_AGENTS_BASE_URL {base_url:?}: {err}"))?;
    let host = url
        .host()
        .ok_or_else(|| anyhow!("CODEX_AGENTS_BASE_URL must include a host: {base_url:?}"))?;

    let is_loopback = match &host {
        url::Host::Domain(host) => host.eq_ignore_ascii_case("localhost"),
        url::Host::Ipv4(ip) => ip.is_loopback(),
        url::Host::Ipv6(ip) => ip.is_loopback(),
    };
    let is_hanzo_host = match &host {
        url::Host::Domain(host) => {
            let host = host.to_ascii_lowercase();
            host == "hanzo.ai" || host.ends_with(".hanzo.ai")
        }
        _ => false,
    };
    let is_allowed = match url.scheme() {
        "https" => is_loopback || is_hanzo_host,
        "http" => is_loopback,
        _ => false,
    };
    if !is_allowed {
        return Err(anyhow!(
            "CODEX_AGENTS_BASE_URL must be an HTTPS hanzo.ai host (or a loopback host for testing); refusing to send credentials to {base_url:?}"
        ));
    }
    Ok(())
}

async fn run_list(backend: &HttpClient, args: ListArgs) -> anyhow::Result<()> {
    let agents = backend.list_agents().await?;
    if args.json {
        println!("{}", serde_json::to_string_pretty(&agents)?);
        return Ok(());
    }
    if agents.is_empty() {
        println!("No agents found.");
        return Ok(());
    }
    let name_w = agents
        .iter()
        .map(|a| a.name.len())
        .max()
        .unwrap_or(4)
        .max(4);
    let model_w = agents
        .iter()
        .map(|a| a.model.len())
        .max()
        .unwrap_or(5)
        .max(5);
    println!(
        "{:<name_w$}  {:<model_w$}  {:<8}  RUNS",
        "NAME", "MODEL", "STATUS"
    );
    for a in &agents {
        println!(
            "{:<name_w$}  {:<model_w$}  {:<8}  {}",
            a.name, a.model, a.status, a.runs
        );
    }
    Ok(())
}

async fn run_run(backend: &HttpClient, args: RunArgs) -> anyhow::Result<()> {
    let input = resolve_prompt(args.prompt)?;
    let run = backend.run_agent(&args.name, &input).await?;
    if args.json {
        // Emit the full run to stdout regardless of outcome so scripts always
        // get the record, but still exit non-zero for a recorded failure so the
        // exit code matches the human-readable path.
        println!("{}", serde_json::to_string_pretty(&run)?);
    } else if run.is_ok() {
        println!("{}", run.output);
    }
    if !run.is_ok() {
        // The run executed and was recorded as a failure; report it honestly.
        return Err(anyhow!(
            "agent run failed ({}): {}",
            run.status,
            if run.error.is_empty() {
                "<no error message>"
            } else {
                &run.error
            }
        ));
    }
    Ok(())
}

async fn run_show(backend: &HttpClient, args: ShowArgs) -> anyhow::Result<()> {
    let detail = backend.get_agent(&args.name).await?;
    if args.json {
        println!("{}", serde_json::to_string_pretty(&detail)?);
        return Ok(());
    }
    let a = &detail.agent;
    println!("{}  ({})", a.name, a.status);
    println!("model:  {}", a.model);
    if !a.description.is_empty() {
        println!("about:  {}", a.description);
    }
    if !a.tools.is_empty() {
        println!("tools:  {}", a.tools.join(", "));
    }
    if !detail.instructions.is_empty() {
        println!("\ninstructions:\n{}", detail.instructions);
    }
    if !detail.recent_runs.is_empty() {
        println!("\nrecent runs:");
        for r in &detail.recent_runs {
            println!("  [{}] {} ({}ms)", r.status, r.created_at, r.duration_ms);
        }
    }
    Ok(())
}

/// Resolve the run prompt: from the argument, or stdin when omitted or `-`.
fn resolve_prompt(prompt: Option<String>) -> anyhow::Result<String> {
    match prompt {
        Some(p) if p != "-" => Ok(p),
        maybe_dash => {
            let force_stdin = matches!(maybe_dash.as_deref(), Some("-"));
            if std::io::stdin().is_terminal() && !force_stdin {
                return Err(anyhow!(
                    "no prompt provided. Pass one as an argument or pipe it via stdin."
                ));
            }
            let mut buffer = String::new();
            std::io::stdin()
                .read_to_string(&mut buffer)
                .map_err(|e| anyhow!("failed to read prompt from stdin: {e}"))?;
            if buffer.trim().is_empty() {
                return Err(anyhow!(
                    "no prompt provided via stdin (received empty input)."
                ));
            }
            Ok(buffer)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::validate_base_url;

    #[test]
    fn base_url_allows_hanzo_https_and_subdomains() {
        for url in [
            "https://api.hanzo.ai",
            "https://api.hanzo.ai/",
            "https://hanzo.ai",
            "https://cloud.staging.hanzo.ai",
            "https://API.HANZO.AI", // scheme/host are case-insensitive
        ] {
            assert!(validate_base_url(url).is_ok(), "should allow {url}");
        }
    }

    #[test]
    fn base_url_allows_loopback_for_testing() {
        for url in [
            "http://127.0.0.1:8787",
            "https://127.0.0.1:8787",
            "http://localhost:3000",
            "http://[::1]:8080",
        ] {
            assert!(validate_base_url(url).is_ok(), "should allow {url}");
        }
    }

    #[test]
    fn base_url_rejects_credential_exfiltration_vectors() {
        for url in [
            "https://evil.com",
            "http://api.hanzo.ai",              // plaintext to a real host
            "https://hanzo.ai.evil.com",        // suffix-spoof
            "https://evilhanzo.ai",             // no dot boundary
            "https://api.hanzo.ai.attacker.io", // trailing-domain trick
            "https://not-hanzo.ai",             // different apex
            "https://api.hanzo.ai@evil.com",    // userinfo bypass — host is evil.com
            "ftp://api.hanzo.ai",               // wrong scheme
            "file:///etc/passwd",
            "not a url",
        ] {
            assert!(
                validate_base_url(url).is_err(),
                "should reject {url}"
            );
        }
    }
}
