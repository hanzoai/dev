//! CLI entry point for Hanzo

use clap::{Parser, Subcommand};
use colored::*;
use hanzo::DevClient;

#[derive(Parser)]
#[command(name = "hanzo")]
#[command(about = "Hanzo AI SDK CLI", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Enable debug output
    #[arg(short, long)]
    debug: bool,

    /// Enable request tracing
    #[arg(long)]
    trace: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Send a chat message
    Chat {
        /// The message to send
        message: String,

        /// Model to use
        #[arg(short, long, default_value = "gpt-5")]
        model: String,
    },

    /// Start interactive REPL
    Repl,

    /// Manage API keys
    Auth {
        #[command(subcommand)]
        action: AuthAction,
    },

    /// Run a tool
    Tool {
        /// Tool name
        name: String,

        /// Tool parameters as JSON
        #[arg(short, long)]
        params: Option<String>,
    },

    /// Show version information
    Version,
}

#[derive(Subcommand)]
enum AuthAction {
    /// Set API key
    Set {
        /// API key to set
        key: String,
    },

    /// Show current auth status
    Status,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Setup tracing
    if cli.debug {
        tracing_subscriber::fmt()
            .with_env_filter("hanzo=debug")
            .init();
    }

    match cli.command {
        Some(Commands::Chat { message, model }) => {
            let client = DevClient::new()?
                .with_debug(cli.debug)
                .with_tracing(cli.trace);

            println!("{}", "Sending message...".cyan());
            let response = client.chat(&message).await?;
            println!("\n{}", response.green());
        }

        Some(Commands::Repl) => {
            let client = DevClient::new()?
                .with_debug(cli.debug)
                .with_tracing(cli.trace);

            client.repl().await?;
        }

        Some(Commands::Auth { action }) => {
            match action {
                AuthAction::Set { key } => {
                    // In real impl, would save to config file
                    std::env::set_var("HANZO_API_KEY", key);
                    println!("{}", "✅ API key set successfully".green());
                }
                AuthAction::Status => {
                    if std::env::var("HANZO_API_KEY").is_ok() {
                        println!("{}", "✅ API key is configured".green());
                    } else if std::env::var("OPENAI_API_KEY").is_ok() {
                        println!("{}", "✅ Using OpenAI API key".yellow());
                    } else {
                        println!("{}", "❌ No API key configured".red());
                    }
                }
            }
        }

        Some(Commands::Tool { name, params }) => {
            println!("Tool execution not yet implemented: {}", name);
            if let Some(p) = params {
                println!("Parameters: {}", p);
            }
        }

        Some(Commands::Version) => {
            println!("Hanzo AI SDK v{}", env!("CARGO_PKG_VERSION"));
            println!("Rust SDK matching Python SDK architecture");
        }

        None => {
            // Start REPL by default
            let client = DevClient::new()?
                .with_debug(cli.debug)
                .with_tracing(cli.trace);

            client.repl().await?;
        }
    }

    Ok(())
}