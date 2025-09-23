//! Hanzo CLI - Container runtime manager

use anyhow::Result;
use clap::Parser;
use hanzod::cli::{Cli, Commands, handle_run, handle_ps, handle_logs, handle_stop, handle_exec, handle_pull};

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run {
            image,
            command,
            detach,
            port,
            env,
            volume
        } => {
            handle_run(&image, command, detach, port, env, volume).await?;
        }
        Commands::Ps { all } => {
            handle_ps(all).await?;
        }
        Commands::Stop { id } => {
            handle_stop(&id).await?;
        }
        Commands::Logs { id, follow } => {
            handle_logs(&id, follow).await?;
        }
        Commands::Exec { id, command } => {
            handle_exec(&id, command).await?;
        }
        Commands::Pull { image } => {
            handle_pull(&image).await?;
        }
        Commands::Runtimes => {
            show_runtimes().await?;
        }
        Commands::Daemon { port } => {
            println!("Starting hanzod daemon on port {}...", port);
            std::process::Command::new("hanzod")
                .env("PORT", port.to_string())
                .spawn()?
                .wait()?;
        }
    }

    Ok(())
}

async fn show_runtimes() -> Result<()> {
    let client = reqwest::Client::new();
    let response = client
        .get("http://localhost:3690/v1/runtimes")
        .send()
        .await?;

    if response.status().is_success() {
        let runtimes: serde_json::Value = response.json().await?;
        println!("Available container runtimes:");
        println!("{}", serde_json::to_string_pretty(&runtimes)?);
    } else {
        println!("hanzod is not running. Start it with: hanzo daemon");
    }

    Ok(())
}