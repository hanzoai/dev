use anyhow::Result;
use clap::Subcommand;
use codex_config::LoaderOverrides;
use codex_features::Feature;
use codex_utils_cli::CliConfigOverrides;

#[derive(Subcommand)]
pub enum MemoryCommand {
    /// Show effective memory settings without making a model call.
    Status,
    /// Enable learning on an independent local Responses-compatible engine.
    Enable {
        #[arg(long, default_value = "http://127.0.0.1:8731/v1")]
        base_url: String,
        #[arg(long, default_value = "halogen-qwen3.8-flash-next")]
        model: String,
    },
    /// Stop automatic memory extraction and retrieval for this profile.
    Disable,
}

pub async fn run(command: MemoryCommand, overrides: CliConfigOverrides) -> Result<()> {
    match command {
        MemoryCommand::Enable { base_url, model } => {
            hanzo_config::set_memory_provider(&hanzo_config::home(), &base_url, &model)?;
            println!("Native memory enabled on an independent provider. Learning starts on eligible session activity; no foreground-provider fallback.");
        }
        MemoryCommand::Disable => {
            hanzo_config::set_feature(&hanzo_config::home(), "memories", false)?;
            println!("Native memory disabled.");
        }
        MemoryCommand::Status => {
            let config = codex_cli::cloud_config::load_config(&overrides, LoaderOverrides::default()).await?;
            println!("memory: {}", if config.features.enabled(Feature::MemoryTool) { "enabled" } else { "disabled" });
            println!("background provider: {}", config.memories.model_provider.as_deref().unwrap_or("foreground"));
            println!("extract model: {}", config.memories.extract_model.as_deref().unwrap_or("provider default"));
            println!("consolidation model: {}", config.memories.consolidation_model.as_deref().unwrap_or("provider default"));
        }
    }
    Ok(())
}
