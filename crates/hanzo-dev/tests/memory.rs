//! Background learning must never borrow the foreground's paid provider.
use codex_core::config::ConfigBuilder;
use codex_features::Feature;

#[tokio::test]
async fn native_memory_defaults_use_an_independent_local_provider() {
    let home = tempfile::tempdir().unwrap();
    std::fs::write(
        home.path().join("config.toml"),
        hanzo_config::DEFAULT_CONFIG,
    )
    .unwrap();
    let config = ConfigBuilder::default()
        .codex_home(home.path().to_path_buf())
        .without_project_context()
        .build()
        .await
        .unwrap();
    assert!(config.features.enabled(Feature::MemoryTool));
    assert_eq!(config.model_provider_id, "hanzo");
    assert_eq!(
        config.memories.model_provider.as_deref(),
        Some("local_memory")
    );
    let local = &config.model_providers["local_memory"];
    assert_eq!(local.base_url.as_deref(), Some("http://127.0.0.1:8731/v1"));
    assert!(!local.requires_openai_auth);
    assert!(config.memories.extract_model.is_some());
    assert!(config.memories.consolidation_model.is_some());
}
