use super::*;

#[test]
fn new_home_loads_hanzo_provider_and_native_mcp() {
    let directory = tempfile::tempdir().unwrap();
    initialize_home(directory.path()).unwrap();
    let config: toml::Value = toml::from_str(&std::fs::read_to_string(directory.path().join("config.toml"))
        .unwrap()).unwrap();
    assert_eq!(config["model_provider"].as_str(), Some("hanzo"));
    assert_eq!(config["model_providers"]["hanzo"]["base_url"].as_str(), Some(API_BASE));
    assert_eq!(config["mcp_servers"]["hanzo"]["command"].as_str(), Some("hanzo-mcp"));
    assert!(config["mcp_servers"]["hanzo"]["args"].as_array().unwrap().is_empty());
}

#[test]
fn existing_configuration_is_preserved_byte_for_byte() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("config.toml");
    let custom = "# My configuration\nmodel = 'custom'\n";
    std::fs::write(&path, custom).unwrap();
    initialize_home(directory.path()).unwrap();
    assert_eq!(std::fs::read_to_string(path).unwrap(), custom);
}

#[test]
fn concurrent_startup_publishes_a_complete_config() {
    let directory = tempfile::tempdir().unwrap();
    std::thread::scope(|scope| {
        for _ in 0..8 {
            let home = directory.path();
            scope.spawn(move || {
                initialize_home(home).unwrap();
                assert_eq!(std::fs::read_to_string(home.join("config.toml")).unwrap(), DEFAULT_CONFIG);
            });
        }
    });
}

#[test]
fn profile_resolution_honors_override_and_requires_a_home() {
    assert_eq!(product_home(Some("custom".into()), Some("user".into())).unwrap(), PathBuf::from("custom"));
    assert_eq!(product_home(None, Some("user".into())).unwrap(), PathBuf::from("user/.hanzo/dev2"));
    assert!(product_home(None, None).is_err());
}
