use hanzo_zt::config::ConfigBuilder;
use hanzo_zt::auth::HanzoJwtCredentials;

#[test]
fn test_config_builder_requires_controller_url() {
    let creds = HanzoJwtCredentials::from_token("test");
    let result = ConfigBuilder::new()
        .credentials(creds)
        .build();
    assert!(result.is_err());
}

#[test]
fn test_config_builder_requires_credentials() {
    let result = ConfigBuilder::new()
        .controller_url("https://example.com")
        .build();
    assert!(result.is_err());
}

#[test]
fn test_config_builder_success() {
    let creds = HanzoJwtCredentials::from_token("test-token");
    let config = ConfigBuilder::new()
        .controller_url("https://zt-api.hanzo.ai")
        .credentials(creds)
        .billing(false)
        .build();
    assert!(config.is_ok());
    let config = config.expect("config should build");
    assert_eq!(config.controller_url, "https://zt-api.hanzo.ai");
    assert!(!config.billing_enabled);
}
