use hanzo_zt::auth::{ApiKeyCredentials, Credentials, HanzoJwtCredentials};

#[test]
fn test_hanzo_jwt_from_token() {
    let creds = HanzoJwtCredentials::from_token("test-token-123");
    assert_eq!(creds.auth_method(), "ext-jwt");
    assert!(creds.display().contains("...n-123"));
}

#[test]
fn test_api_key_credentials() {
    let creds = ApiKeyCredentials::new("my-secret-api-key");
    assert_eq!(creds.auth_method(), "password");
    assert!(creds.display().contains("...i-key"));
}

#[test]
fn test_hanzo_jwt_resolve_env() {
    // SAFETY: This test must run single-threaded to avoid races on env vars.
    unsafe {
        std::env::set_var("HANZO_API_KEY", "test-env-key-12345");
    }
    let creds = HanzoJwtCredentials::resolve();
    assert!(creds.is_ok());
    let creds = creds.expect("credentials should resolve");
    assert_eq!(creds.token, "test-env-key-12345");
    unsafe {
        std::env::remove_var("HANZO_API_KEY");
    }
}

#[test]
fn test_auth_payload_format() {
    let creds = HanzoJwtCredentials::from_token("jwt-token");
    let payload = creds.auth_payload().expect("payload should succeed");
    assert!(payload.get("configTypes").is_some());
}
