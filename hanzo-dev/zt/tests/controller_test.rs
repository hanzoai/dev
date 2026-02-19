use hanzo_zt::controller::ControllerClient;
use std::time::Duration;

#[test]
fn test_controller_client_creation() {
    let client = ControllerClient::new("https://example.com", Duration::from_secs(10));
    assert!(client.is_ok());
    let client = client.unwrap();
    assert!(!client.is_authenticated());
    assert!(client.session_token().is_none());
}
