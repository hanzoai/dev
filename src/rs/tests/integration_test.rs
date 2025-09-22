/// Integration tests for Hanzo Dev Rust components
use hanzo_dev::config::Config;
use hanzo_dev::ConversationManager;
use dev_protocol::models::ResponseItem;

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[tokio::test]
    async fn test_conversation_manager_creation() {
        let config = Config::default();
        let manager = ConversationManager::new(config);
        assert!(manager.is_ok(), "Should create conversation manager");
    }

    #[test]
    fn test_config_loading() {
        let config = Config::default();
        assert_eq!(config.models.len(), 0, "Default config should have no models");
    }

    #[test]
    fn test_response_item_creation() {
        let item = ResponseItem::Message {
            id: Some("test-123".to_string()),
            role: "user".to_string(),
            content: vec![],
        };

        match item {
            ResponseItem::Message { id, role, .. } => {
                assert_eq!(id, Some("test-123".to_string()));
                assert_eq!(role, "user");
            }
            _ => panic!("Wrong response item type"),
        }
    }
}