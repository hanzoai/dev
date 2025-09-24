//! Policy for what events should be persisted to rollout logs.

use crate::protocol::EventMessage;
use codex_protocol::protocol::RolloutItem;
use codex_protocol::models::ResponseItem;

/// Determines whether an event message should be persisted to rollout logs.
/// 
/// This function filters out events that are not useful for rollout replay
/// or debugging purposes.
pub fn should_persist_event_msg(msg: &EventMessage) -> bool {
    match msg {
        // Always persist user messages and assistant responses
        EventMessage::UserMessage(_) => true,
        EventMessage::AssistantMessage(_) => true,
        
        // Persist command executions and results
        EventMessage::CommandExecution(_) => true,
        EventMessage::CommandResult(_) => true,
        
        // Persist tool calls and results
        EventMessage::ToolCall(_) => true,
        EventMessage::ToolResult(_) => true,
        
        // Persist file operations
        EventMessage::FileOperation(_) => true,
        
        // Persist errors and warnings
        EventMessage::Error(_) => true,
        EventMessage::Warning(_) => true,
        
        // Skip status updates and heartbeats
        EventMessage::StatusUpdate(_) => false,
        EventMessage::Heartbeat => false,
        
        // Skip rate limit notifications
        EventMessage::RateLimitUpdate(_) => false,
        
        // For any other message types, be conservative and persist
        _ => true,
    }
}

/// Determines whether a response item should be persisted to rollout logs.
pub fn should_persist_response_item(item: &ResponseItem) -> bool {
    // Always persist response items - they contain the actual model responses
    // which are essential for rollout replay and debugging
    true
}

/// Determines whether a rollout item should be persisted to rollout logs.
pub fn should_persist_rollout_item(item: &RolloutItem) -> bool {
    // Always persist rollout items by default - they represent the core
    // conversation flow that needs to be reproducible
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::protocol::*;

    #[test]
    fn test_should_persist_important_events() {
        // User messages should be persisted
        let user_msg = EventMessage::UserMessage(UserMessage {
            content: "test".to_string(),
            timestamp: chrono::Utc::now(),
        });
        assert!(should_persist_event_msg(&user_msg));
        
        // Command executions should be persisted
        let cmd_msg = EventMessage::CommandExecution(CommandExecution {
            command: "ls -la".to_string(),
            working_dir: "/tmp".to_string(),
            timestamp: chrono::Utc::now(),
        });
        assert!(should_persist_event_msg(&cmd_msg));
    }

    #[test]
    fn test_should_not_persist_noise() {
        // Heartbeats should not be persisted
        assert!(!should_persist_event_msg(&EventMessage::Heartbeat));
        
        // Rate limit updates should not be persisted
        let rate_limit_msg = EventMessage::RateLimitUpdate(RateLimitUpdate {
            requests_remaining: 100,
            reset_time: chrono::Utc::now(),
        });
        assert!(!should_persist_event_msg(&rate_limit_msg));
    }
}