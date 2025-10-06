use crate::protocol::AgentMessageEvent;
use crate::protocol::AgentReasoningEvent;
use crate::protocol::AgentReasoningRawContentEvent;
use crate::protocol::EventMsg;
use crate::protocol::WebSearchCompleteEvent;
use code_protocol::models::ContentItem;
use code_protocol::models::ReasoningItemContent;
use code_protocol::models::ReasoningItemReasoningSummary;
use code_protocol::models::ResponseItem;
use code_protocol::models::WebSearchAction;

/// Convert a `ResponseItem` into zero or more `EventMsg` values that the UI can render.
///
/// When `show_raw_agent_reasoning` is false, raw reasoning content events are omitted.
#[allow(dead_code)]
pub(crate) fn map_response_item_to_event_messages(
    item: &ResponseItem,
    show_raw_agent_reasoning: bool,
) -> Vec<EventMsg> {
    match item {
        ResponseItem::Message { role, content, .. } => {
            // Do not surface system messages as user events.
            if role == "system" {
                return Vec::new();
            }

            let mut events: Vec<EventMsg> = Vec::new();

            for content_item in content.iter() {
                match content_item {
                    ContentItem::InputText { .. } => {}
                    ContentItem::InputImage { .. } => {}
                    ContentItem::OutputText { text } => {
                        events.push(EventMsg::AgentMessage(AgentMessageEvent {
                            message: text.clone(),
                        }));
                    }
                }
            }

            events
        }

        ResponseItem::Reasoning {
            summary, content, ..
        } => {
            let mut events = Vec::new();
            for ReasoningItemReasoningSummary::SummaryText { text } in summary {
                events.push(EventMsg::AgentReasoning(AgentReasoningEvent {
                    text: text.clone(),
                }));
            }
            if let Some(items) = content.as_ref().filter(|_| show_raw_agent_reasoning) {
                for c in items {
                    let text = match c {
                        ReasoningItemContent::ReasoningText { text }
                        | ReasoningItemContent::Text { text } => text,
                    };
                    events.push(EventMsg::AgentReasoningRawContent(
                        AgentReasoningRawContentEvent { text: text.clone() },
                    ));
                }
            }
            events
        }

        ResponseItem::WebSearchCall { id, action, .. } => match action {
            WebSearchAction::Search { query } => {
                let call_id = id.clone().unwrap_or_else(|| "".to_string());
                vec![EventMsg::WebSearchComplete(WebSearchCompleteEvent {
                    call_id,
                    query: Some(query.clone()),
                })]
            }
            WebSearchAction::Other => Vec::new(),
        },

        // Variants that require side effects are handled by higher layers and do not emit events here.
        ResponseItem::FunctionCall { .. }
        | ResponseItem::FunctionCallOutput { .. }
        | ResponseItem::LocalShellCall { .. }
        | ResponseItem::CustomToolCall { .. }
        | ResponseItem::CustomToolCallOutput { .. }
        | ResponseItem::Other => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::map_response_item_to_event_messages;
    use code_protocol::models::ContentItem;
    use code_protocol::models::ResponseItem;

    #[test]
    fn maps_user_message_with_text_and_two_images() {
        let img1 = "https://example.com/one.png".to_string();
        let img2 = "https://example.com/two.jpg".to_string();

        let item = ResponseItem::Message {
            id: None,
            role: "user".to_string(),
            content: vec![
                ContentItem::InputText {
                    text: "Hello world".to_string(),
                },
                ContentItem::InputImage {
                    image_url: img1.clone(),
                },
                ContentItem::InputImage {
                    image_url: img2.clone(),
                },
            ],
        };

        let events = map_response_item_to_event_messages(&item, false);
        // No UI event is emitted for raw user input in this fork
        assert!(events.is_empty());
    }
}
