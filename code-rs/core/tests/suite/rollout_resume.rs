use code_core::protocol::{event_msg_to_protocol, AgentMessageEvent, EventMsg, RecordedEvent};
use code_core::rollout::RolloutRecorder;
use code_core::rollout::recorder::RolloutRecorderParams;
use code_protocol::ConversationId;
use code_protocol::protocol::SessionSource;
use code_protocol::models::{ContentItem, ResponseItem};
use tempfile::TempDir;
use uuid::Uuid;

use crate::common::load_default_config_for_test;

#[tokio::test]
async fn resume_restores_recorded_events() {
    let code_home = TempDir::new().expect("tempdir");
    let mut config = load_default_config_for_test(&code_home);
    config.cwd = code_home.path().to_path_buf();

    let conversation_id = ConversationId::from(Uuid::new_v4());
    let recorder = RolloutRecorder::new(
        &config,
        RolloutRecorderParams::new(conversation_id, None, SessionSource::Cli),
    )
        .await
        .expect("create recorder");

    let response_item = ResponseItem::Message {
        id: None,
        role: "assistant".to_string(),
        content: vec![ContentItem::OutputText {
            text: "hello from resume".to_string(),
        }],
    };
    recorder
        .record_response_items(std::slice::from_ref(&response_item))
        .await
        .expect("record response");

    let recorded_event = RecordedEvent {
        id: "turn-1".to_string(),
        event_seq: 1,
        order: None,
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "assistant says hi".to_string(),
        }),
    };
    let protocol_event = code_protocol::protocol::RecordedEvent {
        id: recorded_event.id.clone(),
        event_seq: recorded_event.event_seq,
        order: None,
        msg: event_msg_to_protocol(&recorded_event.msg).expect("convert event"),
    };
    recorder
        .record_events(std::slice::from_ref(&protocol_event))
        .await
        .expect("record event");

    let rollout_path = recorder.rollout_path.clone();
    recorder.shutdown().await.expect("shutdown recorder");

    let (_rec, saved) = RolloutRecorder::resume(&config, &rollout_path)
        .await
        .expect("resume recorder");

    assert_eq!(saved.items, vec![response_item]);
}
