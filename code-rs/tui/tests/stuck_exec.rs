use code_core::protocol::{
    Event,
    EventMsg,
    ExecCommandBeginEvent,
    ExecCommandEndEvent,
    OrderMeta,
    PatchApplyBeginEvent,
    PatchApplyEndEvent,
};
use code_tui::test_helpers::{render_chat_widget_to_vt100, ChatWidgetHarness};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

fn next_order_meta(request_ordinal: u64, seq: &mut u64) -> OrderMeta {
    let order = OrderMeta {
        request_ordinal,
        output_index: Some(0),
        sequence_number: Some(*seq),
    };
    *seq += 1;
    order
}

#[test]
fn exec_cell_clears_after_patch_flow() {
    let mut harness = ChatWidgetHarness::new();
    let mut seq = 0_u64;
    let call_id = "call_bug";
    let cwd = PathBuf::from("/tmp");

    harness.handle_event(Event {
        id: "exec-begin".to_string(),
        event_seq: 0,
        msg: EventMsg::ExecCommandBegin(ExecCommandBeginEvent {
            call_id: call_id.to_string(),
            command: vec!["bash".into(), "-lc".into(), "apply_patch".into()],
            cwd: cwd.clone(),
            parsed_cmd: Vec::new(),
        }),
        order: Some(next_order_meta(1, &mut seq)),
    });

    harness.handle_event(Event {
        id: "patch-begin".to_string(),
        event_seq: 0,
        msg: EventMsg::PatchApplyBegin(PatchApplyBeginEvent {
            call_id: call_id.to_string(),
            auto_approved: true,
            changes: HashMap::new(),
        }),
        order: Some(next_order_meta(1, &mut seq)),
    });

    harness.handle_event(Event {
        id: "exec-end".to_string(),
        event_seq: 1,
        msg: EventMsg::ExecCommandEnd(ExecCommandEndEvent {
            call_id: call_id.to_string(),
            stdout: String::new(),
            stderr: String::new(),
            exit_code: 0,
            duration: Duration::from_millis(50),
        }),
        order: Some(next_order_meta(1, &mut seq)),
    });

    harness.handle_event(Event {
        id: "patch-end".to_string(),
        event_seq: 1,
        msg: EventMsg::PatchApplyEnd(PatchApplyEndEvent {
            call_id: call_id.to_string(),
            stdout: "Success".into(),
            stderr: String::new(),
            success: true,
        }),
        order: Some(next_order_meta(1, &mut seq)),
    });

    let output = render_chat_widget_to_vt100(&mut harness, 80, 12);
    assert!(
        !output.contains("Running"),
        "exec cell should not remain running after patch apply:\n{}",
        output
    );
}

#[test]
fn exec_spinner_clears_after_final_answer() {
    use code_core::protocol::AgentMessageEvent;

    let mut harness = ChatWidgetHarness::new();
    let mut seq = 0_u64;
    let call_id = "call_spinner".to_string();
    let cwd = PathBuf::from("/tmp");

    harness.handle_event(Event {
        id: "exec-begin-spinner".to_string(),
        event_seq: 0,
        msg: EventMsg::ExecCommandBegin(ExecCommandBeginEvent {
            call_id: call_id.clone(),
            command: vec!["bash".into(), "-lc".into(), "echo running".into()],
            cwd: cwd.clone(),
            parsed_cmd: Vec::new(),
        }),
        order: Some(next_order_meta(1, &mut seq)),
    });

    harness.handle_event(Event {
        id: "answer-final".to_string(),
        event_seq: 1,
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "All done.".into(),
        }),
        order: Some(next_order_meta(1, &mut seq)),
    });

    let output = render_chat_widget_to_vt100(&mut harness, 80, 12);
    assert!(
        !output.contains("running command"),
        "spinner should clear after final answer, but output was:\n{}",
        output
    );
}
