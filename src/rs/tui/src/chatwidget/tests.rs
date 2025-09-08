#![allow(clippy::unwrap_used, clippy::expect_used, unnameable_test_items)]

use super::*;
use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use dev_core::config::Config;
use dev_core::config::ConfigOverrides;
use dev_core::config::ConfigToml;
use dev_core::plan_tool::PlanItemArg;
use dev_core::plan_tool::StepStatus;
use dev_core::plan_tool::UpdatePlanArgs;
use dev_core::protocol::AgentMessageDeltaEvent;
use dev_core::protocol::AgentMessageEvent;
use dev_core::protocol::AgentReasoningDeltaEvent;
use dev_core::protocol::AgentReasoningEvent;
use dev_core::protocol::ApplyPatchApprovalRequestEvent;
use dev_core::protocol::Event;
use dev_core::protocol::EventMsg;
use dev_core::protocol::ExecApprovalRequestEvent;
use dev_core::protocol::ExecCommandBeginEvent;
use dev_core::protocol::ExecCommandEndEvent;
use dev_core::protocol::FileChange;
use dev_core::protocol::PatchApplyBeginEvent;
use dev_core::protocol::PatchApplyEndEvent;
use dev_core::protocol::StreamErrorEvent;
use dev_core::protocol::TaskCompleteEvent;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyModifiers;
use insta::assert_snapshot;
use pretty_assertions::assert_eq;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::path::PathBuf;
use std::sync::mpsc::channel;
use tokio::sync::mpsc::unbounded_channel;

fn test_config() -> Config {
    // Use base defaults to avoid depending on host state.
    dev_core::config::Config::load_from_base_config_with_overrides(
        ConfigToml::default(),
        ConfigOverrides::default(),
        std::env::temp_dir(),
    )
    .expect("config")
}

#[test]
fn final_answer_without_newline_is_flushed_immediately() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Set up a VT100 test terminal to capture ANSI visual output
    let width: u16 = 80;
    let height: u16 = 2000;
    let viewport = ratatui::layout::Rect::new(0, height - 1, width, 1);
    let backend = ratatui::backend::TestBackend::new(width, height);
    let mut terminal = crate::custom_terminal::Terminal::with_options(backend)
        .expect("failed to construct terminal");
    terminal.set_viewport_area(viewport);

    // Simulate a streaming answer without any newline characters.
    chat.handle_dev_event(Event {
        id: "sub-a".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "Hi! How can I help with codex-rs or anything else today?".into(),
        }),
    });

    // Now simulate the final AgentMessage which should flush the pending line immediately.
    chat.handle_dev_event(Event {
        id: "sub-a".into(),
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "Hi! How can I help with codex-rs or anything else today?".into(),
        }),
    });

    // Drain history insertions and verify the final line is present.
    // We no longer emit a visible "codex" header during streaming.
    let cells = drain_insert_history(&rx);
    let found_final = cells.iter().any(|lines| {
        let s = lines
            .iter()
            .flat_map(|l| l.spans.iter())
            .map(|sp| sp.content.clone())
            .collect::<String>();
        s.contains("Hi! How can I help with codex-rs or anything else today?")
    });
    assert!(
        found_final,
        "expected final answer text to be flushed to history"
    );
}

#[tokio::test(flavor = "current_thread")]
async fn helpers_are_available_and_do_not_panic() {
    let (tx_raw, _rx) = channel::<AppEvent>();
    let tx = AppEventSender::new(tx_raw);
    let cfg = test_config();
    let conversation_manager = Arc::new(ConversationManager::default());
    let mut w = ChatWidget::new(cfg, conversation_manager, tx, None, Vec::new(), false);
    // Basic construction sanity.
    let _ = &mut w;
}

// --- Helpers for tests that need direct construction and event draining ---
fn make_chatwidget_manual() -> (
    ChatWidget<'static>,
    std::sync::mpsc::Receiver<AppEvent>,
    tokio::sync::mpsc::UnboundedReceiver<Op>,
) {
    let (tx_raw, rx) = channel::<AppEvent>();
    let app_event_tx = AppEventSender::new(tx_raw);
    let (op_tx, op_rx) = unbounded_channel::<Op>();
    let cfg = test_config();
    let bottom = BottomPane::new(BottomPaneParams {
        app_event_tx: app_event_tx.clone(),
        has_input_focus: true,
        enhanced_keys_supported: false,
        using_chatgpt_auth: false,
    });
    let widget = ChatWidget {
        app_event_tx,
        dev_op_tx: op_tx,
        bottom_pane: bottom,
        active_exec_cell: None,
        config: cfg.clone(),
        initial_user_message: None,
        total_token_usage: TokenUsage::default(),
        last_token_usage: TokenUsage::default(),
        stream: StreamController::new(cfg),
        last_stream_kind: None,
        running_commands: HashMap::new(),
        pending_exec_completions: Vec::new(),
        task_complete_pending: false,
        interrupts: InterruptManager::new(),
        needs_redraw: false,
    };
    (widget, rx, op_rx)
}

fn drain_insert_history(
    rx: &std::sync::mpsc::Receiver<AppEvent>,
) -> Vec<Vec<ratatui::text::Line<'static>>> {
    let mut out = Vec::new();
    while let Ok(ev) = rx.try_recv() {
        if let AppEvent::InsertHistory(lines) = ev {
            out.push(lines);
        }
    }
    out
}

fn lines_to_single_string(lines: &[ratatui::text::Line<'static>]) -> String {
    let mut s = String::new();
    for line in lines {
        for span in &line.spans {
            s.push_str(&span.content);
        }
        s.push('\n');
    }
    s
}

fn open_fixture(name: &str) -> std::fs::File {
    // 1) Prefer fixtures within this crate
    {
        let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        p.push("tests");
        p.push("fixtures");
        p.push(name);
        if let Ok(f) = File::open(&p) {
            return f;
        }
    }
    // 2) Fallback to parent (workspace root)
    {
        let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        p.push("..");
        p.push(name);
        if let Ok(f) = File::open(&p) {
            return f;
        }
    }
    // 3) Last resort: CWD
    File::open(name).expect("open fixture file")
}

#[test]
fn alt_up_edits_most_recent_queued_message() {
    let (mut chat, _rx, _op_rx) = make_chatwidget_manual();

    // Simulate a running task so messages would normally be queued.
    chat.bottom_pane.set_task_running(true);

    // Seed two queued messages.
    chat.queued_user_messages
        .push_back(UserMessage::from("first queued".to_string()));
    chat.queued_user_messages
        .push_back(UserMessage::from("second queued".to_string()));
    chat.refresh_queued_user_messages();

    // Press Alt+Up to edit the most recent (last) queued message.
    chat.handle_key_event(KeyEvent::new(KeyCode::Up, KeyModifiers::ALT));

    // Composer should now contain the last queued message.
    assert_eq!(
        chat.bottom_pane.composer_text(),
        "second queued".to_string()
    );
    // And the queue should now contain only the remaining (older) item.
    assert_eq!(chat.queued_user_messages.len(), 1);
    assert_eq!(
        chat.queued_user_messages.front().unwrap().text,
        "first queued"
    );
}

#[test]
fn exec_history_cell_shows_working_then_completed() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Begin command
    chat.handle_dev_event(Event {
        id: "call-1".into(),
        msg: EventMsg::ExecCommandBegin(ExecCommandBeginEvent {
            call_id: "call-1".into(),
            command: vec!["bash".into(), "-lc".into(), "echo done".into()],
            cwd: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
            parsed_cmd: vec![dev_core::parse_command::ParsedCommand::Unknown {
                cmd: "echo done".into(),
            }],
        }),
    });

    // End command successfully
    chat.handle_dev_event(Event {
        id: "call-1".into(),
        msg: EventMsg::ExecCommandEnd(ExecCommandEndEvent {
            call_id: "call-1".into(),
            stdout: "done".into(),
            stderr: String::new(),
            aggregated_output: "done".into(),
            exit_code: 0,
            duration: std::time::Duration::from_millis(5),
        }),
    });

    let cells = drain_insert_history(&rx);
    assert_eq!(
        cells.len(),
        1,
        "expected only the completed exec cell to be inserted into history"
    );
    let blob = lines_to_single_string(&cells[0]);
    assert!(
        blob.contains("Completed"),
        "expected completed exec cell to show Completed header: {blob:?}"
    );
}

#[test]
fn exec_history_cell_shows_working_then_failed() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Begin command
    chat.handle_dev_event(Event {
        id: "call-2".into(),
        msg: EventMsg::ExecCommandBegin(ExecCommandBeginEvent {
            call_id: "call-2".into(),
            command: vec!["bash".into(), "-lc".into(), "false".into()],
            cwd: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
            parsed_cmd: vec![dev_core::parse_command::ParsedCommand::Unknown {
                cmd: "false".into(),
            }],
        }),
    });

    // End command with failure
    chat.handle_dev_event(Event {
        id: "call-2".into(),
        msg: EventMsg::ExecCommandEnd(ExecCommandEndEvent {
            call_id: "call-2".into(),
            stdout: String::new(),
            stderr: "error".into(),
            aggregated_output: "error".into(),
            exit_code: 2,
            duration: std::time::Duration::from_millis(7),
        }),
    });

    let cells = drain_insert_history(&rx);
    assert_eq!(
        cells.len(),
        1,
        "expected only the completed exec cell to be inserted into history"
    );
    let blob = lines_to_single_string(&cells[0]);
    assert!(
        blob.contains("Failed (exit 2)"),
        "expected completed exec cell to show Failed header with exit code: {blob:?}"
    );
}

#[tokio::test(flavor = "current_thread")]
async fn binary_size_transcript_matches_ideal_fixture() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Set up a VT100 test terminal to capture ANSI visual output
    let width: u16 = 80;
    let height: u16 = 2000;
    let viewport = ratatui::layout::Rect::new(0, height - 1, width, 1);
    let backend = ratatui::backend::TestBackend::new(width, height);
    let mut terminal = crate::custom_terminal::Terminal::with_options(backend)
        .expect("failed to construct terminal");
    terminal.set_viewport_area(viewport);

    // Replay the recorded session into the widget and collect transcript
    let file = open_fixture("binary-size-log.jsonl");
    let reader = BufReader::new(file);
    let mut transcript = String::new();
    let mut ansi: Vec<u8> = Vec::new();

    for line in reader.lines() {
        let line = line.expect("read line");
        if line.trim().is_empty() || line.starts_with('#') {
            continue;
        }
        let Ok(v): Result<serde_json::Value, _> = serde_json::from_str(&line) else {
            continue;
        };
        let Some(dir) = v.get("dir").and_then(|d| d.as_str()) else {
            continue;
        };
        if dir != "to_tui" {
            continue;
        }
        let Some(kind) = v.get("kind").and_then(|k| k.as_str()) else {
            continue;
        };

        match kind {
            "dev_event" => {
                if let Some(payload) = v.get("payload") {
                    let ev: Event = serde_json::from_value(payload.clone()).expect("parse");
                    chat.handle_dev_event(ev);
                    while let Ok(app_ev) = rx.try_recv() {
                        if let AppEvent::InsertHistory(lines) = app_ev {
                            transcript.push_str(&lines_to_single_string(&lines));
                            crate::insert_history::insert_history_lines_to_writer(
                                &mut terminal,
                                &mut ansi,
                                lines,
                            );
                        }
                    }
                }
            }
            "app_event" => {
                if let Some(variant) = v.get("variant").and_then(|s| s.as_str())
                    && variant == "CommitTick"
                {
                    chat.on_commit_tick();
                    while let Ok(app_ev) = rx.try_recv() {
                        if let AppEvent::InsertHistory(lines) = app_ev {
                            transcript.push_str(&lines_to_single_string(&lines));
                            crate::insert_history::insert_history_lines_to_writer(
                                &mut terminal,
                                &mut ansi,
                                lines,
                            );
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Read the ideal fixture as-is
    let mut f = open_fixture("ideal-binary-response.txt");
    let mut ideal = String::new();
    f.read_to_string(&mut ideal)
        .expect("read ideal-binary-response.txt");
    // Normalize line endings for Windows vs. Unix checkouts
    let ideal = ideal.replace("\r\n", "\n");

    // Build the final VT100 visual by parsing the ANSI stream. Trim trailing spaces per line
    // and drop trailing empty lines so the shape matches the ideal fixture exactly.
    let mut parser = vt100::Parser::new(height, width, 0);
    parser.process(&ansi);
    let mut lines: Vec<String> = Vec::with_capacity(height as usize);
    for row in 0..height {
        let mut s = String::with_capacity(width as usize);
        for col in 0..width {
            if let Some(cell) = parser.screen().cell(row, col) {
                if let Some(ch) = cell.contents().chars().next() {
                    s.push(ch);
                } else {
                    s.push(' ');
                }
            } else {
                s.push(' ');
            }
        }
        // Trim trailing spaces to match plain text fixture
        lines.push(s.trim_end().to_string());
    }
    while lines.last().is_some_and(|l| l.is_empty()) {
        lines.pop();
    }
    // Compare only after the last session banner marker, and start at the next 'thinking' line.
    const MARKER_PREFIX: &str = ">_ You are using OpenAI Codex in ";
    let last_marker_line_idx = lines
        .iter()
        .rposition(|l| l.starts_with(MARKER_PREFIX))
        .expect("marker not found in visible output");
    let thinking_line_idx = (last_marker_line_idx + 1..lines.len())
        .find(|&idx| lines[idx].trim_start() == "thinking")
        .expect("no 'thinking' line found after marker");

    let mut compare_lines: Vec<String> = Vec::new();
    // Ensure the first line is exactly 'thinking' without leading spaces to match the fixture
    compare_lines.push(lines[thinking_line_idx].trim_start().to_string());
    compare_lines.extend(lines[(thinking_line_idx + 1)..].iter().cloned());
    let visible_after = compare_lines.join("\n");

    // Optionally update the fixture when env var is set
    if std::env::var("UPDATE_IDEAL").as_deref() == Ok("1") {
        let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        p.push("tests");
        p.push("fixtures");
        p.push("ideal-binary-response.txt");
        std::fs::write(&p, &visible_after).expect("write updated ideal fixture");
        return;
    }

    // Exact equality with pretty diff on failure
    assert_eq!(visible_after, ideal);
}

//
// Snapshot test: command approval modal
//
// Synthesizes a Codex ExecApprovalRequest event to trigger the approval modal
// and snapshots the visual output using the ratatui TestBackend.
#[test]
fn approval_modal_exec_snapshot() {
    // Build a chat widget with manual channels to avoid spawning the agent.
    let (mut chat, _rx, _op_rx) = make_chatwidget_manual();
    // Ensure policy allows surfacing approvals explicitly (not strictly required for direct event).
    chat.config.approval_policy = dev_core::protocol::AskForApproval::OnRequest;
    // Inject an exec approval request to display the approval modal.
    let ev = ExecApprovalRequestEvent {
        call_id: "call-approve-cmd".into(),
        command: vec!["bash".into(), "-lc".into(), "echo hello world".into()],
        cwd: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
        reason: Some("Model wants to run a command".into()),
    };
    chat.handle_dev_event(Event {
        id: "sub-approve".into(),
        msg: EventMsg::ExecApprovalRequest(ev),
    });
    // Render to a fixed-size test terminal and snapshot.
    // Call desired_height first and use that exact height for rendering.
    let height = chat.desired_height(80);
    let mut terminal = ratatui::Terminal::new(ratatui::backend::TestBackend::new(80, height))
        .expect("create terminal");
    terminal
        .draw(|f| f.render_widget_ref(&chat, f.area()))
        .expect("draw approval modal");
    assert_snapshot!("approval_modal_exec", terminal.backend());
}

// Snapshot test: patch approval modal
#[test]
fn approval_modal_patch_snapshot() {
    let (mut chat, _rx, _op_rx) = make_chatwidget_manual();
    chat.config.approval_policy = dev_core::protocol::AskForApproval::OnRequest;

    // Build a small changeset and a reason/grant_root to exercise the prompt text.
    let mut changes = std::collections::HashMap::new();
    changes.insert(
        PathBuf::from("README.md"),
        FileChange::Add {
            content: "hello\nworld\n".into(),
        },
    );
    let ev = ApplyPatchApprovalRequestEvent {
        call_id: "call-approve-patch".into(),
        changes,
        reason: Some("The model wants to apply changes".into()),
        grant_root: Some(PathBuf::from("/tmp")),
    };
    chat.handle_dev_event(Event {
        id: "sub-approve-patch".into(),
        msg: EventMsg::ApplyPatchApprovalRequest(ev),
    });

    // Render at the widget's desired height and snapshot.
    let height = chat.desired_height(80);
    let mut terminal = ratatui::Terminal::new(ratatui::backend::TestBackend::new(80, height))
        .expect("create terminal");
    terminal
        .draw(|f| f.render_widget_ref(&chat, f.area()))
        .expect("draw patch approval modal");
    assert_snapshot!("approval_modal_patch", terminal.backend());
}

#[test]
fn interrupt_restores_queued_messages_into_composer() {
    let (mut chat, mut rx, mut op_rx) = make_chatwidget_manual();

    // Simulate a running task to enable queuing of user inputs.
    chat.bottom_pane.set_task_running(true);

    // Queue two user messages while the task is running.
    chat.queued_user_messages
        .push_back(UserMessage::from("first queued".to_string()));
    chat.queued_user_messages
        .push_back(UserMessage::from("second queued".to_string()));
    chat.refresh_queued_user_messages();

    // Deliver a TurnAborted event with Interrupted reason (as if Esc was pressed).
    chat.handle_dev_event(Event {
        id: "turn-1".into(),
        msg: EventMsg::TurnAborted(dev_core::protocol::TurnAbortedEvent {
            reason: dev_core::protocol::TurnAbortReason::Interrupted,
        }),
    });

    // Composer should now contain the queued messages joined by newlines, in order.
    assert_eq!(
        chat.bottom_pane.composer_text(),
        "first queued\nsecond queued"
    );

    // Queue should be cleared and no new user input should have been auto-submitted.
    assert!(chat.queued_user_messages.is_empty());
    assert!(
        op_rx.try_recv().is_err(),
        "unexpected outbound op after interrupt"
    );

    // Drain rx to avoid unused warnings.
    let _ = drain_insert_history(&mut rx);
}

// Snapshot test: ChatWidget at very small heights (idle)
// Ensures overall layout behaves when terminal height is extremely constrained.
#[test]
fn ui_snapshots_small_heights_idle() {
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    let (chat, _rx, _op_rx) = make_chatwidget_manual();
    for h in [1u16, 2, 3] {
        let name = format!("chat_small_idle_h{h}");
        let mut terminal = Terminal::new(TestBackend::new(40, h)).expect("create terminal");
        terminal
            .draw(|f| f.render_widget_ref(&chat, f.area()))
            .expect("draw chat idle");
        assert_snapshot!(name, terminal.backend());
    }
}

// Snapshot test: ChatWidget at very small heights (task running)
// Validates how status + composer are presented within tight space.
#[test]
fn ui_snapshots_small_heights_task_running() {
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    let (mut chat, _rx, _op_rx) = make_chatwidget_manual();
    // Activate status line
    chat.handle_dev_event(Event {
        id: "task-1".into(),
        msg: EventMsg::TaskStarted(TaskStartedEvent {
            model_context_window: None,
        }),
    });
    chat.handle_dev_event(Event {
        id: "task-1".into(),
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "**Thinking**".into(),
        }),
    });
    for h in [1u16, 2, 3] {
        let name = format!("chat_small_running_h{h}");
        let mut terminal = Terminal::new(TestBackend::new(40, h)).expect("create terminal");
        terminal
            .draw(|f| f.render_widget_ref(&chat, f.area()))
            .expect("draw chat running");
        assert_snapshot!(name, terminal.backend());
    }
}

// Snapshot test: status widget + approval modal active together
// The modal takes precedence visually; this captures the layout with a running
// task (status indicator active) while an approval request is shown.
#[test]
fn status_widget_and_approval_modal_snapshot() {
    use dev_core::protocol::ExecApprovalRequestEvent;

    let (mut chat, _rx, _op_rx) = make_chatwidget_manual();
    // Begin a running task so the status indicator would be active.
    chat.handle_dev_event(Event {
        id: "task-1".into(),
        msg: EventMsg::TaskStarted(TaskStartedEvent {
            model_context_window: None,
        }),
    });
    // Provide a deterministic header for the status line.
    chat.handle_dev_event(Event {
        id: "task-1".into(),
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "**Analyzing**".into(),
        }),
    });

    // Now show an approval modal (e.g. exec approval).
    let ev = ExecApprovalRequestEvent {
        call_id: "call-approve-exec".into(),
        command: vec!["echo".into(), "hello world".into()],
        cwd: std::path::PathBuf::from("/tmp"),
        reason: Some("Codex wants to run a command".into()),
    };
    chat.handle_dev_event(Event {
        id: "sub-approve-exec".into(),
        msg: EventMsg::ExecApprovalRequest(ev),
    });

    // Render at the widget's desired height and snapshot.
    let height = chat.desired_height(80);
    let mut terminal = ratatui::Terminal::new(ratatui::backend::TestBackend::new(80, height))
        .expect("create terminal");
    terminal
        .draw(|f| f.render_widget_ref(&chat, f.area()))
        .expect("draw status + approval modal");
    assert_snapshot!("status_widget_and_approval_modal", terminal.backend());
}

// Snapshot test: status widget active (StatusIndicatorView)
// Ensures the VT100 rendering of the status indicator is stable when active.
#[test]
fn status_widget_active_snapshot() {
    let (mut chat, _rx, _op_rx) = make_chatwidget_manual();
    // Activate the status indicator by simulating a task start.
    chat.handle_dev_event(Event {
        id: "task-1".into(),
        msg: EventMsg::TaskStarted(TaskStartedEvent {
            model_context_window: None,
        }),
    });
    // Provide a deterministic header via a bold reasoning chunk.
    chat.handle_dev_event(Event {
        id: "task-1".into(),
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "**Analyzing**".into(),
        }),
    });
    // Render and snapshot.
    let height = chat.desired_height(80);
    let mut terminal = ratatui::Terminal::new(ratatui::backend::TestBackend::new(80, height))
        .expect("create terminal");
    terminal
        .draw(|f| f.render_widget_ref(&chat, f.area()))
        .expect("draw status widget");
    assert_snapshot!("status_widget_active", terminal.backend());
}

#[test]
fn apply_patch_events_emit_history_cells() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // 1) Approval request -> proposed patch summary cell
    let mut changes = HashMap::new();
    changes.insert(
        PathBuf::from("foo.txt"),
        FileChange::Add {
            content: "hello\n".to_string(),
        },
    );
    let ev = ApplyPatchApprovalRequestEvent {
        call_id: "c1".into(),
        changes,
        reason: None,
        grant_root: None,
    };
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::ApplyPatchApprovalRequest(ev),
    });
    let cells = drain_insert_history(&rx);
    assert!(!cells.is_empty(), "expected pending patch cell to be sent");
    let blob = lines_to_single_string(cells.last().unwrap());
    assert!(
        blob.contains("proposed patch"),
        "missing proposed patch header: {blob:?}"
    );

    // 2) Begin apply -> applying patch cell
    let mut changes2 = HashMap::new();
    changes2.insert(
        PathBuf::from("foo.txt"),
        FileChange::Add {
            content: "hello\n".to_string(),
        },
    );
    let begin = PatchApplyBeginEvent {
        call_id: "c1".into(),
        auto_approved: true,
        changes: changes2,
    };
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::PatchApplyBegin(begin),
    });
    let cells = drain_insert_history(&rx);
    assert!(!cells.is_empty(), "expected applying patch cell to be sent");
    let blob = lines_to_single_string(cells.last().unwrap());
    assert!(
        blob.contains("Applying patch"),
        "missing applying patch header: {blob:?}"
    );

    // 3) End apply success -> success cell
    let end = PatchApplyEndEvent {
        call_id: "c1".into(),
        stdout: "ok\n".into(),
        stderr: String::new(),
        success: true,
    };
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::PatchApplyEnd(end),
    });
    let cells = drain_insert_history(&rx);
    assert!(!cells.is_empty(), "expected applied patch cell to be sent");
    let blob = lines_to_single_string(cells.last().unwrap());
    assert!(
        blob.contains("Applied patch"),
        "missing applied patch header: {blob:?}"
    );
}

#[test]
fn apply_patch_approval_sends_op_with_submission_id() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();
    // Simulate receiving an approval request with a distinct submission id and call id
    let mut changes = HashMap::new();
    changes.insert(
        PathBuf::from("file.rs"),
        FileChange::Add {
            content: "fn main(){}\n".into(),
        },
    );
    let ev = ApplyPatchApprovalRequestEvent {
        call_id: "call-999".into(),
        changes,
        reason: None,
        grant_root: None,
    };
    chat.handle_dev_event(Event {
        id: "sub-123".into(),
        msg: EventMsg::ApplyPatchApprovalRequest(ev),
    });

    // Approve via key press 'y'
    chat.handle_key_event(KeyEvent::new(KeyCode::Char('y'), KeyModifiers::NONE));

    // Expect a CodexOp with PatchApproval carrying the submission id, not call id
    let mut found = false;
    while let Ok(app_ev) = rx.try_recv() {
        if let AppEvent::CodexOp(Op::PatchApproval { id, decision }) = app_ev {
            assert_eq!(id, "sub-123");
            assert!(matches!(
                decision,
                dev_core::protocol::ReviewDecision::Approved
            ));
            found = true;
            break;
        }
    }
    assert!(found, "expected PatchApproval op to be sent");
}

#[test]
fn apply_patch_full_flow_integration_like() {
    let (mut chat, rx, mut op_rx) = make_chatwidget_manual();

    // 1) Backend requests approval
    let mut changes = HashMap::new();
    changes.insert(
        PathBuf::from("pkg.rs"),
        FileChange::Add { content: "".into() },
    );
    chat.handle_dev_event(Event {
        id: "sub-xyz".into(),
        msg: EventMsg::ApplyPatchApprovalRequest(ApplyPatchApprovalRequestEvent {
            call_id: "call-1".into(),
            changes,
            reason: None,
            grant_root: None,
        }),
    });

    // 2) User approves via 'y' and App receives a CodexOp
    chat.handle_key_event(KeyEvent::new(KeyCode::Char('y'), KeyModifiers::NONE));
    let mut maybe_op: Option<Op> = None;
    while let Ok(app_ev) = rx.try_recv() {
        if let AppEvent::CodexOp(op) = app_ev {
            maybe_op = Some(op);
            break;
        }
    }
    let op = maybe_op.expect("expected CodexOp after key press");

    // 3) App forwards to widget.submit_op, which pushes onto dev_op_tx
    chat.submit_op(op);
    let forwarded = op_rx
        .try_recv()
        .expect("expected op forwarded to codex channel");
    match forwarded {
        Op::PatchApproval { id, decision } => {
            assert_eq!(id, "sub-xyz");
            assert!(matches!(
                decision,
                dev_core::protocol::ReviewDecision::Approved
            ));
        }
        other => panic!("unexpected op forwarded: {other:?}"),
    }

    // 4) Simulate patch begin/end events from backend; ensure history cells are emitted
    let mut changes2 = HashMap::new();
    changes2.insert(
        PathBuf::from("pkg.rs"),
        FileChange::Add { content: "".into() },
    );
    chat.handle_dev_event(Event {
        id: "sub-xyz".into(),
        msg: EventMsg::PatchApplyBegin(PatchApplyBeginEvent {
            call_id: "call-1".into(),
            auto_approved: false,
            changes: changes2,
        }),
    });
    chat.handle_dev_event(Event {
        id: "sub-xyz".into(),
        msg: EventMsg::PatchApplyEnd(PatchApplyEndEvent {
            call_id: "call-1".into(),
            stdout: String::from("ok"),
            stderr: String::new(),
            success: true,
        }),
    });
}

#[test]
fn apply_patch_untrusted_shows_approval_modal() {
    let (mut chat, _rx, _op_rx) = make_chatwidget_manual();
    // Ensure approval policy is untrusted (OnRequest)
    chat.config.approval_policy = dev_core::protocol::AskForApproval::OnRequest;

    // Simulate a patch approval request from backend
    let mut changes = HashMap::new();
    changes.insert(
        PathBuf::from("a.rs"),
        FileChange::Add { content: "".into() },
    );
    chat.handle_dev_event(Event {
        id: "sub-1".into(),
        msg: EventMsg::ApplyPatchApprovalRequest(ApplyPatchApprovalRequestEvent {
            call_id: "call-1".into(),
            changes,
            reason: None,
            grant_root: None,
        }),
    });

    // Render and ensure the approval modal title is present
    let area = ratatui::layout::Rect::new(0, 0, 80, 12);
    let mut buf = ratatui::buffer::Buffer::empty(area);
    (&chat).render_ref(area, &mut buf);

    let mut contains_title = false;
    for y in 0..area.height {
        let mut row = String::new();
        for x in 0..area.width {
            row.push(buf[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if row.contains("Apply changes?") {
            contains_title = true;
            break;
        }
    }
    assert!(
        contains_title,
        "expected approval modal to be visible with title 'Apply changes?'"
    );
}

#[test]
fn apply_patch_request_shows_diff_summary() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Ensure we are in OnRequest so an approval is surfaced
    chat.config.approval_policy = dev_core::protocol::AskForApproval::OnRequest;

    // Simulate backend asking to apply a patch adding two lines to README.md
    let mut changes = HashMap::new();
    changes.insert(
        PathBuf::from("README.md"),
        FileChange::Add {
            // Two lines (no trailing empty line counted)
            content: "line one\nline two\n".into(),
        },
    );
    chat.handle_dev_event(Event {
        id: "sub-apply".into(),
        msg: EventMsg::ApplyPatchApprovalRequest(ApplyPatchApprovalRequestEvent {
            call_id: "call-apply".into(),
            changes,
            reason: None,
            grant_root: None,
        }),
    });

    // Drain history insertions and verify the diff summary is present
    let cells = drain_insert_history(&rx);
    assert!(
        !cells.is_empty(),
        "expected a history cell with the proposed patch summary"
    );
    let blob = lines_to_single_string(cells.last().unwrap());

    // Header should summarize totals
    assert!(
        blob.contains("proposed patch to 1 file (+2 -0)"),
        "missing or incorrect diff header: {blob:?}"
    );

    // Per-file summary line should include the file path and counts
    assert!(
        blob.contains("README.md"),
        "missing per-file diff summary: {blob:?}"
    );
}

#[test]
fn plan_update_renders_history_cell() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();
    let update = UpdatePlanArgs {
        explanation: Some("Adapting plan".to_string()),
        plan: vec![
            PlanItemArg {
                step: "Explore codebase".into(),
                status: StepStatus::Completed,
            },
            PlanItemArg {
                step: "Implement feature".into(),
                status: StepStatus::InProgress,
            },
            PlanItemArg {
                step: "Write tests".into(),
                status: StepStatus::Pending,
            },
        ],
    };
    chat.handle_dev_event(Event {
        id: "sub-1".into(),
        msg: EventMsg::PlanUpdate(update),
    });
    let cells = drain_insert_history(&rx);
    assert!(!cells.is_empty(), "expected plan update cell to be sent");
    let blob = lines_to_single_string(cells.last().unwrap());
    assert!(
        blob.contains("Update plan"),
        "missing plan header: {blob:?}"
    );
    assert!(blob.contains("Explore codebase"));
    assert!(blob.contains("Implement feature"));
    assert!(blob.contains("Write tests"));
}

#[test]
fn headers_emitted_on_stream_begin_for_answer_and_reasoning() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Answer: no header until a newline commit
    chat.handle_dev_event(Event {
        id: "sub-a".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "Hello".into(),
        }),
    });
    let mut saw_dev_pre = false;
    while let Ok(ev) = rx.try_recv() {
        if let AppEvent::InsertHistory(lines) = ev {
            let s = lines
                .iter()
                .flat_map(|l| l.spans.iter())
                .map(|sp| sp.content.clone())
                .collect::<Vec<_>>()
                .join("");
            if s.contains("codex") {
                saw_dev_pre = true;
                break;
            }
        }
    }
    assert!(
        !saw_dev_pre,
        "answer header should not be emitted before first newline commit"
    );

    // Newline arrives; no visible header should be emitted for Answer
    chat.handle_dev_event(Event {
        id: "sub-a".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "!\n".into(),
        }),
    });
    chat.on_commit_tick();
    let mut saw_dev_post = false;
    while let Ok(ev) = rx.try_recv() {
        if let AppEvent::InsertHistory(lines) = ev {
            let s = lines
                .iter()
                .flat_map(|l| l.spans.iter())
                .map(|sp| sp.content.clone())
                .collect::<Vec<_>>()
                .join("");
            if s.contains("codex") {
                saw_dev_post = true;
                break;
            }
        }
    }
    assert!(
        !saw_dev_post,
        "did not expect a visible 'codex' header to be emitted after first newline commit"
    );

    // Reasoning: header immediately
    let (mut chat2, rx2, _op_rx2) = make_chatwidget_manual();
    chat2.handle_dev_event(Event {
        id: "sub-b".into(),
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "Thinking".into(),
        }),
    });
    let mut saw_thinking = false;
    while let Ok(ev) = rx2.try_recv() {
        if let AppEvent::InsertHistory(lines) = ev {
            let s = lines
                .iter()
                .flat_map(|l| l.spans.iter())
                .map(|sp| sp.content.clone())
                .collect::<Vec<_>>()
                .join("");
            if s.contains("thinking") {
                saw_thinking = true;
                break;
            }
        }
    }
    assert!(
        saw_thinking,
        "expected 'thinking' header to be emitted at stream start"
    );
}

#[test]
fn multiple_agent_messages_in_single_turn_emit_multiple_headers() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Begin turn
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::TaskStarted(TaskStartedEvent {
            model_context_window: None,
        }),
    });

    // First finalized assistant message
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "First message".into(),
        }),
    });

    // Second finalized assistant message in the same turn
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "Second message".into(),
        }),
    });

    // End turn
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::TaskComplete(TaskCompleteEvent {
            last_agent_message: None,
        }),
    });

    let cells = drain_insert_history(&rx);
    let mut combined = String::new();
    for lines in &cells {
        for l in lines {
            for sp in &l.spans {
                let s = &sp.content;
                combined.push_str(s);
            }
            combined.push('\n');
        }
    }
    assert!(
        combined.contains("First message"),
        "missing first message: {combined}"
    );
    assert!(
        combined.contains("Second message"),
        "missing second message: {combined}"
    );
    let first_idx = combined.find("First message").unwrap();
    let second_idx = combined.find("Second message").unwrap();
    assert!(first_idx < second_idx, "messages out of order: {combined}");
}

#[test]
fn two_final_answers_append_not_overwrite_when_no_deltas() {
    // Directly exercise ChatWidget::insert_final_answer to validate we do not
    // overwrite a prior finalized assistant message when a new final arrives
    // without any streaming deltas (regression guard for overwrite bug).
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // First finalized assistant message (no streaming cell exists yet)
    chat.insert_final_answer_with_id(None, Vec::new(), "First message".to_string());
    // Second finalized assistant message (also without streaming)
    chat.insert_final_answer_with_id(None, Vec::new(), "Second message".to_string());

    // Drain any history insert side-effects (not strictly required here)
    let _ = drain_insert_history(&rx);

    // Verify via exported ResponseItems so we don't reach into private fields
    let items = chat.export_response_items();
    let assistants: Vec<String> = items
        .into_iter()
        .filter_map(|it| match it {
            dev_protocol::models::ResponseItem::Message { role, content, .. } if role == "assistant" => {
                let text = content.into_iter().filter_map(|c| match c {
                    dev_protocol::models::ContentItem::OutputText { text } => Some(text),
                    dev_protocol::models::ContentItem::InputText { text } => Some(text),
                    _ => None,
                }).collect::<Vec<_>>().join("\n");
                Some(text)
            }
            _ => None,
        })
        .collect();

    assert_eq!(assistants.len(), 2, "expected two assistant messages, got {}", assistants.len());
    assert!(assistants.iter().any(|s| s.contains("First message")), "missing first message");
    assert!(assistants.iter().any(|s| s.contains("Second message")), "missing second message");
}

#[test]
fn second_final_that_is_superset_replaces_first() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    let base = "• Behavior\n  ◦ One\n\nNotes\n  ◦ Alpha\n".to_string();
    let extended = format!("{}  ◦ Beta\n", base);

    chat.insert_final_answer_with_id(None, Vec::new(), base);
    chat.insert_final_answer_with_id(None, Vec::new(), extended.clone());

    // Drain history events
    let _ = drain_insert_history(&rx);

    // Expect exactly one assistant message containing the extended text
    let items = chat.export_response_items();
    let assistants: Vec<String> = items
        .into_iter()
        .filter_map(|it| match it {
            dev_protocol::models::ResponseItem::Message { role, content, .. } if role == "assistant" => {
                let text = content.into_iter().filter_map(|c| match c {
                    dev_protocol::models::ContentItem::OutputText { text } => Some(text),
                    dev_protocol::models::ContentItem::InputText { text } => Some(text),
                    _ => None,
                }).collect::<Vec<_>>().join("\n");
                Some(text)
            }
            _ => None,
        })
        .collect();

    assert_eq!(assistants.len(), 1, "expected single assistant after superset replace");
    assert!(assistants[0].contains("Beta"), "expected extended content present");
}

#[test]
fn identical_content_with_unicode_bullets_dedupes() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    let a = "• Behavior\n  ◦ One\n\nNotes\n  ◦ Alpha".to_string();
    let b = "- Behavior\n  - One\n\nNotes\n  - Alpha".to_string();

    // Two finals that only differ in bullet glyphs should dedupe into one cell
    chat.insert_final_answer_with_id(None, Vec::new(), a);
    chat.insert_final_answer_with_id(None, Vec::new(), b);

    // Drain history events
    let _ = drain_insert_history(&rx);

    let items = chat.export_response_items();
    let assistants: Vec<String> = items
        .into_iter()
        .filter_map(|it| match it {
            dev_protocol::models::ResponseItem::Message { role, content, .. } if role == "assistant" => {
                let text = content.into_iter().filter_map(|c| match c {
                    dev_protocol::models::ContentItem::OutputText { text } => Some(text),
                    dev_protocol::models::ContentItem::InputText { text } => Some(text),
                    _ => None,
                }).collect::<Vec<_>>().join("\n");
                Some(text)
            }
            _ => None,
        })
        .collect();

    assert_eq!(assistants.len(), 1, "expected deduped single assistant cell");
}

#[test]
fn final_reasoning_then_message_without_deltas_are_rendered() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // No deltas; only final reasoning followed by final message.
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentReasoning(AgentReasoningEvent {
            text: "I will first analyze the request.".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "Here is the result.".into(),
        }),
    });

    // Drain history and snapshot the combined visible content.
    let cells = drain_insert_history(&rx);
    let combined = cells
        .iter()
        .map(|lines| lines_to_single_string(lines))
        .collect::<String>();
    assert_snapshot!(combined);
}

#[test]
fn deltas_then_same_final_message_are_rendered_snapshot() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Stream some reasoning deltas first.
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "I will ".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "first analyze the ".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "request.".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentReasoning(AgentReasoningEvent {
            text: "request.".into(),
        }),
    });

    // Then stream answer deltas, followed by the exact same final message.
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "Here is the ".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "result.".into(),
        }),
    });

    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "Here is the result.".into(),
        }),
    });

    // Snapshot the combined visible content to ensure we render as expected
    // when deltas are followed by the identical final message.
    let cells = drain_insert_history(&rx);
    let combined = cells
        .iter()
        .map(|lines| lines_to_single_string(lines))
        .collect::<String>();
    assert_snapshot!(combined);
}

#[test]
fn late_final_does_not_duplicate_when_stream_finalized_early() {
    use dev_core::protocol::*;

    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Stream some answer deltas for id "s1"
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "What I Can Do\n".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "- Explore/Modify Code\n".into(),
        }),
    });

    // Simulate a side event that forces finalization (e.g., tool start)
    chat.finalize_active_stream();

    // Now a late final AgentMessage arrives with the full text
    let final_text = "What I Can Do\n- Explore/Modify Code\n".to_string();
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: final_text.clone(),
        }),
    });

    // Drain any history insert side-effects (not strictly required here)
    let _ = drain_insert_history(&rx);

    // Export and assert that only a single assistant message exists
    let items = chat.export_response_items();
    let assistants: Vec<String> = items
        .into_iter()
        .filter_map(|it| match it {
            dev_protocol::models::ResponseItem::Message { role, content, .. } if role == "assistant" => {
                let text = content.into_iter().filter_map(|c| match c {
                    dev_protocol::models::ContentItem::OutputText { text } => Some(text),
                    dev_protocol::models::ContentItem::InputText { text } => Some(text),
                    _ => None,
                }).collect::<Vec<_>>().join("\n");
                Some(text)
            }
            _ => None,
        })
        .collect();

    assert_eq!(assistants.len(), 1, "late final should replace, not duplicate");
    assert!(assistants[0].contains("Explore/Modify Code"));
}

#[test]
fn streaming_answer_then_finalize_does_not_truncate() {
    let (mut chat, rx, _op_rx) = make_chatwidget_manual();

    // Stream an assistant message in chunks, without sending a final AgentMessage.
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "Files changed\n".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "- codex-rs/tui/src/markdown.rs: Guard against list markers...\n".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "\nWhat to expect\n".into(),
        }),
    });
    chat.handle_dev_event(Event {
        id: "s1".into(),
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "- Third-level bullets render correctly now.\n".into(),
        }),
    });

    // Simulate lifecycle location that finalizes active stream (e.g., new event)
    chat.finalize_active_stream();

    // Drain and combine visible content
    let cells = drain_insert_history(&rx);
    let combined = cells
        .iter()
        .map(|lines| lines_to_single_string(lines))
        .collect::<String>();

    assert!(combined.contains("Files changed"), "missing header: {combined}");
    assert!(combined.contains("What to expect"), "missing section header: {combined}");
    assert!(combined.contains("Third-level bullets render correctly now"),
        "missing tail content after finalize: {combined}");
}
