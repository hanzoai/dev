#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fs::File;
use std::fs::{self};
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;

use tempfile::TempDir;
use time::OffsetDateTime;
use time::PrimitiveDateTime;
use time::format_description::FormatItem;
use time::macros::format_description;
use uuid::Uuid;

use crate::rollout::INTERACTIVE_SESSION_SOURCES;
use crate::rollout::list::ConversationItem;
use crate::rollout::list::ConversationsPage;
use crate::rollout::list::Cursor;
use crate::rollout::list::get_conversation;
use crate::rollout::list::get_conversations;
use code_protocol::ConversationId;
use code_protocol::protocol::RecordedEvent;
use code_protocol::protocol::RolloutItem;
use code_protocol::protocol::RolloutLine;
use code_protocol::protocol::SessionMeta;
use code_protocol::protocol::SessionMetaLine;
use code_protocol::protocol::SessionSource;
use code_protocol::protocol::UserMessageEvent;
use crate::protocol::EventMsg;

const NO_SOURCE_FILTER: &[SessionSource] = &[];

fn write_session_file(
    root: &Path,
    ts_str: &str,
    uuid: Uuid,
    num_records: usize,
    source: Option<SessionSource>,
) -> std::io::Result<(OffsetDateTime, Uuid)> {
    let format: &[FormatItem] =
        format_description!("[year]-[month]-[day]T[hour]-[minute]-[second]");
    let dt = PrimitiveDateTime::parse(ts_str, format)
        .unwrap()
        .assume_utc();
    let dir = root
        .join("sessions")
        .join(format!("{:04}", dt.year()))
        .join(format!("{:02}", u8::from(dt.month())))
        .join(format!("{:02}", dt.day()));
    fs::create_dir_all(&dir)?;

    let filename = format!("rollout-{ts_str}-{uuid}.jsonl");
    let file_path = dir.join(filename);
    let file = File::create(file_path)?;
    let mut writer = BufWriter::new(file);

    let conversation_id = ConversationId::from(uuid);
    let session_meta = SessionMeta {
        id: conversation_id,
        timestamp: ts_str.to_string(),
        cwd: Path::new(".").to_path_buf(),
        originator: "test_originator".to_string(),
        cli_version: "test_version".to_string(),
        instructions: None,
        source: source.unwrap_or_default(),
    };
    let session_meta_line = RolloutLine {
        timestamp: ts_str.to_string(),
        item: RolloutItem::SessionMeta(SessionMetaLine {
            meta: session_meta,
            git: None,
        }),
    };
    serde_json::to_writer(&mut writer, &session_meta_line)?;
    writer.write_all(b"\n")?;

    for i in 0..num_records {
        let event = RecordedEvent {
            id: format!("event-{i}"),
            event_seq: i as u64,
            order: None,
            msg: EventMsg::UserMessage(UserMessageEvent {
                message: format!("Message {i}"),
                kind: None,
                images: None,
            }),
        };
        let line = RolloutLine {
            timestamp: ts_str.to_string(),
            item: RolloutItem::Event(event),
        };
        serde_json::to_writer(&mut writer, &line)?;
        writer.write_all(b"\n")?;
    }
    Ok((dt, uuid))
}

#[tokio::test]
async fn test_list_conversations_latest_first() {
    let temp = TempDir::new().unwrap();
    let home = temp.path();

    // Fixed UUIDs for deterministic expectations
    let u1 = Uuid::from_u128(1);
    let u2 = Uuid::from_u128(2);
    let u3 = Uuid::from_u128(3);

    // Create three sessions across three days
    write_session_file(
        home,
        "2025-01-01T12-00-00",
        u1,
        3,
        Some(SessionSource::VSCode),
    )
    .unwrap();
    write_session_file(
        home,
        "2025-01-02T12-00-00",
        u2,
        3,
        Some(SessionSource::VSCode),
    )
    .unwrap();
    write_session_file(
        home,
        "2025-01-03T12-00-00",
        u3,
        3,
        Some(SessionSource::VSCode),
    )
    .unwrap();

    let page = get_conversations(home, 10, None, INTERACTIVE_SESSION_SOURCES)
        .await
        .unwrap();

    // Build expected objects
    let p1 = home
        .join("sessions")
        .join("2025")
        .join("01")
        .join("03")
        .join(format!("rollout-2025-01-03T12-00-00-{u3}.jsonl"));
    let p2 = home
        .join("sessions")
        .join("2025")
        .join("01")
        .join("02")
        .join(format!("rollout-2025-01-02T12-00-00-{u2}.jsonl"));
    let p3 = home
        .join("sessions")
        .join("2025")
        .join("01")
        .join("01")
        .join(format!("rollout-2025-01-01T12-00-00-{u1}.jsonl"));

    let head_3 = vec![
        serde_json::json!({"timestamp": "2025-01-03T12-00-00", "id": u3.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
        serde_json::json!({"record_type": "response", "index": 1}),
        serde_json::json!({"record_type": "response", "index": 2}),
    ];
    let head_2 = vec![
        serde_json::json!({"timestamp": "2025-01-02T12-00-00", "id": u2.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
        serde_json::json!({"record_type": "response", "index": 1}),
        serde_json::json!({"record_type": "response", "index": 2}),
    ];
    let head_1 = vec![
        serde_json::json!({"timestamp": "2025-01-01T12-00-00", "id": u1.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
        serde_json::json!({"record_type": "response", "index": 1}),
        serde_json::json!({"record_type": "response", "index": 2}),
    ];

    let expected_cursor: Cursor =
        serde_json::from_str(&format!("\"2025-01-01T12-00-00|{u1}\"")).unwrap();

    let expected = ConversationsPage {
        items: vec![
            ConversationItem {
                path: p1,
                head: head_3,
            },
            ConversationItem {
                path: p2,
                head: head_2,
            },
            ConversationItem {
                path: p3,
                head: head_1,
            },
        ],
        next_cursor: Some(expected_cursor),
        num_scanned_files: 3,
        reached_scan_cap: false,
    };

    assert_eq!(page, expected);
}

#[tokio::test]
async fn test_pagination_cursor() {
    let temp = TempDir::new().unwrap();
    let home = temp.path();

    // Fixed UUIDs for deterministic expectations
    let u1 = Uuid::from_u128(11);
    let u2 = Uuid::from_u128(22);
    let u3 = Uuid::from_u128(33);
    let u4 = Uuid::from_u128(44);
    let u5 = Uuid::from_u128(55);

    // Oldest to newest
    write_session_file(
        home,
        "2025-03-01T09-00-00",
        u1,
        1,
        Some(SessionSource::VSCode),
    )
    .unwrap();
    write_session_file(
        home,
        "2025-03-02T09-00-00",
        u2,
        1,
        Some(SessionSource::VSCode),
    )
    .unwrap();
    write_session_file(
        home,
        "2025-03-03T09-00-00",
        u3,
        1,
        Some(SessionSource::VSCode),
    )
    .unwrap();
    write_session_file(
        home,
        "2025-03-04T09-00-00",
        u4,
        1,
        Some(SessionSource::VSCode),
    )
    .unwrap();
    write_session_file(
        home,
        "2025-03-05T09-00-00",
        u5,
        1,
        Some(SessionSource::VSCode),
    )
    .unwrap();

    let page1 = get_conversations(home, 2, None, INTERACTIVE_SESSION_SOURCES)
        .await
        .unwrap();
    let p5 = home
        .join("sessions")
        .join("2025")
        .join("03")
        .join("05")
        .join(format!("rollout-2025-03-05T09-00-00-{u5}.jsonl"));
    let p4 = home
        .join("sessions")
        .join("2025")
        .join("03")
        .join("04")
        .join(format!("rollout-2025-03-04T09-00-00-{u4}.jsonl"));
    let head_5 = vec![
        serde_json::json!({"timestamp": "2025-03-05T09-00-00", "id": u5.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
    ];
    let head_4 = vec![
        serde_json::json!({"timestamp": "2025-03-04T09-00-00", "id": u4.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
    ];
    let expected_cursor1: Cursor =
        serde_json::from_str(&format!("\"2025-03-04T09-00-00|{u4}\"")).unwrap();
    let expected_page1 = ConversationsPage {
        items: vec![
            ConversationItem {
                path: p5,
                head: head_5,
            },
            ConversationItem {
                path: p4,
                head: head_4,
            },
        ],
        next_cursor: Some(expected_cursor1.clone()),
        num_scanned_files: 3, // scanned 05, 04, and peeked at 03 before breaking
        reached_scan_cap: false,
    };
    assert_eq!(page1, expected_page1);

    let page2 = get_conversations(
        home,
        2,
        page1.next_cursor.as_ref(),
        INTERACTIVE_SESSION_SOURCES,
    )
    .await
    .unwrap();
    let p3 = home
        .join("sessions")
        .join("2025")
        .join("03")
        .join("03")
        .join(format!("rollout-2025-03-03T09-00-00-{u3}.jsonl"));
    let p2 = home
        .join("sessions")
        .join("2025")
        .join("03")
        .join("02")
        .join(format!("rollout-2025-03-02T09-00-00-{u2}.jsonl"));
    let head_3 = vec![
        serde_json::json!({"timestamp": "2025-03-03T09-00-00", "id": u3.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
    ];
    let head_2 = vec![
        serde_json::json!({"timestamp": "2025-03-02T09-00-00", "id": u2.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
    ];
    let expected_cursor2: Cursor =
        serde_json::from_str(&format!("\"2025-03-02T09-00-00|{u2}\"")).unwrap();
    let expected_page2 = ConversationsPage {
        items: vec![
            ConversationItem {
                path: p3,
                head: head_3,
            },
            ConversationItem {
                path: p2,
                head: head_2,
            },
        ],
        next_cursor: Some(expected_cursor2.clone()),
        num_scanned_files: 5, // scanned 05, 04 (anchor), 03, 02, and peeked at 01
        reached_scan_cap: false,
    };
    assert_eq!(page2, expected_page2);

    let page3 = get_conversations(
        home,
        2,
        page2.next_cursor.as_ref(),
        INTERACTIVE_SESSION_SOURCES,
    )
    .await
    .unwrap();
    let p1 = home
        .join("sessions")
        .join("2025")
        .join("03")
        .join("01")
        .join(format!("rollout-2025-03-01T09-00-00-{u1}.jsonl"));
    let head_1 = vec![
        serde_json::json!({"timestamp": "2025-03-01T09-00-00", "id": u1.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
    ];
    let expected_cursor3: Cursor =
        serde_json::from_str(&format!("\"2025-03-01T09-00-00|{u1}\"")).unwrap();
    let expected_page3 = ConversationsPage {
        items: vec![ConversationItem {
            path: p1,
            head: head_1,
        }],
        next_cursor: Some(expected_cursor3.clone()),
        num_scanned_files: 5, // scanned 05, 04 (anchor), 03, 02 (anchor), 01
        reached_scan_cap: false,
    };
    assert_eq!(page3, expected_page3);
}

#[tokio::test]
async fn test_get_conversation_contents() {
    let temp = TempDir::new().unwrap();
    let home = temp.path();

    let uuid = Uuid::new_v4();
    let ts = "2025-04-01T10-30-00";
    write_session_file(home, ts, uuid, 2, Some(SessionSource::VSCode)).unwrap();

    let page = get_conversations(home, 1, None, INTERACTIVE_SESSION_SOURCES)
        .await
        .unwrap();
    let path = &page.items[0].path;

    let content = get_conversation(path).await.unwrap();

    // Page equality (single item)
    let expected_path = home
        .join("sessions")
        .join("2025")
        .join("04")
        .join("01")
        .join(format!("rollout-2025-04-01T10-30-00-{uuid}.jsonl"));
    let expected_head = vec![
        serde_json::json!({"timestamp": ts, "id": uuid.to_string()}),
        serde_json::json!({"record_type": "response", "index": 0}),
        serde_json::json!({"record_type": "response", "index": 1}),
    ];
    let expected_cursor: Cursor = serde_json::from_str(&format!("\"{ts}|{uuid}\"")).unwrap();
    let expected_page = ConversationsPage {
        items: vec![ConversationItem {
            path: expected_path.clone(),
            head: expected_head,
        }],
        next_cursor: Some(expected_cursor),
        num_scanned_files: 1,
        reached_scan_cap: false,
    };
    assert_eq!(page, expected_page);

    // Entire file contents equality
    let meta = serde_json::json!({"timestamp": ts, "id": uuid.to_string()});
    let rec0 = serde_json::json!({"record_type": "response", "index": 0});
    let rec1 = serde_json::json!({"record_type": "response", "index": 1});
    let expected_content = format!("{meta}\n{rec0}\n{rec1}\n");
    assert_eq!(content, expected_content);
}

#[tokio::test]
async fn test_stable_ordering_same_second_pagination() {
    let temp = TempDir::new().unwrap();
    let home = temp.path();

    let ts = "2025-07-01T00-00-00";
    let u1 = Uuid::from_u128(1);
    let u2 = Uuid::from_u128(2);
    let u3 = Uuid::from_u128(3);

    write_session_file(home, ts, u1, 0, Some(SessionSource::VSCode)).unwrap();
    write_session_file(home, ts, u2, 0, Some(SessionSource::VSCode)).unwrap();
    write_session_file(home, ts, u3, 0, Some(SessionSource::VSCode)).unwrap();

    let page1 = get_conversations(home, 2, None, INTERACTIVE_SESSION_SOURCES)
        .await
        .unwrap();

    let p3 = home
        .join("sessions")
        .join("2025")
        .join("07")
        .join("01")
        .join(format!("rollout-2025-07-01T00-00-00-{u3}.jsonl"));
    let p2 = home
        .join("sessions")
        .join("2025")
        .join("07")
        .join("01")
        .join(format!("rollout-2025-07-01T00-00-00-{u2}.jsonl"));
    let head = |u: Uuid| -> Vec<serde_json::Value> {
        vec![serde_json::json!({"timestamp": ts, "id": u.to_string()})]
    };
    let expected_cursor1: Cursor = serde_json::from_str(&format!("\"{ts}|{u2}\"")).unwrap();
    let expected_page1 = ConversationsPage {
        items: vec![
            ConversationItem {
                path: p3,
                head: head(u3),
            },
            ConversationItem {
                path: p2,
                head: head(u2),
            },
        ],
        next_cursor: Some(expected_cursor1.clone()),
        num_scanned_files: 3, // scanned u3, u2, peeked u1
        reached_scan_cap: false,
    };
    assert_eq!(page1, expected_page1);

    let page2 = get_conversations(
        home,
        2,
        page1.next_cursor.as_ref(),
        INTERACTIVE_SESSION_SOURCES,
    )
    .await
    .unwrap();
    let p1 = home
        .join("sessions")
        .join("2025")
        .join("07")
        .join("01")
        .join(format!("rollout-2025-07-01T00-00-00-{u1}.jsonl"));
    let expected_cursor2: Cursor = serde_json::from_str(&format!("\"{ts}|{u1}\"")).unwrap();
    let expected_page2 = ConversationsPage {
        items: vec![ConversationItem {
            path: p1,
            head: head(u1),
        }],
        next_cursor: Some(expected_cursor2.clone()),
        num_scanned_files: 3, // scanned u3, u2 (anchor), u1
        reached_scan_cap: false,
    };
    assert_eq!(page2, expected_page2);
}

#[tokio::test]
async fn test_source_filter_excludes_non_matching_sessions() {
    let temp = TempDir::new().unwrap();
    let home = temp.path();

    let interactive_id = Uuid::from_u128(42);
    let non_interactive_id = Uuid::from_u128(77);

    write_session_file(
        home,
        "2025-08-02T10-00-00",
        interactive_id,
        2,
        Some(SessionSource::Cli),
    )
    .unwrap();
    write_session_file(
        home,
        "2025-08-01T10-00-00",
        non_interactive_id,
        2,
        Some(SessionSource::Exec),
    )
    .unwrap();

    let interactive_only = get_conversations(home, 10, None, INTERACTIVE_SESSION_SOURCES)
        .await
        .unwrap();
    let paths: Vec<_> = interactive_only
        .items
        .iter()
        .map(|item| item.path.as_path())
        .collect();

    assert_eq!(paths.len(), 1);
    assert!(paths.iter().all(|path| {
        path.ends_with("rollout-2025-08-02T10-00-00-00000000-0000-0000-0000-00000000002a.jsonl")
    }));

    let all_sessions = get_conversations(home, 10, None, NO_SOURCE_FILTER)
        .await
        .unwrap();
    let all_paths: Vec<_> = all_sessions
        .items
        .into_iter()
        .map(|item| item.path)
        .collect();
    assert_eq!(all_paths.len(), 2);
    assert!(all_paths.iter().any(|path| {
        path.ends_with("rollout-2025-08-02T10-00-00-00000000-0000-0000-0000-00000000002a.jsonl")
    }));
    assert!(all_paths.iter().any(|path| {
        path.ends_with("rollout-2025-08-01T10-00-00-00000000-0000-0000-0000-00000000004d.jsonl")
    }));
}
