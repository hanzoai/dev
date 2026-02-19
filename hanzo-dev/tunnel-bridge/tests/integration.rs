//! Integration tests for the tunnel connection flow.
//!
//! These tests connect to a local bot gateway at ws://127.0.0.1:18789/v1/tunnel.
//! They will be skipped if the gateway is not running.

use hanzo_tunnel::{AppKind, TunnelConfig};
use std::time::Duration;

fn gateway_available() -> bool {
    std::net::TcpStream::connect_timeout(
        &"127.0.0.1:18789".parse().unwrap(),
        Duration::from_millis(500),
    )
    .is_ok()
}

#[tokio::test]
async fn connect_and_register_with_local_gateway() {
    if !gateway_available() {
        eprintln!("SKIP: local bot gateway not running on port 18789");
        return;
    }

    let config = TunnelConfig {
        relay_url: "ws://127.0.0.1:18789/v1/tunnel".into(),
        auth_token: "test-integration-key".into(),
        app_kind: AppKind::Dev,
        display_name: "integration-test".into(),
        capabilities: vec!["chat".into(), "exec".into()],
        commands: vec!["system.info".into(), "terminal.list".into()],
        cwd: std::env::current_dir().ok(),
        version: "test-0.0.1".into(),
        channel_size: 64,
    };

    let (conn, session_url) = hanzo_tunnel::connect_and_register(config)
        .await
        .expect("should connect to local gateway");

    // Verify we got a registration confirmation.
    assert!(
        !conn.instance_id.is_empty(),
        "instance_id should not be empty"
    );

    // Session URL should point to app.hanzo.bot.
    if let Some(ref url) = session_url {
        assert!(
            url.contains("app.hanzo.bot"),
            "session_url should contain app.hanzo.bot, got: {url}"
        );
        assert!(
            url.contains(&conn.instance_id),
            "session_url should contain instance_id"
        );
        eprintln!("  session_url = {url}");
    }

    eprintln!("  instance_id = {}", conn.instance_id);

    // Send a test event.
    conn.send_event("test.ping", serde_json::json!({"ts": "now"}))
        .await
        .expect("should send event");

    // Clean shutdown.
    conn.shutdown();
}

#[tokio::test]
async fn tunnel_reconnects_after_close() {
    if !gateway_available() {
        eprintln!("SKIP: local bot gateway not running on port 18789");
        return;
    }

    // Connect twice to verify reconnection works.
    for i in 0..2 {
        let config = TunnelConfig {
            relay_url: "ws://127.0.0.1:18789/v1/tunnel".into(),
            auth_token: "test-integration-key".into(),
            app_kind: AppKind::Dev,
            display_name: format!("reconnect-test-{i}"),
            capabilities: vec!["chat".into()],
            commands: vec![],
            version: "test-0.0.1".into(),
            ..Default::default()
        };

        let conn = hanzo_tunnel::connect(config)
            .await
            .expect("should connect");

        assert!(!conn.instance_id.is_empty());
        conn.shutdown();

        // Small delay between connections.
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
}

#[tokio::test]
async fn graceful_failure_when_gateway_unavailable() {
    // connect_and_register waits for the Registered response with a 10s timeout.
    // When the gateway is not reachable, the background transport will fail and
    // the recv_command will return None, causing connect_and_register to return
    // (conn, None) — no session URL but no panic.
    let config = TunnelConfig {
        relay_url: "ws://127.0.0.1:19999/v1/tunnel".into(),
        auth_token: "test-key".into(),
        app_kind: AppKind::Dev,
        display_name: "should-fail".into(),
        capabilities: vec![],
        commands: vec![],
        version: "test".into(),
        ..Default::default()
    };

    let result = hanzo_tunnel::connect_and_register(config).await;
    match result {
        Ok((conn, session_url)) => {
            // Connection object created but no session URL (server unreachable).
            assert!(
                session_url.is_none(),
                "should not have session_url when gateway is down"
            );
            conn.shutdown();
        }
        Err(_) => {
            // Also acceptable — connection failed outright.
        }
    }
}
