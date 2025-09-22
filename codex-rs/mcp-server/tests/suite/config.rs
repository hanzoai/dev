use std::path::Path;

use mcp_test_support::McpProcess;
use mcp_test_support::to_response;
use mcp_types::JSONRPCResponse;
use mcp_types::RequestId;
use pretty_assertions::assert_eq;
use serde_json::json;
use tempfile::TempDir;
use tokio::time::timeout;

const DEFAULT_READ_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(10);

fn create_config_toml(codex_home: &Path) -> std::io::Result<()> {
    let config_toml = codex_home.join("config.toml");
    std::fs::write(
        config_toml,
        r#"
approval_policy = "on-request"
sandbox_mode = "workspace-write"
model_reasoning_effort = "high"
profile = "test"

[profiles.test]
model = "gpt-4o"
approval_policy = "on-request"
model_reasoning_effort = "high"
model_reasoning_summary = "detailed"
"#,
    )
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn get_config_toml_returns_subset() {
    let codex_home = TempDir::new().unwrap_or_else(|e| panic!("create tempdir: {e}"));
    create_config_toml(codex_home.path()).expect("write config.toml");

    let mut mcp = McpProcess::new(codex_home.path())
        .await
        .expect("spawn mcp process");
    timeout(DEFAULT_READ_TIMEOUT, mcp.initialize())
        .await
        .expect("init timeout")
        .expect("init failed");

    let request_id = mcp
        .send_get_config_toml_request()
        .await
        .expect("send getConfigToml");
    let resp: JSONRPCResponse = timeout(
        DEFAULT_READ_TIMEOUT,
        mcp.read_stream_until_response_message(RequestId::Integer(request_id)),
    )
    .await
    .expect("getConfigToml timeout")
    .expect("getConfigToml response");

    let config: serde_json::Value = to_response(resp).expect("deserialize config");
    let expected = json!({
        "approval_policy": "on-request",
        "sandbox_mode": "workspace-write",
        "model_reasoning_effort": "high",
        "profile": "test",
        "profiles": {
            "test": {
                "model": "gpt-4o",
                "approval_policy": "on-request",
                "model_reasoning_effort": "high"
            }
        }
    });

    assert_eq!(expected, config);
}
