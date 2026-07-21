use codex_login::CODEX_API_KEY_ENV_VAR;
use std::path::Path;
use tempfile::TempDir;
use wiremock::MockServer;

pub struct TestCodexExecBuilder {
    home: TempDir,
    cwd: TempDir,
}

impl TestCodexExecBuilder {
    pub fn cmd(&self) -> assert_cmd::Command {
        let mut cmd = assert_cmd::Command::new(
            codex_utils_cargo_bin::cargo_bin("codex-exec")
                .expect("should find binary for codex-exec"),
        );
        cmd.current_dir(self.cwd.path())
            .env("CODEX_HOME", self.home.path())
            .env("CODEX_SQLITE_HOME", self.home.path())
            .env(CODEX_API_KEY_ENV_VAR, "dummy");
        cmd
    }
    pub fn cmd_with_server(&self, server: &MockServer) -> assert_cmd::Command {
        let mut cmd = self.cmd();
        for override_arg in mock_server_config_overrides(server) {
            cmd.arg("-c").arg(override_arg);
        }
        cmd
    }

    pub fn cwd_path(&self) -> &Path {
        self.cwd.path()
    }
    pub fn home_path(&self) -> &Path {
        self.home.path()
    }
}

fn toml_string_literal(value: &str) -> String {
    serde_json::to_string(value).expect("serialize TOML string literal")
}

/// Config overrides that point a spawned binary's model traffic at `server`,
/// independent of the production default provider (`hanzo`). Single source of
/// truth for "use this mock endpoint": the default `hanzo` provider would reach
/// the real api.hanzo.ai, so we set `openai_base_url` to the mock AND pin the
/// `openai` provider so that override is the one actually used. Consumed by
/// [`TestCodexExecBuilder::cmd_with_server`] and by tests that build their own
/// command (e.g. `resume`).
pub fn mock_server_config_overrides(server: &MockServer) -> Vec<String> {
    let base = format!("{}/v1", server.uri());
    vec![
        format!("openai_base_url={}", toml_string_literal(&base)),
        format!("model_provider={}", toml_string_literal("openai")),
    ]
}

pub fn test_codex_exec() -> TestCodexExecBuilder {
    TestCodexExecBuilder {
        home: TempDir::new().expect("create temp home"),
        cwd: TempDir::new().expect("create temp cwd"),
    }
}
