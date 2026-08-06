use std::path::Path;

use code_core::config::Config;
use code_core::protocol::Event;
use code_core::protocol::SessionConfiguredEvent;

pub(crate) enum CodexStatus {
    Running,
    InitiateShutdown,
    Shutdown,
}

pub(crate) trait EventProcessor {
    /// Print summary of effective configuration and user prompt.
    fn print_config_summary(&mut self, config: &Config, prompt: &str);

    /// The session handshake. `exec` consumes `SessionConfigured` before the
    /// event loop starts, so a processor that needs it — JSON mode, which
    /// derives `thread.started` from it — has to be handed it here.
    fn on_session_configured(&mut self, _ev: &SessionConfiguredEvent) {}

    /// Handle a single event emitted by the agent.
    fn process_event(&mut self, event: Event) -> CodexStatus;

    // No exit_code method; CLI controls process exit based on core events.
}

pub(crate) fn handle_last_message(last_agent_message: Option<&str>, output_file: &Path) {
    let message = last_agent_message.unwrap_or_default();
    write_last_message_file(message, Some(output_file));
    if last_agent_message.is_none() {
        eprintln!(
            "Warning: no last agent message; wrote empty content to {}",
            output_file.display()
        );
    }
}

fn write_last_message_file(contents: &str, last_message_path: Option<&Path>) {
    if let Some(path) = last_message_path {
        if let Err(e) = std::fs::write(path, contents) {
            eprintln!("Failed to write last message file {path:?}: {e}");
        }
    }
}
