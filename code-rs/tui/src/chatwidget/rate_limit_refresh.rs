use std::sync::{Arc, Mutex};

use anyhow::{Context, Result};
use code_core::{AuthManager, ModelClient, Prompt, ResponseEvent};
use code_core::config::Config;
use code_core::config_types::ReasoningEffort;
use code_core::debug_logger::DebugLogger;
use code_core::protocol::{Event, EventMsg, RateLimitSnapshotEvent, TokenCountEvent};
use code_protocol::models::{ContentItem, ResponseItem};
use futures::StreamExt;
use tokio::runtime::Runtime;
use uuid::Uuid;

#[cfg(feature = "code-fork")]
use crate::tui_event_extensions::handle_rate_limit;

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;

/// Fire-and-forget helper that refreshes rate limit data using a dedicated model
/// request. Results are funneled back into the main TUI loop via `AppEvent` so
/// history ordering stays consistent.
pub(super) fn start_rate_limit_refresh(
    app_event_tx: AppEventSender,
    config: Config,
    debug_enabled: bool,
) {
    std::thread::spawn(move || {
        if let Err(err) = run_refresh(app_event_tx.clone(), config, debug_enabled) {
            let message = format!("Failed to refresh rate limits: {err}");
            app_event_tx.send(AppEvent::RateLimitFetchFailed { message });
        }
    });
}

fn run_refresh(
    app_event_tx: AppEventSender,
    config: Config,
    debug_enabled: bool,
) -> Result<()> {
    let runtime = build_runtime()?;
    runtime.block_on(async move {
        let auth_mode = if config.using_chatgpt_auth {
            code_protocol::mcp_protocol::AuthMode::ChatGPT
        } else {
            code_protocol::mcp_protocol::AuthMode::ApiKey
        };

        let auth_mgr = AuthManager::shared_with_mode_and_originator(
            config.code_home.clone(),
            auth_mode,
            config.responses_originator_header.clone(),
        );

        let client = build_model_client(&config, auth_mgr, debug_enabled)?;

        let mut prompt = Prompt::default();
        prompt.store = false;
        prompt.input.push(ResponseItem::Message {
            id: None,
            role: "user".to_string(),
            content: vec![ContentItem::InputText {
                text: "Yield immediately with only the message \"ok\"".to_string(),
            }],
        });

        let mut stream = client
            .stream(&prompt)
            .await
            .context("requesting rate limit snapshot")?;

        let mut snapshot = None;
        while let Some(event) = stream.next().await {
            match event? {
                ResponseEvent::RateLimits(s) => {
                    snapshot = Some(s);
                    break;
                }
                ResponseEvent::Completed { .. } => break,
                _ => {}
            }
        }

        let proto_snapshot = snapshot.context("rate limit snapshot missing from response")?;

        let snapshot: RateLimitSnapshotEvent = proto_snapshot.clone();

        #[cfg(feature = "code-fork")]
        handle_rate_limit(&snapshot, &app_event_tx);

        let event = Event {
            id: "rate-limit-refresh".to_string(),
            event_seq: 0,
            msg: EventMsg::TokenCount(TokenCountEvent {
                info: None,
                rate_limits: Some(snapshot),
            }),
            order: None,
        };

        app_event_tx.send(AppEvent::CodexEvent(event));
        Ok(())
    })
}

fn build_runtime() -> Result<Runtime> {
    Ok(
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .context("building rate limit refresh runtime")?,
    )
}

fn build_model_client(
    config: &Config,
    auth_mgr: Arc<AuthManager>,
    debug_enabled: bool,
) -> Result<ModelClient> {
    let debug_logger = DebugLogger::new(debug_enabled)
        .or_else(|_| DebugLogger::new(false))
        .context("initializing debug logger")?;

    let client = ModelClient::new(
        Arc::new(config.clone()),
        Some(auth_mgr),
        None,
        config.model_provider.clone(),
        ReasoningEffort::Low,
        config.model_reasoning_summary,
        config.model_text_verbosity,
        Uuid::new_v4(),
        Arc::new(Mutex::new(debug_logger)),
    );

    Ok(client)
}
