//! Root of the `codex-core` library.
//! This module provides the core functionality for the codex CLI.

// Prevent accidental direct writes to stdout/stderr in library code. All
// user-visible output must go through the appropriate abstraction (e.g.,
// the TUI or the tracing stack).
#![deny(clippy::print_stdout, clippy::print_stderr)]

mod acp;
mod agent_defaults;
mod agent_tool;
mod apply_patch;
pub mod auth;
mod bash;
mod chat_completions;
mod client;
mod client_common;
pub mod codex;
pub mod debug_logger;
pub mod slash_commands;
pub use codex::Codex;
pub use codex::CodexSpawnOk;
mod codex_conversation;
mod conversation;
pub mod token_data;
// Export the real CodexConversation from conversation module
pub use conversation::CodexConversation;
pub mod config;
mod config_edit;
pub mod config_profile;
pub mod config_types;
mod conversation_history;
pub mod custom_prompts;
mod dry_run_guard;
mod environment_context;
pub mod error;
mod event_mapping;
pub mod exec;
mod exec_command;
pub mod exec_env;
mod flags;
pub mod git_info;
pub mod git_worktree;
mod hanzo_kong_manager;
mod internal_storage;
mod image_comparison;
mod is_safe_command;
pub mod landlock;
pub mod mcp_connection_manager;
mod mcp_tool_call;
mod message_history;
mod model_provider_info;
mod patch_harness;
mod project_features;
pub mod parse_command;
pub use model_provider_info::BUILT_IN_OSS_MODEL_PROVIDER_ID;
pub use model_provider_info::ModelProviderInfo;
pub use model_provider_info::WireApi;
pub use model_provider_info::built_in_model_providers;
pub use model_provider_info::create_oss_provider_with_base_url;
mod conversation_manager;
pub use codex_protocol::protocol::InitialHistory;
pub use conversation_manager::ConversationManager;
pub use conversation_manager::NewConversation;
// Re-export common auth types for workspace consumers
pub use auth::AuthManager;
pub use auth::CodexAuth;
pub mod default_client;
pub mod model_family;
mod openai_model_info;
pub mod compat;
mod openai_tools;
pub mod plan_tool;
pub mod project_doc;
mod rollout;
pub(crate) mod safety;
pub mod seatbelt;
pub mod shell;
pub mod spawn;
pub mod terminal;
mod tool_apply_patch;
mod truncate;
pub mod turn_diff_tracker;
mod user_instructions;
mod workflow_validation;
pub use rollout::list::ConversationsPage;
mod user_notification;
pub mod util;
pub mod http_client;
pub mod hanzo_integration;
pub mod qwen_model_info;
pub mod node_client;
pub mod engine_integration;
pub mod hanzo_inference;
pub mod hanzo_supabase;
pub use apply_patch::CODEX_APPLY_PATCH_ARG1;
pub use safety::get_platform_sandbox;
// Use our local protocol definitions to preserve custom events and input items.
pub mod protocol;
// Optionally expose upstream protocol config enums for callers that need them.
pub use codex_protocol::config_types as protocol_config_types;
// Re-export protocol models for compatibility with existing imports.
pub use codex_protocol::models as models;
// Public re-exports for API compatibility with downstream users and tests.
// Keep these stable to avoid breaking callers.
pub use crate::client::ModelClient;
pub use crate::client_common::Prompt;
pub use crate::client_common::TextFormat;
pub use crate::client_common::ResponseEvent;
pub use crate::client_common::ResponseStream;
pub use codex_protocol::models::ContentItem;
pub use codex_protocol::models::ReasoningItemContent;
pub use codex_protocol::models::ResponseItem;
