//! Rollout module: persistence and discovery of session rollout files.

pub const SESSIONS_SUBDIR: &str = "sessions";
#[allow(dead_code)]
pub const ARCHIVED_SESSIONS_SUBDIR: &str = "archived_sessions";

pub mod list;
pub(crate) mod policy;
pub mod recorder;

#[allow(unused_imports)]
pub use codex_protocol::protocol::SessionMeta;
#[allow(unused_imports)]
pub use list::find_conversation_path_by_id_str;
pub use recorder::RolloutRecorder;
#[allow(unused_imports)]
pub use recorder::RolloutRecorderParams;

#[cfg(test)]
pub mod tests;
