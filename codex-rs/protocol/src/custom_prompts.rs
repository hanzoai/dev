use serde::Deserialize;
use serde::Serialize;
use ts_rs::TS;
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Debug, Clone, TS)]
pub struct CustomPrompt {
    pub name: String,
    pub path: PathBuf,
    pub content: String,
}
