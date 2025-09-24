use crate::config_types::ReasoningSummaryFormat;
use crate::tool_apply_patch::ApplyPatchToolType;
use serde::{Deserialize, Serialize};

/// Model family enum for proper typing
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ModelFamilyType {
    GPT35,
    GPT4,
    GPT4O,
    GPT41,
    GPT5,
    GPT5Codex,
    O3,
    O4Mini,
    CodexMiniLatest,
    GptOss,
    QwenV2,
    QwenV3,
    Custom(String),
}

impl ModelFamilyType {
    pub fn from_slug(slug: &str) -> Self {
        if slug.starts_with("o3") {
            Self::O3
        } else if slug.starts_with("o4-mini") {
            Self::O4Mini
        } else if slug.starts_with("codex-mini-latest") {
            Self::CodexMiniLatest
        } else if slug.starts_with("gpt-4.1") {
            Self::GPT41
        } else if slug.starts_with("gpt-oss") {
            Self::GptOss
        } else if slug.starts_with("gpt-4o") {
            Self::GPT4O
        } else if slug.starts_with("gpt-3.5") {
            Self::GPT35
        } else if slug.starts_with("codex-") || slug.starts_with("gpt-5-codex") {
            Self::GPT5Codex
        } else if slug.starts_with("gpt-5") {
            Self::GPT5
        } else if slug.starts_with("qwen3") || slug.starts_with("Qwen3") {
            Self::QwenV3
        } else if slug.starts_with("qwen2") || slug.starts_with("Qwen2") {
            Self::QwenV2
        } else {
            Self::Custom(slug.to_string())
        }
    }
}

/// The `instructions` field in the payload sent to a model should always start
/// with this content.
pub const BASE_INSTRUCTIONS: &str = include_str!("../prompt.md");
const GPT_5_CODEX_INSTRUCTIONS: &str = include_str!("../gpt_5_codex_prompt.md");

/// A model family is a group of models that share certain characteristics.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModelFamily {
    /// The full model slug used to derive this model family, e.g.
    /// "gpt-4.1-2025-04-14".
    pub slug: String,

    /// The model family name, e.g. "gpt-4.1". Note this should able to be used
    /// with [`crate::openai_model_info::get_model_info`].
    pub family: String,

    /// True if the model needs additional instructions on how to use the
    /// "virtual" `apply_patch` CLI.
    pub needs_special_apply_patch_instructions: bool,

    // Whether the `reasoning` field can be set when making a request to this
    // model family. Note it has `effort` and `summary` subfields (though
    // `summary` is optional).
    pub supports_reasoning_summaries: bool,

    // Define if we need a special handling of reasoning summary
    pub reasoning_summary_format: ReasoningSummaryFormat,

    // This should be set to true when the model expects a tool named
    // "local_shell" to be provided. Its contract must be understood natively by
    // the model such that its description can be omitted.
    // See https://platform.openai.com/docs/guides/tools-local-shell
    pub uses_local_shell_tool: bool,

    /// Present if the model performs better when `apply_patch` is provided as
    /// a tool call instead of just a bash command
    pub apply_patch_tool_type: Option<ApplyPatchToolType>,

    // Instructions to use for querying the model
    pub base_instructions: String,
}

macro_rules! model_family {
    (
        $slug:expr, $family:expr $(, $key:ident : $value:expr )* $(,)?
    ) => {{
        // defaults
        let mut mf = ModelFamily {
            slug: $slug.to_string(),
            family: $family.to_string(),
            needs_special_apply_patch_instructions: false,
            supports_reasoning_summaries: false,
            reasoning_summary_format: ReasoningSummaryFormat::None,
            uses_local_shell_tool: false,
            apply_patch_tool_type: None,
            base_instructions: BASE_INSTRUCTIONS.to_string(),
        };
        // apply overrides
        $(
            mf.$key = $value;
        )*
        Some(mf)
    }};
}

/// Returns a `ModelFamily` for the given model slug, or `None` if the slug
/// does not match any known model family.
pub fn find_family_for_model(slug: &str) -> Option<ModelFamily> {
    if slug.starts_with("o3") {
        model_family!(
            slug, "o3",
            supports_reasoning_summaries: true,
            needs_special_apply_patch_instructions: true,
        )
    } else if slug.starts_with("o4-mini") {
        model_family!(
            slug, "o4-mini",
            supports_reasoning_summaries: true,
            needs_special_apply_patch_instructions: true,
        )
    } else if slug.starts_with("codex-mini-latest") {
        model_family!(
            slug, "codex-mini-latest",
            supports_reasoning_summaries: true,
            uses_local_shell_tool: true,
            needs_special_apply_patch_instructions: true,
        )
    } else if slug.starts_with("gpt-4.1") {
        model_family!(
            slug, "gpt-4.1",
            needs_special_apply_patch_instructions: true,
        )
    } else if slug.starts_with("gpt-oss") || slug.starts_with("openai/gpt-oss") {
        model_family!(slug, "gpt-oss", apply_patch_tool_type: Some(ApplyPatchToolType::Function))
    } else if slug.starts_with("gpt-4o") {
        model_family!(slug, "gpt-4o", needs_special_apply_patch_instructions: true)
    } else if slug.starts_with("gpt-3.5") {
        model_family!(slug, "gpt-3.5", needs_special_apply_patch_instructions: true)
    } else if slug.starts_with("codex-") || slug.starts_with("gpt-5-codex") {
        model_family!(
            slug, slug,
            supports_reasoning_summaries: true,
            reasoning_summary_format: ReasoningSummaryFormat::Detailed,
            base_instructions: GPT_5_CODEX_INSTRUCTIONS.to_string(),
        )
    } else if slug.starts_with("gpt-5") {
        model_family!(
            slug, "gpt-5",
            supports_reasoning_summaries: true,
            needs_special_apply_patch_instructions: true,
        )
    } else if slug.starts_with("qwen3") || slug.starts_with("Qwen3") || slug.starts_with("qwen/qwen3") {
        model_family!(
            slug, "qwen3",
            supports_reasoning_summaries: true,
            needs_special_apply_patch_instructions: true,
            apply_patch_tool_type: Some(ApplyPatchToolType::Function),
        )
    } else if slug.starts_with("qwen2") || slug.starts_with("Qwen2") || slug.starts_with("qwen/qwen2") {
        model_family!(
            slug, "qwen2",
            needs_special_apply_patch_instructions: true,
            apply_patch_tool_type: Some(ApplyPatchToolType::Function),
        )
    } else {
        None
    }
}

pub fn derive_default_model_family(model: &str) -> ModelFamily {
    ModelFamily {
        slug: model.to_string(),
        family: model.to_string(),
        needs_special_apply_patch_instructions: false,
        supports_reasoning_summaries: false,
        reasoning_summary_format: ReasoningSummaryFormat::None,
        uses_local_shell_tool: false,
        apply_patch_tool_type: None,
        base_instructions: BASE_INSTRUCTIONS.to_string(),
    }
}
