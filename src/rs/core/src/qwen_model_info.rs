// Qwen3 model information and configuration
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QwenModelInfo {
    pub name: String,
    pub family: String,  // Changed from ModelFamily to String
    pub context_window: u64,
    pub max_output_tokens: u64,
    pub supports_thinking: bool,
    pub supports_reranking: bool,
    pub supports_tools: bool,
    pub supports_web_search: bool,
}

impl QwenModelInfo {
    pub fn qwen3_8b() -> Self {
        Self {
            name: "Qwen3-8B".to_string(),
            family: "qwen3".to_string(),
            context_window: 32768,
            max_output_tokens: 8192,
            supports_thinking: true,
            supports_reranking: false,
            supports_tools: true,
            supports_web_search: true,
        }
    }
    
    pub fn qwen3_14b() -> Self {
        Self {
            name: "Qwen3-14B".to_string(),
            family: "qwen3".to_string(),
            context_window: 32768,
            max_output_tokens: 8192,
            supports_thinking: true,
            supports_reranking: false,
            supports_tools: true,
            supports_web_search: true,
        }
    }
    
    pub fn qwen3_30b_a3b() -> Self {
        Self {
            name: "Qwen3-30B-A3B".to_string(),
            family: "qwen3".to_string(),
            context_window: 32768,
            max_output_tokens: 8192,
            supports_thinking: true,
            supports_reranking: false,
            supports_tools: true,
            supports_web_search: true,
        }
    }
    
    pub fn qwen3_72b() -> Self {
        Self {
            name: "Qwen3-72B".to_string(),
            family: "qwen3".to_string(),
            context_window: 131072,
            max_output_tokens: 32768,
            supports_thinking: true,
            supports_reranking: false,
            supports_tools: true,
            supports_web_search: true,
        }
    }
    
    pub fn qwen3_235b() -> Self {
        Self {
            name: "Qwen3-235B".to_string(),
            family: "qwen3".to_string(),
            context_window: 131072,
            max_output_tokens: 32768,
            supports_thinking: true,
            supports_reranking: false,
            supports_tools: true,
            supports_web_search: true,
        }
    }
    
    pub fn qwen3_reranker() -> Self {
        Self {
            name: "Qwen3-Reranker".to_string(),
            family: "qwen3".to_string(),
            context_window: 8192,
            max_output_tokens: 0, // Reranker doesn't generate text
            supports_thinking: false,
            supports_reranking: true,
            supports_tools: false,
            supports_web_search: false,
        }
    }
    
    pub fn qwen2_vl() -> Self {
        Self {
            name: "Qwen2-VL".to_string(),
            family: "qwen3".to_string(),
            context_window: 32768,
            max_output_tokens: 8192,
            supports_thinking: false,
            supports_reranking: false,
            supports_tools: true,
            supports_web_search: false,
        }
    }
}

/// Get Qwen model info by name
pub fn get_qwen_model_info(model_name: &str) -> Option<QwenModelInfo> {
    match model_name.to_lowercase().as_str() {
        "qwen3-8b" | "qwen/qwen3-8b" => Some(QwenModelInfo::qwen3_8b()),
        "qwen3-14b" | "qwen/qwen3-14b" => Some(QwenModelInfo::qwen3_14b()),
        "qwen3-30b-a3b" | "qwen/qwen3-30b-a3b" => Some(QwenModelInfo::qwen3_30b_a3b()),
        "qwen3-72b" | "qwen/qwen3-72b" => Some(QwenModelInfo::qwen3_72b()),
        "qwen3-235b" | "qwen/qwen3-235b" => Some(QwenModelInfo::qwen3_235b()),
        "qwen3-reranker" | "qwen/qwen3-reranker" => Some(QwenModelInfo::qwen3_reranker()),
        "qwen2-vl" | "qwen/qwen2-vl" => Some(QwenModelInfo::qwen2_vl()),
        _ => None,
    }
}

/// Check if a model name is a Qwen model
pub fn is_qwen_model(model_name: &str) -> bool {
    model_name.to_lowercase().contains("qwen")
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_qwen_model_detection() {
        assert!(is_qwen_model("Qwen3-8B"));
        assert!(is_qwen_model("qwen/qwen3-72b"));
        assert!(is_qwen_model("Qwen3-Reranker"));
        assert!(!is_qwen_model("gpt-4"));
        assert!(!is_qwen_model("claude-3"));
    }
    
    #[test]
    fn test_get_qwen_model_info() {
        let info = get_qwen_model_info("qwen3-8b").unwrap();
        assert_eq!(info.name, "Qwen3-8B");
        assert_eq!(info.context_window, 32768);
        assert!(info.supports_thinking);
        assert!(info.supports_tools);
    }
}