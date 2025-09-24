//! Qwen3 Model Support - All Variants
//! 
//! Supports:
//! - Qwen3-Next: Standard dense models (8B, 14B, 32B, 72B)
//! - Qwen3-MoE: Mixture of Experts models for efficiency
//! - Qwen3-MoE-Omni: Multimodal MoE with vision/audio capabilities
//! - Qwen3-Reranker: Document reranking model

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// All supported Qwen3 model variants
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Qwen3Model {
    // Standard Qwen3-Next dense models
    Qwen3Next8B,
    Qwen3Next14B,
    Qwen3Next32B,
    Qwen3Next72B,
    
    // Mixture of Experts models
    Qwen3MoE8x7B,      // 8 experts, 7B each
    Qwen3MoE16x7B,     // 16 experts, 7B each
    Qwen3MoE32x7B,     // 32 experts, 7B each
    
    // Multimodal MoE Omni models
    Qwen3MoEOmni8x7B,  // Text + Vision
    Qwen3MoEOmni16x7B, // Text + Vision + Audio
    Qwen3MoEOmni32x7B, // Text + Vision + Audio + 3D
    
    // Specialized models
    Qwen3Reranker,     // Document reranking
    Qwen3Coder,        // Code generation
    Qwen3Math,         // Mathematical reasoning
}

impl Qwen3Model {
    /// Get model identifier string
    pub fn model_id(&self) -> &str {
        match self {
            // Dense models
            Self::Qwen3Next8B => "qwen3-next:8b",
            Self::Qwen3Next14B => "qwen3-next:14b",
            Self::Qwen3Next32B => "qwen3-next:32b",
            Self::Qwen3Next72B => "qwen3-next:72b",
            
            // MoE models
            Self::Qwen3MoE8x7B => "qwen3-moe:8x7b",
            Self::Qwen3MoE16x7B => "qwen3-moe:16x7b",
            Self::Qwen3MoE32x7B => "qwen3-moe:32x7b",
            
            // MoE Omni models
            Self::Qwen3MoEOmni8x7B => "qwen3-moe-omni:8x7b",
            Self::Qwen3MoEOmni16x7B => "qwen3-moe-omni:16x7b",
            Self::Qwen3MoEOmni32x7B => "qwen3-moe-omni:32x7b",
            
            // Specialized
            Self::Qwen3Reranker => "qwen3-reranker",
            Self::Qwen3Coder => "qwen3-coder",
            Self::Qwen3Math => "qwen3-math",
        }
    }
    
    /// Get minimum GPU memory required (GB)
    pub fn min_gpu_memory_gb(&self) -> u32 {
        match self {
            Self::Qwen3Next8B => 16,
            Self::Qwen3Next14B => 24,
            Self::Qwen3Next32B => 48,
            Self::Qwen3Next72B => 80,
            
            // MoE models are more efficient
            Self::Qwen3MoE8x7B => 24,
            Self::Qwen3MoE16x7B => 32,
            Self::Qwen3MoE32x7B => 48,
            
            // Omni models need more for multimodal
            Self::Qwen3MoEOmni8x7B => 32,
            Self::Qwen3MoEOmni16x7B => 48,
            Self::Qwen3MoEOmni32x7B => 80,
            
            Self::Qwen3Reranker => 8,
            Self::Qwen3Coder => 16,
            Self::Qwen3Math => 16,
        }
    }
    
    /// Check if model supports multimodal input
    pub fn is_multimodal(&self) -> bool {
        matches!(self, 
            Self::Qwen3MoEOmni8x7B | 
            Self::Qwen3MoEOmni16x7B | 
            Self::Qwen3MoEOmni32x7B
        )
    }
    
    /// Check if model uses Mixture of Experts
    pub fn is_moe(&self) -> bool {
        matches!(self,
            Self::Qwen3MoE8x7B |
            Self::Qwen3MoE16x7B |
            Self::Qwen3MoE32x7B |
            Self::Qwen3MoEOmni8x7B |
            Self::Qwen3MoEOmni16x7B |
            Self::Qwen3MoEOmni32x7B
        )
    }
    
    /// Get supported modalities
    pub fn modalities(&self) -> Vec<Modality> {
        match self {
            Self::Qwen3MoEOmni8x7B => vec![Modality::Text, Modality::Vision],
            Self::Qwen3MoEOmni16x7B => vec![Modality::Text, Modality::Vision, Modality::Audio],
            Self::Qwen3MoEOmni32x7B => vec![Modality::Text, Modality::Vision, Modality::Audio, Modality::ThreeD],
            _ => vec![Modality::Text],
        }
    }
}

/// Input modalities
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Modality {
    Text,
    Vision,
    Audio,
    ThreeD,
}

/// Node operator capabilities for Qwen3 models
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Qwen3Capabilities {
    pub supported_models: Vec<Qwen3Model>,
    pub gpu_memory_gb: u32,
    pub cpu_cores: u32,
    pub supports_moe: bool,
    pub supports_multimodal: bool,
    pub max_batch_size: u32,
    pub max_sequence_length: u32,
}

impl Qwen3Capabilities {
    /// Check if operator can run a specific model
    pub fn can_run_model(&self, model: &Qwen3Model) -> bool {
        // Check GPU memory
        if self.gpu_memory_gb < model.min_gpu_memory_gb() {
            return false;
        }
        
        // Check MoE support
        if model.is_moe() && !self.supports_moe {
            return false;
        }
        
        // Check multimodal support
        if model.is_multimodal() && !self.supports_multimodal {
            return false;
        }
        
        // Check if explicitly supported
        self.supported_models.contains(model)
    }
    
    /// Get all models this operator can run
    pub fn available_models(&self) -> Vec<Qwen3Model> {
        self.supported_models
            .iter()
            .filter(|m| self.can_run_model(m))
            .cloned()
            .collect()
    }
}

/// Inference request for Qwen3 models
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Qwen3InferenceRequest {
    pub model: Qwen3Model,
    pub inputs: InferenceInputs,
    pub parameters: InferenceParameters,
}

/// Multimodal inputs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InferenceInputs {
    Text(String),
    TextWithImage { text: String, image_base64: String },
    TextWithAudio { text: String, audio_base64: String },
    Multimodal {
        text: String,
        image_base64: Option<String>,
        audio_base64: Option<String>,
        point_cloud: Option<Vec<f32>>, // For 3D
    },
    // For reranking
    Rerank {
        query: String,
        documents: Vec<String>,
    },
}

/// Inference parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceParameters {
    pub max_tokens: u32,
    pub temperature: f32,
    pub top_p: f32,
    pub top_k: u32,
    pub repetition_penalty: f32,
    pub stop_sequences: Vec<String>,
    
    // MoE specific
    pub num_experts_per_token: Option<u32>, // For MoE models
    pub expert_routing_threshold: Option<f32>,
}

impl Default for InferenceParameters {
    fn default() -> Self {
        Self {
            max_tokens: 2048,
            temperature: 0.7,
            top_p: 0.9,
            top_k: 40,
            repetition_penalty: 1.1,
            stop_sequences: vec![],
            num_experts_per_token: Some(2), // Default for MoE
            expert_routing_threshold: Some(0.1),
        }
    }
}

/// Response from Qwen3 inference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Qwen3InferenceResponse {
    pub model: Qwen3Model,
    pub output: InferenceOutput,
    pub usage: TokenUsage,
    pub latency_ms: u64,
    pub node_operator: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InferenceOutput {
    Text(String),
    RankedDocuments(Vec<RankedDocument>),
    Multimodal {
        text: String,
        generated_image: Option<String>, // base64
        audio_transcription: Option<String>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RankedDocument {
    pub index: usize,
    pub score: f32,
    pub document: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenUsage {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
    pub active_experts: Option<u32>, // For MoE models
}

/// Model registry for node operators
pub struct Qwen3ModelRegistry {
    models: HashMap<String, ModelInfo>,
}

#[derive(Debug, Clone)]
struct ModelInfo {
    model: Qwen3Model,
    operators: Vec<String>, // Node operator addresses
    total_capacity: u32,    // Total inference capacity
    current_load: f32,      // Current utilization
}

impl Qwen3ModelRegistry {
    pub fn new() -> Self {
        let mut models = HashMap::new();
        
        // Initialize all models
        for model in [
            Qwen3Model::Qwen3Next8B,
            Qwen3Model::Qwen3Next14B,
            Qwen3Model::Qwen3Next32B,
            Qwen3Model::Qwen3Next72B,
            Qwen3Model::Qwen3MoE8x7B,
            Qwen3Model::Qwen3MoE16x7B,
            Qwen3Model::Qwen3MoE32x7B,
            Qwen3Model::Qwen3MoEOmni8x7B,
            Qwen3Model::Qwen3MoEOmni16x7B,
            Qwen3Model::Qwen3MoEOmni32x7B,
            Qwen3Model::Qwen3Reranker,
            Qwen3Model::Qwen3Coder,
            Qwen3Model::Qwen3Math,
        ] {
            models.insert(
                model.model_id().to_string(),
                ModelInfo {
                    model: model.clone(),
                    operators: Vec::new(),
                    total_capacity: 0,
                    current_load: 0.0,
                },
            );
        }
        
        Self { models }
    }
    
    /// Register a node operator for specific models
    pub fn register_operator(&mut self, operator: String, capabilities: &Qwen3Capabilities) {
        for model in &capabilities.supported_models {
            if let Some(info) = self.models.get_mut(model.model_id()) {
                if !info.operators.contains(&operator) {
                    info.operators.push(operator.clone());
                    info.total_capacity += capabilities.max_batch_size;
                }
            }
        }
    }
    
    /// Find best operator for a model
    pub fn find_operator(&self, model: &Qwen3Model) -> Option<String> {
        self.models
            .get(model.model_id())
            .and_then(|info| {
                // Select operator with lowest load
                info.operators.first().cloned()
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_all_qwen3_models() {
        // Test standard dense models
        assert_eq!(Qwen3Model::Qwen3Next8B.model_id(), "qwen3-next:8b");
        assert_eq!(Qwen3Model::Qwen3Next14B.model_id(), "qwen3-next:14b");
        assert_eq!(Qwen3Model::Qwen3Next32B.model_id(), "qwen3-next:32b");
        assert_eq!(Qwen3Model::Qwen3Next72B.model_id(), "qwen3-next:72b");
        
        // Test MoE models
        assert_eq!(Qwen3Model::Qwen3MoE8x7B.model_id(), "qwen3-moe:8x7b");
        assert_eq!(Qwen3Model::Qwen3MoE16x7B.model_id(), "qwen3-moe:16x7b");
        assert_eq!(Qwen3Model::Qwen3MoE32x7B.model_id(), "qwen3-moe:32x7b");
        
        // Test MoE Omni models
        assert_eq!(Qwen3Model::Qwen3MoEOmni8x7B.model_id(), "qwen3-moe-omni:8x7b");
        assert_eq!(Qwen3Model::Qwen3MoEOmni16x7B.model_id(), "qwen3-moe-omni:16x7b");
        assert_eq!(Qwen3Model::Qwen3MoEOmni32x7B.model_id(), "qwen3-moe-omni:32x7b");
        
        // Test specialized models
        assert_eq!(Qwen3Model::Qwen3Reranker.model_id(), "qwen3-reranker");
        assert_eq!(Qwen3Model::Qwen3Coder.model_id(), "qwen3-coder");
        assert_eq!(Qwen3Model::Qwen3Math.model_id(), "qwen3-math");
    }
    
    #[test]
    fn test_gpu_memory_requirements() {
        assert_eq!(Qwen3Model::Qwen3Next8B.min_gpu_memory_gb(), 16);
        assert_eq!(Qwen3Model::Qwen3Next72B.min_gpu_memory_gb(), 80);
        assert_eq!(Qwen3Model::Qwen3MoE8x7B.min_gpu_memory_gb(), 24);
        assert_eq!(Qwen3Model::Qwen3MoEOmni32x7B.min_gpu_memory_gb(), 80);
        assert_eq!(Qwen3Model::Qwen3Reranker.min_gpu_memory_gb(), 8);
    }
    
    #[test]
    fn test_moe_detection() {
        assert!(!Qwen3Model::Qwen3Next8B.is_moe());
        assert!(Qwen3Model::Qwen3MoE8x7B.is_moe());
        assert!(Qwen3Model::Qwen3MoEOmni16x7B.is_moe());
        assert!(!Qwen3Model::Qwen3Reranker.is_moe());
    }
    
    #[test]
    fn test_multimodal_detection() {
        assert!(!Qwen3Model::Qwen3Next8B.is_multimodal());
        assert!(!Qwen3Model::Qwen3MoE8x7B.is_multimodal());
        assert!(Qwen3Model::Qwen3MoEOmni8x7B.is_multimodal());
        assert!(Qwen3Model::Qwen3MoEOmni16x7B.is_multimodal());
        assert!(Qwen3Model::Qwen3MoEOmni32x7B.is_multimodal());
    }
    
    #[test]
    fn test_modalities() {
        let text_only = Qwen3Model::Qwen3Next8B.modalities();
        assert_eq!(text_only, vec![Modality::Text]);
        
        let vision = Qwen3Model::Qwen3MoEOmni8x7B.modalities();
        assert_eq!(vision, vec![Modality::Text, Modality::Vision]);
        
        let audio = Qwen3Model::Qwen3MoEOmni16x7B.modalities();
        assert_eq!(audio, vec![Modality::Text, Modality::Vision, Modality::Audio]);
        
        let full = Qwen3Model::Qwen3MoEOmni32x7B.modalities();
        assert_eq!(full, vec![Modality::Text, Modality::Vision, Modality::Audio, Modality::ThreeD]);
    }
    
    #[test]
    fn test_operator_capabilities() {
        let caps = Qwen3Capabilities {
            supported_models: vec![
                Qwen3Model::Qwen3Next8B,
                Qwen3Model::Qwen3MoE8x7B,
                Qwen3Model::Qwen3MoEOmni8x7B,
            ],
            gpu_memory_gb: 48,
            cpu_cores: 32,
            supports_moe: true,
            supports_multimodal: true,
            max_batch_size: 32,
            max_sequence_length: 32768,
        };
        
        // Can run models with enough GPU
        assert!(caps.can_run_model(&Qwen3Model::Qwen3Next8B));
        assert!(caps.can_run_model(&Qwen3Model::Qwen3MoE8x7B));
        assert!(caps.can_run_model(&Qwen3Model::Qwen3MoEOmni8x7B));
        
        // Cannot run models with insufficient GPU
        assert!(!caps.can_run_model(&Qwen3Model::Qwen3Next72B)); // Needs 80GB
        assert!(!caps.can_run_model(&Qwen3Model::Qwen3MoEOmni32x7B)); // Needs 80GB
    }
    
    #[test]
    fn test_model_registry() {
        let mut registry = Qwen3ModelRegistry::new();
        
        let caps = Qwen3Capabilities {
            supported_models: vec![
                Qwen3Model::Qwen3Next8B,
                Qwen3Model::Qwen3MoE8x7B,
            ],
            gpu_memory_gb: 32,
            cpu_cores: 16,
            supports_moe: true,
            supports_multimodal: false,
            max_batch_size: 16,
            max_sequence_length: 16384,
        };
        
        registry.register_operator("operator-1".to_string(), &caps);
        
        // Find operator for supported model
        let op = registry.find_operator(&Qwen3Model::Qwen3Next8B);
        assert_eq!(op, Some("operator-1".to_string()));
        
        // No operator for unsupported model
        let op = registry.find_operator(&Qwen3Model::Qwen3Next72B);
        assert_eq!(op, None);
    }
}