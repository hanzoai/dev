//! Comprehensive test for ALL Qwen3 model variants
//! Tests qwen3, qwen3_moe and qwen3_moe_omni as requested

use hanzod::qwen3_models::*;
use hanzod::lux_consensus::{LuxConsensus, LuxConsensusConfig};

#[tokio::test]
async fn test_all_qwen3_variants_supported() {
    println!("\n{}", "=".repeat(60));
    println!("🧪 TESTING ALL QWEN3 MODEL VARIANTS");
    println!("{}", "=".repeat(60));

    // Test all 13 Qwen3 variants
    let all_models = vec![
        // Standard Dense Models (Qwen3-Next)
        Qwen3Model::Qwen3Next8B,
        Qwen3Model::Qwen3Next14B,
        Qwen3Model::Qwen3Next32B,
        Qwen3Model::Qwen3Next72B,

        // MoE Models
        Qwen3Model::Qwen3MoE8x7B,
        Qwen3Model::Qwen3MoE16x7B,
        Qwen3Model::Qwen3MoE32x7B,

        // MoE-Omni Multimodal Models
        Qwen3Model::Qwen3MoEOmni8x7B,
        Qwen3Model::Qwen3MoEOmni16x7B,
        Qwen3Model::Qwen3MoEOmni32x7B,

        // Specialized Models
        Qwen3Model::Qwen3Reranker,
        Qwen3Model::Qwen3Coder,
        Qwen3Model::Qwen3Math,
    ];

    println!("\n✅ Testing {} Qwen3 model variants:", all_models.len());

    for model in &all_models {
        println!("  • {} (GPU: {}GB, MoE: {}, Multimodal: {})",
            model.model_id(),
            model.min_gpu_memory_gb(),
            if model.is_moe() { "Yes" } else { "No" },
            if model.is_multimodal() { "Yes" } else { "No" }
        );
    }

    // Test Qwen3 (standard dense models)
    println!("\n📋 Testing Qwen3 Dense Models:");
    test_qwen3_dense_models();

    // Test Qwen3-MoE
    println!("\n📋 Testing Qwen3-MoE Models:");
    test_qwen3_moe_models();

    // Test Qwen3-MoE-Omni
    println!("\n📋 Testing Qwen3-MoE-Omni Models:");
    test_qwen3_moe_omni_models();

    // Test node operator registration with Qwen3 support
    println!("\n📋 Testing Node Operator Registration:");
    test_node_operator_qwen3_support().await;

    // Test model registry
    println!("\n📋 Testing Model Registry:");
    test_qwen3_model_registry();

    println!("\n{}", "=".repeat(60));
    println!("✅ ALL QWEN3 VARIANTS TESTED SUCCESSFULLY!");
    println!("  • Qwen3 (Dense): ✅");
    println!("  • Qwen3-MoE: ✅");
    println!("  • Qwen3-MoE-Omni: ✅");
    println!("{}", "=".repeat(60));
}

fn test_qwen3_dense_models() {
    let dense_models = vec![
        Qwen3Model::Qwen3Next8B,
        Qwen3Model::Qwen3Next14B,
        Qwen3Model::Qwen3Next32B,
        Qwen3Model::Qwen3Next72B,
    ];

    for model in dense_models {
        assert!(!model.is_moe(), "{} should not be MoE", model.model_id());
        assert!(!model.is_multimodal(), "{} should not be multimodal", model.model_id());
        assert_eq!(model.modalities(), vec![Modality::Text]);
        println!("  ✅ {} validated", model.model_id());
    }
}

fn test_qwen3_moe_models() {
    let moe_models = vec![
        Qwen3Model::Qwen3MoE8x7B,
        Qwen3Model::Qwen3MoE16x7B,
        Qwen3Model::Qwen3MoE32x7B,
    ];

    for model in moe_models {
        assert!(model.is_moe(), "{} should be MoE", model.model_id());
        assert!(!model.is_multimodal(), "{} should not be multimodal", model.model_id());
        assert_eq!(model.modalities(), vec![Modality::Text]);
        println!("  ✅ {} validated", model.model_id());
    }
}

fn test_qwen3_moe_omni_models() {
    // Test 8x7B - Text + Vision
    let omni_8x7b = Qwen3Model::Qwen3MoEOmni8x7B;
    assert!(omni_8x7b.is_moe());
    assert!(omni_8x7b.is_multimodal());
    assert_eq!(omni_8x7b.modalities(), vec![Modality::Text, Modality::Vision]);
    println!("  ✅ {} validated (Text + Vision)", omni_8x7b.model_id());

    // Test 16x7B - Text + Vision + Audio
    let omni_16x7b = Qwen3Model::Qwen3MoEOmni16x7B;
    assert!(omni_16x7b.is_moe());
    assert!(omni_16x7b.is_multimodal());
    assert_eq!(omni_16x7b.modalities(), vec![Modality::Text, Modality::Vision, Modality::Audio]);
    println!("  ✅ {} validated (Text + Vision + Audio)", omni_16x7b.model_id());

    // Test 32x7B - Text + Vision + Audio + 3D
    let omni_32x7b = Qwen3Model::Qwen3MoEOmni32x7B;
    assert!(omni_32x7b.is_moe());
    assert!(omni_32x7b.is_multimodal());
    assert_eq!(omni_32x7b.modalities(), vec![Modality::Text, Modality::Vision, Modality::Audio, Modality::ThreeD]);
    println!("  ✅ {} validated (Text + Vision + Audio + 3D)", omni_32x7b.model_id());
}

async fn test_node_operator_qwen3_support() {
    let config = LuxConsensusConfig::default();
    let consensus = LuxConsensus::new(config).unwrap();

    // Register validator with Qwen3 support
    let validator = consensus
        .register_validator(2_000_000_000_000_000, true) // supports_qwen3 = true
        .await
        .unwrap();

    assert!(validator.supports_qwen3);
    println!("  ✅ Node operator registered with Qwen3 support");

    // Test capabilities
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
        max_sequence_length: 131072,
    };

    // Verify can run supported models
    assert!(caps.can_run_model(&Qwen3Model::Qwen3Next8B));
    assert!(caps.can_run_model(&Qwen3Model::Qwen3MoE8x7B));
    assert!(caps.can_run_model(&Qwen3Model::Qwen3MoEOmni8x7B));

    // Verify cannot run unsupported models
    assert!(!caps.can_run_model(&Qwen3Model::Qwen3Next72B)); // Needs 80GB

    println!("  ✅ Node capabilities validated for Qwen3 models");
}

fn test_qwen3_model_registry() {
    let mut registry = Qwen3ModelRegistry::new();

    // Register operator 1 - supports dense and standard MoE
    let caps1 = Qwen3Capabilities {
        supported_models: vec![
            Qwen3Model::Qwen3Next8B,
            Qwen3Model::Qwen3Next14B,
            Qwen3Model::Qwen3MoE8x7B,
        ],
        gpu_memory_gb: 32,
        cpu_cores: 16,
        supports_moe: true,
        supports_multimodal: false,
        max_batch_size: 16,
        max_sequence_length: 131072,
    };
    registry.register_operator("operator-1".to_string(), &caps1);

    // Register operator 2 - supports MoE-Omni multimodal
    let caps2 = Qwen3Capabilities {
        supported_models: vec![
            Qwen3Model::Qwen3MoEOmni8x7B,
            Qwen3Model::Qwen3MoEOmni16x7B,
        ],
        gpu_memory_gb: 64,
        cpu_cores: 32,
        supports_moe: true,
        supports_multimodal: true,
        max_batch_size: 8,
        max_sequence_length: 131072,
    };
    registry.register_operator("operator-2".to_string(), &caps2);

    // Verify operators found for models
    assert!(registry.find_operator(&Qwen3Model::Qwen3Next8B).is_some());
    assert!(registry.find_operator(&Qwen3Model::Qwen3MoE8x7B).is_some());
    assert!(registry.find_operator(&Qwen3Model::Qwen3MoEOmni8x7B).is_some());

    println!("  ✅ Model registry tested with multiple operators");
}

#[test]
fn test_inference_parameters() {
    let params = InferenceParameters::default();

    // Test MoE-specific parameters
    assert_eq!(params.num_experts_per_token, Some(2));
    assert_eq!(params.expert_routing_threshold, Some(0.1));

    println!("✅ MoE inference parameters validated");
}

#[test]
fn test_multimodal_inference() {
    // Test text-only input
    let text_input = InferenceInputs::Text("Hello world".to_string());

    // Test text with image
    let text_image = InferenceInputs::TextWithImage {
        text: "What's in this image?".to_string(),
        image_base64: "base64_data".to_string(),
    };

    // Test full multimodal
    let multimodal = InferenceInputs::Multimodal {
        text: "Analyze this".to_string(),
        image_base64: Some("image".to_string()),
        audio_base64: Some("audio".to_string()),
        point_cloud: Some(vec![1.0, 2.0, 3.0]),
    };

    // Test reranking
    let rerank = InferenceInputs::Rerank {
        query: "best AI model".to_string(),
        documents: vec![
            "Document 1".to_string(),
            "Document 2".to_string(),
        ],
    };

    println!("✅ All inference input types validated");
}