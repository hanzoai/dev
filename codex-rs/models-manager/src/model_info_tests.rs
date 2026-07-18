use super::*;
use crate::ModelsManagerConfig;
use pretty_assertions::assert_eq;

#[test]
fn reasoning_summaries_override_true_enables_support() {
    let model = model_info_from_slug("unknown-model");
    let config = ModelsManagerConfig {
        model_supports_reasoning_summaries: Some(true),
        ..Default::default()
    };

    let updated = with_config_overrides(model.clone(), &config);
    let mut expected = model;
    expected.supports_reasoning_summaries = true;

    assert_eq!(updated, expected);
}

#[test]
fn reasoning_summaries_override_false_does_not_disable_support() {
    let mut model = model_info_from_slug("unknown-model");
    model.supports_reasoning_summaries = true;
    let config = ModelsManagerConfig {
        model_supports_reasoning_summaries: Some(false),
        ..Default::default()
    };

    let updated = with_config_overrides(model.clone(), &config);

    assert_eq!(updated, model);
}

#[test]
fn reasoning_summaries_override_false_is_noop_when_model_is_false() {
    let model = model_info_from_slug("unknown-model");
    let config = ModelsManagerConfig {
        model_supports_reasoning_summaries: Some(false),
        ..Default::default()
    };

    let updated = with_config_overrides(model.clone(), &config);

    assert_eq!(updated, model);
}

#[test]
fn model_context_window_override_clamps_to_max_context_window() {
    let mut model = model_info_from_slug("unknown-model");
    model.context_window = Some(273_000);
    model.max_context_window = Some(400_000);
    let config = ModelsManagerConfig {
        model_context_window: Some(500_000),
        ..Default::default()
    };

    let updated = with_config_overrides(model.clone(), &config);
    let mut expected = model;
    expected.context_window = Some(400_000);

    assert_eq!(updated, expected);
}

#[test]
fn model_context_window_uses_model_value_without_override() {
    let mut model = model_info_from_slug("unknown-model");
    model.context_window = Some(273_000);
    model.max_context_window = Some(400_000);
    let config = ModelsManagerConfig::default();

    let updated = with_config_overrides(model.clone(), &config);

    assert_eq!(updated, model);
}

/// The bundled catalog ships an `enso` entry with a full 1M-token window so the
/// Hanzo flagship is NOT silently capped at the unknown-slug fallback (272k).
/// Prefix matching means `enso`, `enso-1m`, etc. all resolve to this entry.
#[test]
fn bundled_enso_declares_one_million_token_window() {
    let catalog = crate::bundled_models_response().expect("bundled models.json parses");
    let enso = catalog
        .models
        .iter()
        .find(|m| m.slug == "enso")
        .expect("bundled catalog must contain the `enso` model");

    assert_eq!(enso.context_window, Some(1_000_000));
    assert_eq!(enso.max_context_window, Some(1_000_000));
    assert_eq!(enso.resolved_context_window(), Some(1_000_000));
    // Auto-compaction must key off the true window (90% of 1M), not the 272k floor.
    assert_eq!(enso.auto_compact_token_limit(), Some(900_000));
}
