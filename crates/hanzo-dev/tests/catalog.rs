//! The model picker is Hanzo's: Enso first, and the bundled OpenAI models known but unlisted.
//!
//! This test verifies that the model picker correctly filters and prioritizes models:
//! - Enso models are listed first with highest priority
//! - Hanzo-listed models appear after Enso models
//! - Bundled OpenAI models are known but unlisted (visibility != List)
//! - The picker can still resolve sessions using known OpenAI models

use codex_protocol::openai_models::ModelVisibility;

#[test]
fn the_picker_leads_with_enso_and_does_not_list_the_bundled_openai_models() {
    let models = codex_models_manager::bundled_models_response()
        .expect("the bundled catalog parses")
        .models;
    // Filter models that are marked for listing in the picker
    let listed: Vec<_> = models
        .iter()
        .filter(|model| matches!(model.visibility, ModelVisibility::List))
        .collect();
    // Sort by priority (lower priority number = higher display priority)
    // For models with equal priority, sort slug alphabetically for deterministic results
    let mut listed_sorted = listed;
    listed_sorted.sort_by(|a, b| {
        a.priority.cmp(&b.priority).then_with(|| a.slug.cmp(&b.slug))
    });
    // The first listed model should be Enso (highest priority)
    assert!(
        listed_sorted.first().map_or(false, |m| m.slug == "enso-auto"),
        "the first listed model should be enso-auto, got {:?}",
        listed_sorted.first().map(|m| m.slug.as_str())
    );
    // zen5 should be among the listed models (Hanzo model with listed visibility)
    assert!(
        listed_sorted.iter().any(|model| model.slug == "zen5"),
        "zen5 should be listed in the model picker"
    );
    // No model with "gpt-" prefix should appear in the listed models;
    // bundled OpenAI models have List=false and are known but unlisted
    assert!(
        listed_sorted.iter().all(|model| !model.slug.starts_with("gpt-")),
        "no listed model should start with 'gpt-'; found: {:?}",
        listed_sorted
            .iter()
            .filter(|m| m.slug.starts_with("gpt-"))
            .map(|m| m.slug.as_str())
            .collect::<Vec<_>>()
    );
    // An OpenAI model (gpt-6-astra) should exist in the full catalog
    // so that sessions can still resolve OpenAI models even though they're unlisted
    assert!(
        models.iter().any(|model| model.slug == "gpt-6-astra"),
        "an OpenAI model stays known, so a session that names one still resolves"
    );
}
