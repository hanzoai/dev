//! The model picker is Hanzo's: Enso first, and the bundled OpenAI models known but unlisted.

use codex_protocol::openai_models::ModelVisibility;

#[test]
fn the_picker_leads_with_enso_and_does_not_list_the_bundled_openai_models() {
    let models = codex_models_manager::bundled_models_response()
        .expect("the bundled catalog parses")
        .models;
    let mut listed: Vec<_> = models
        .iter()
        .filter(|model| matches!(model.visibility, ModelVisibility::List))
        .collect();
    listed.sort_by_key(|model| model.priority);
    assert_eq!(listed.first().map(|model| model.slug.as_str()), Some("enso-auto"));
    assert!(listed.iter().any(|model| model.slug == "zen5"));
    assert!(listed.iter().all(|model| !model.slug.starts_with("gpt-")));
    assert!(
        models.iter().any(|model| model.slug == "gpt-6-astra"),
        "an OpenAI model stays known, so a session that names one still resolves"
    );
}
