use code_core::model_provider_info::{built_in_model_providers, WireApi};

fn with_env_override<F, R>(key: &str, value: Option<&str>, f: F) -> R
where
    F: FnOnce() -> R,
{
    let original = std::env::var(key).ok();
    match value {
        Some(v) => std::env::set_var(key, v),
        None => std::env::remove_var(key),
    }

    let result = f();

    match original {
        Some(v) => std::env::set_var(key, v),
        None => std::env::remove_var(key),
    }

    result
}

fn openai_provider_wire_api() -> WireApi {
    let providers = built_in_model_providers();
    providers
        .get("openai")
        .unwrap_or_else(|| panic!("missing built-in openai provider"))
        .wire_api
}

#[test]
fn openai_wire_api_defaults_to_responses() {
    let wire_api = with_env_override("OPENAI_WIRE_API", None, openai_provider_wire_api);
    assert_eq!(wire_api, WireApi::Responses);
}

#[test]
fn openai_wire_api_env_chat() {
    let wire_api = with_env_override("OPENAI_WIRE_API", Some("chat"), openai_provider_wire_api);
    assert_eq!(wire_api, WireApi::Chat);
}

#[test]
fn openai_wire_api_env_responses() {
    let wire_api = with_env_override(
        "OPENAI_WIRE_API",
        Some("responses"),
        openai_provider_wire_api,
    );
    assert_eq!(wire_api, WireApi::Responses);
}

#[test]
fn openai_wire_api_env_invalid_falls_back_to_responses() {
    let wire_api = with_env_override(
        "OPENAI_WIRE_API",
        Some("invalid-mode"),
        openai_provider_wire_api,
    );
    assert_eq!(wire_api, WireApi::Responses);
}

#[test]
fn openai_wire_api_env_chat_is_case_insensitive_and_tolerates_whitespace() {
    let wire_api = with_env_override(
        "OPENAI_WIRE_API",
        Some("  CHAT  "),
        openai_provider_wire_api,
    );
    assert_eq!(wire_api, WireApi::Chat);
}
