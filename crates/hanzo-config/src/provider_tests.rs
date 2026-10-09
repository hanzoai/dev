use super::*;

#[test]
fn chatgpt_login_selects_openai_without_sending_hanzo_model_or_key() {
    let home = tempfile::tempdir().unwrap();
    crate::initialize_home(home.path()).unwrap();
    activate_provider(home.path(), Provider::OpenAi).unwrap();
    let config: toml::Value = toml::from_str(&std::fs::read_to_string(home.path().join("config.toml")).unwrap()).unwrap();
    let mut expected: toml::Value = toml::from_str(crate::DEFAULT_CONFIG).unwrap();
    expected["model_provider"] = "openai".into();
    expected.as_table_mut().unwrap().remove("model");
    assert_eq!(config, expected);
}

#[test]
fn switching_back_to_hanzo_preserves_unrelated_preferences() {
    let home = tempfile::tempdir().unwrap();
    crate::initialize_home(home.path()).unwrap();
    let path = home.path().join("config.toml");
    let text = std::fs::read_to_string(&path).unwrap();
    std::fs::write(&path, format!("# My preferences\nmodel_reasoning_effort = 'high'\n{text}")).unwrap();
    activate_provider(home.path(), Provider::OpenAi).unwrap();
    activate_provider(home.path(), Provider::Hanzo).unwrap();
    let text = std::fs::read_to_string(path).unwrap();
    assert!(text.starts_with("# My preferences\nmodel_reasoning_effort = 'high'\n"));
    let config: toml::Value = toml::from_str(&text).unwrap();
    let mut expected: toml::Value = toml::from_str(crate::DEFAULT_CONFIG).unwrap();
    expected
        .as_table_mut()
        .unwrap()
        .insert("model_reasoning_effort".to_string(), "high".into());
    assert_eq!(config, expected);
}

#[test]
fn a_profile_naming_no_provider_runs_on_hanzo_and_keeps_its_own_providers() {
    let home = tempfile::tempdir().unwrap();
    let path = home.path().join("config.toml");
    std::fs::write(&path, "[model_providers.pool]\nname = \"Pool\"\nbase_url = \"http://pool/v1\"\n").unwrap();
    default_to_hanzo(home.path());
    let config: toml::Value = toml::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
    let defaults: toml::Value = toml::from_str(crate::DEFAULT_CONFIG).unwrap();
    assert_eq!(config["model_provider"].as_str(), Some("hanzo"));
    assert_eq!(config["model"].as_str(), Some(crate::DEFAULT_MODEL));
    assert_eq!(config["model_providers"]["hanzo"], defaults["model_providers"]["hanzo"]);
    assert_eq!(config["model_providers"]["pool"]["base_url"].as_str(), Some("http://pool/v1"));
}

#[test]
fn a_profile_that_names_a_provider_is_left_as_it_is() {
    let home = tempfile::tempdir().unwrap();
    let path = home.path().join("config.toml");
    let text = "model_provider = \"pool\"\nmodel = \"qwen\"\n\n[model_providers.pool]\nname = \"Pool\"\n";
    std::fs::write(&path, text).unwrap();
    default_to_hanzo(home.path());
    assert_eq!(std::fs::read_to_string(path).unwrap(), text);
}

const JWT: &str = "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJ6In0.c2ln";

#[test]
fn an_api_key_in_the_environment_wins() {
    let chosen = pick(Some("hk-live-key".into()), Some("saved".into()), || Some(JWT.into()));
    assert_eq!(chosen.as_deref(), Some("hk-live-key"));
}

#[test]
fn a_key_the_host_hands_the_run_is_used_as_given() {
    // A cloud sandbox has no CLI to ask: the delegated token in HANZO_API_KEY is it.
    let chosen = choose(Some(JWT.into()), Some("hk-live-key".into()), Some("saved".into()), || {
        panic!("a run handed a key never asks the CLI")
    });
    assert_eq!(chosen.as_deref(), Some(JWT));
    let fallback = choose(None, None, None, || Some("fresh".into()));
    assert_eq!(fallback.as_deref(), Some("fresh"));
}

#[test]
fn an_inherited_sign_in_token_yields_to_a_fresh_one() {
    let fresh = "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJmcmVzaCJ9.c2ln";
    assert_eq!(pick(Some(JWT.into()), None, || Some(fresh.into())).as_deref(), Some(fresh));
    assert_eq!(pick(Some(JWT.into()), Some("saved".into()), || None).as_deref(), Some("saved"));
    // With nothing fresher, the inherited token is still better than none.
    assert_eq!(pick(Some(JWT.into()), None, || None).as_deref(), Some(JWT));
}

#[test]
fn the_default_provider_refreshes_its_token() {
    let config: toml::Value = toml::from_str(crate::DEFAULT_CONFIG).unwrap();
    let provider = &config["model_providers"]["hanzo"];
    assert!(provider.get("env_key").is_none(), "auth and env_key cannot both be set");
    assert_eq!(provider["auth"]["command"].as_str(), Some("dev"));
    assert_eq!(provider["auth"]["args"].as_array().unwrap()[0].as_str(), Some("token"));
    // Shorter than the hour a sign-in token lives.
    assert!(provider["auth"]["refresh_interval_ms"].as_integer().unwrap() < 3_600_000);
}

const OLD_PROVIDER: &str = r#"# mine
model_provider = "hanzo"
model = "enso-auto"

[model_providers.hanzo]
name = "Hanzo"
base_url = "https://api.hanzo.ai/v1"
wire_api = "responses"
env_key = "HANZO_USER_KEY"
env_key_instructions = "Run `dev login` to sign in to Hanzo or paste a Hanzo API key, or set HANZO_USER_KEY."
requires_openai_auth = false
"#;

#[test]
fn a_startup_only_credential_is_upgraded_to_the_refreshing_one() {
    let home = tempfile::tempdir().unwrap();
    let path = home.path().join("config.toml");
    std::fs::write(&path, OLD_PROVIDER).unwrap();
    upgrade_auth(home.path());
    let text = std::fs::read_to_string(&path).unwrap();
    assert!(text.starts_with("# mine\n"));
    let config: toml::Value = toml::from_str(&text).unwrap();
    let defaults: toml::Value = toml::from_str(crate::DEFAULT_CONFIG).unwrap();
    assert_eq!(config["model_providers"]["hanzo"], defaults["model_providers"]["hanzo"]);
    // Twice changes nothing more.
    upgrade_auth(home.path());
    assert_eq!(std::fs::read_to_string(&path).unwrap(), text);
}

#[test]
fn a_provider_the_user_changed_is_left_alone() {
    let home = tempfile::tempdir().unwrap();
    let path = home.path().join("config.toml");
    let theirs = OLD_PROVIDER.replace("env_key = \"HANZO_USER_KEY\"", "env_key = \"MY_KEY\"");
    std::fs::write(&path, &theirs).unwrap();
    upgrade_auth(home.path());
    assert_eq!(std::fs::read_to_string(&path).unwrap(), theirs);
}

#[test]
fn the_api_key_name_nothing_reads_is_upgraded_too() {
    let home = tempfile::tempdir().expect("tempdir");
    let stale = OLD_PROVIDER.replace("HANZO_USER_KEY", "HANZO_API_KEY");
    std::fs::write(home.path().join("config.toml"), stale).expect("write");
    upgrade_auth(home.path());
    let text = std::fs::read_to_string(home.path().join("config.toml")).expect("read");
    assert!(text.contains("command = \"dev\""), "{text}");
    assert!(!text.contains("HANZO_API_KEY"), "{text}");
}
