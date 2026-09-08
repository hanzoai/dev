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
