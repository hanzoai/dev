use super::*;

#[test]
fn new_home_loads_hanzo_provider_and_native_mcp() {
    let directory = tempfile::tempdir().unwrap();
    initialize_home(directory.path()).unwrap();
    let config: toml::Value = toml::from_str(&std::fs::read_to_string(directory.path().join("config.toml"))
        .unwrap()).unwrap();
    assert_eq!(config["model_provider"].as_str(), Some("hanzo"));
    assert_eq!(config["model_providers"]["hanzo"]["base_url"].as_str(), Some(API_BASE));
    assert_eq!(config["mcp_servers"]["hanzo"]["command"].as_str(), Some("hanzo-mcp"));
    assert!(config["mcp_servers"]["hanzo"]["args"].as_array().unwrap().is_empty());
}

#[test]
fn existing_configuration_is_preserved_byte_for_byte() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("config.toml");
    let custom = "# My configuration\nmodel = 'custom'\n";
    std::fs::write(&path, custom).unwrap();
    initialize_home(directory.path()).unwrap();
    assert_eq!(std::fs::read_to_string(path).unwrap(), custom);
}

#[test]
fn concurrent_startup_publishes_a_complete_config() {
    let directory = tempfile::tempdir().unwrap();
    std::thread::scope(|scope| {
        for _ in 0..8 {
            let home = directory.path();
            scope.spawn(move || {
                initialize_home(home).unwrap();
                assert_eq!(std::fs::read_to_string(home.join("config.toml")).unwrap(), DEFAULT_CONFIG);
            });
        }
    });
}

#[test]
fn profile_resolution_honors_override_and_requires_a_home() {
    assert_eq!(product_home(Some("custom".into()), Some("user".into())).unwrap(), PathBuf::from("custom"));
    assert_eq!(product_home(None, Some("user".into())).unwrap(), PathBuf::from("user/.hanzo/dev"));
    assert!(product_home(None, None).is_err());
}

/// The default profile links its credential to the shared one, so signing in to
/// any Hanzo tool signs in to all of them.
#[test]
fn the_default_profile_shares_one_account() {
    let root = tempfile::tempdir().unwrap();
    let home = root.path().join(".hanzo/dev");
    std::fs::create_dir_all(&home).unwrap();
    share_credential(&home);
    let link = std::fs::read_link(home.join("auth.json")).unwrap();
    assert_eq!(link, root.path().join(".hanzo/auth.json"));
}

/// Running it twice must not stack or break the link it already made.
#[test]
fn sharing_the_account_again_changes_nothing() {
    let root = tempfile::tempdir().unwrap();
    let home = root.path().join(".hanzo/dev");
    std::fs::create_dir_all(&home).unwrap();
    share_credential(&home);
    share_credential(&home);
    let link = std::fs::read_link(home.join("auth.json")).unwrap();
    assert_eq!(link, root.path().join(".hanzo/auth.json"));
}

/// A credential the user already has is theirs; never replace it with a link.
#[test]
fn an_existing_credential_is_left_alone() {
    let root = tempfile::tempdir().unwrap();
    let home = root.path().join(".hanzo/dev");
    std::fs::create_dir_all(&home).unwrap();
    let theirs = home.join("auth.json");
    std::fs::write(&theirs, "{\"token\":\"theirs\"}").unwrap();
    share_credential(&home);
    assert!(!std::fs::symlink_metadata(&theirs).unwrap().is_symlink());
    assert_eq!(std::fs::read_to_string(&theirs).unwrap(), "{\"token\":\"theirs\"}");
}

/// An explicit DEV_HOME is a separate profile, and separate means its own account.
#[test]
fn an_explicit_profile_keeps_its_own_account() {
    let root = tempfile::tempdir().unwrap();
    let home = root.path().join("somewhere/else");
    std::fs::create_dir_all(&home).unwrap();
    share_credential(&home);
    assert!(std::fs::symlink_metadata(home.join("auth.json")).is_err());
}
