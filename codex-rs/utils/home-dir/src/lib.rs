use codex_utils_absolute_path::AbsolutePathBuf;
use dirs::home_dir;
use std::path::Path;
use std::path::PathBuf;

/// Preferred env override for the config home.
const HANZO_HOME_ENV_VAR: &str = "HANZO_HOME";
/// Legacy env override, still honored for backwards compatibility.
const CODEX_HOME_ENV_VAR: &str = "CODEX_HOME";
/// Primary config directory name under the user's home (`~/.hanzo`).
const PRIMARY_HOME_DIR: &str = ".hanzo";
/// Legacy config directory names, read (in order) only when the primary
/// `~/.hanzo` does not yet exist, so prior `dev`/codex installs keep working.
const LEGACY_HOME_DIRS: &[&str] = &[".codex", ".code"];

/// Returns the path to the Hanzo Dev configuration directory.
///
/// Resolution order:
/// 1. `HANZO_HOME` if set (preferred), else `CODEX_HOME` (legacy). A set value
///    must exist and be a directory; it is canonicalized or this function Errs.
/// 2. Otherwise `~/.hanzo`. If `~/.hanzo` does not exist but a legacy `~/.codex`
///    or `~/.code` does, the legacy directory is returned so an existing install
///    is read unchanged. Existence of the default is not otherwise required.
pub fn find_codex_home() -> std::io::Result<AbsolutePathBuf> {
    let env = std::env::var(HANZO_HOME_ENV_VAR)
        .ok()
        .filter(|val| !val.is_empty())
        .map(|val| (HANZO_HOME_ENV_VAR, val))
        .or_else(|| {
            std::env::var(CODEX_HOME_ENV_VAR)
                .ok()
                .filter(|val| !val.is_empty())
                .map(|val| (CODEX_HOME_ENV_VAR, val))
        });

    match env {
        Some((var, val)) => resolve_home_from_env(var, &val),
        None => {
            let home = home_dir().ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "Could not find home directory",
                )
            })?;
            AbsolutePathBuf::from_absolute_path(default_home_dir(&home, |p| p.exists()))
        }
    }
}

/// Validate and canonicalize an explicit config-home path supplied via `var`.
fn resolve_home_from_env(var: &str, val: &str) -> std::io::Result<AbsolutePathBuf> {
    let path = PathBuf::from(val);
    let metadata = std::fs::metadata(&path).map_err(|err| match err.kind() {
        std::io::ErrorKind::NotFound => std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("{var} points to {val:?}, but that path does not exist"),
        ),
        _ => std::io::Error::new(err.kind(), format!("failed to read {var} {val:?}: {err}")),
    })?;

    if !metadata.is_dir() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("{var} points to {val:?}, but that path is not a directory"),
        ));
    }

    let canonical = path.canonicalize().map_err(|err| {
        std::io::Error::new(
            err.kind(),
            format!("failed to canonicalize {var} {val:?}: {err}"),
        )
    })?;
    AbsolutePathBuf::from_absolute_path(canonical)
}

/// Resolve the default config home under `home`: `~/.hanzo`, unless it is absent
/// and a legacy `~/.codex` / `~/.code` exists (then the first existing legacy).
/// `exists` is injected so the choice is unit-testable without touching real FS.
fn default_home_dir(home: &Path, exists: impl Fn(&Path) -> bool) -> PathBuf {
    let primary = home.join(PRIMARY_HOME_DIR);
    if exists(&primary) {
        return primary;
    }
    for legacy in LEGACY_HOME_DIRS {
        let candidate = home.join(legacy);
        if exists(&candidate) {
            return candidate;
        }
    }
    primary
}

#[cfg(test)]
mod tests {
    use super::CODEX_HOME_ENV_VAR;
    use super::HANZO_HOME_ENV_VAR;
    use super::default_home_dir;
    use super::resolve_home_from_env;
    use codex_utils_absolute_path::AbsolutePathBuf;
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::io::ErrorKind;
    use std::path::Path;
    use std::path::PathBuf;
    use tempfile::TempDir;

    #[test]
    fn home_env_missing_path_is_fatal() {
        let temp_home = TempDir::new().expect("temp home");
        let missing = temp_home.path().join("missing-home");
        let missing_str = missing
            .to_str()
            .expect("missing home path should be valid utf-8");

        let err =
            resolve_home_from_env(CODEX_HOME_ENV_VAR, missing_str).expect_err("missing CODEX_HOME");
        assert_eq!(err.kind(), ErrorKind::NotFound);
        assert!(
            err.to_string().contains("CODEX_HOME"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn home_env_file_path_is_fatal() {
        let temp_home = TempDir::new().expect("temp home");
        let file_path = temp_home.path().join("home.txt");
        fs::write(&file_path, "not a directory").expect("write temp file");
        let file_str = file_path
            .to_str()
            .expect("file home path should be valid utf-8");

        let err = resolve_home_from_env(HANZO_HOME_ENV_VAR, file_str).expect_err("file HANZO_HOME");
        assert_eq!(err.kind(), ErrorKind::InvalidInput);
        assert!(
            err.to_string().contains("HANZO_HOME"),
            "unexpected error: {err}"
        );
        assert!(
            err.to_string().contains("not a directory"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn home_env_valid_directory_canonicalizes() {
        let temp_home = TempDir::new().expect("temp home");
        let temp_str = temp_home
            .path()
            .to_str()
            .expect("temp home path should be valid utf-8");

        let resolved =
            resolve_home_from_env(HANZO_HOME_ENV_VAR, temp_str).expect("valid HANZO_HOME");
        let expected = temp_home
            .path()
            .canonicalize()
            .expect("canonicalize temp home");
        let expected = AbsolutePathBuf::from_absolute_path(expected).expect("absolute home");
        assert_eq!(resolved, expected);
    }

    #[test]
    fn default_is_primary_hanzo_when_nothing_exists() {
        let home = PathBuf::from("/home/user");
        let resolved = default_home_dir(&home, |_p| false);
        assert_eq!(resolved, home.join(".hanzo"));
    }

    #[test]
    fn default_prefers_primary_hanzo_when_it_exists() {
        let home = PathBuf::from("/home/user");
        // Both ~/.hanzo and legacy ~/.codex "exist": primary wins.
        let resolved = default_home_dir(&home, |_p| true);
        assert_eq!(resolved, home.join(".hanzo"));
    }

    #[test]
    fn default_falls_back_to_legacy_codex_when_hanzo_absent() {
        let home = PathBuf::from("/home/user");
        let legacy = home.join(".codex");
        let resolved = default_home_dir(&home, {
            let legacy = legacy.clone();
            move |p: &Path| p == legacy
        });
        assert_eq!(resolved, legacy);
    }

    #[test]
    fn default_falls_back_to_legacy_code_when_only_code_exists() {
        let home = PathBuf::from("/home/user");
        let legacy = home.join(".code");
        let resolved = default_home_dir(&home, {
            let legacy = legacy.clone();
            move |p: &Path| p == legacy
        });
        assert_eq!(resolved, legacy);
    }
}
