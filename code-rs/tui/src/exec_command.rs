use std::path::Path;
use std::path::PathBuf;

use shlex::try_join;

pub(crate) fn escape_command(command: &[String]) -> String {
    try_join(command.iter().map(String::as_str)).unwrap_or_else(|_| command.join(" "))
}

pub(crate) fn strip_bash_lc_and_escape(command: &[String]) -> String {
    match command {
        // exactly three items
        [first, second, third]
            // first two must be "bash", "-lc"
            if is_bash_like(first) && second == "-lc" =>
        {
            strip_shell_wrapper_for_display(third)
        }
        _ => escape_command(command),
    }
}

fn strip_shell_wrapper_for_display(script: &str) -> String {
    unwrap_profile_wrapper(script).unwrap_or(script).to_string()
}

fn unwrap_profile_wrapper(script: &str) -> Option<&str> {
    let script = script.strip_prefix("set +m; ").unwrap_or(script);
    let body = script.strip_prefix("source ")?;
    let (rc_path, wrapped) = body.split_once(" && ")?;
    if !looks_like_shell_rc_path(rc_path) {
        return None;
    }

    wrapped
        .strip_prefix('(')
        .and_then(|inner| inner.strip_suffix(')'))
        .or_else(|| {
            wrapped
                .strip_prefix("{\n")
                .and_then(|inner| inner.strip_suffix("\n}"))
        })
}

fn looks_like_shell_rc_path(path: &str) -> bool {
    let Some(home) = std::env::var_os("HOME") else {
        return false;
    };
    let home = Path::new(&home);
    path == home.join(".bashrc").to_string_lossy() || path == home.join(".zshrc").to_string_lossy()
}

fn is_bash_like(cmd: &str) -> bool {
    let trimmed = cmd.trim_matches('"').trim_matches('\'');
    let lowered = Path::new(trimmed)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(trimmed)
        .to_ascii_lowercase();
    matches!(
        lowered.as_str(),
        "bash"
            | "bash.exe"
            | "sh"
            | "sh.exe"
            | "dash"
            | "dash.exe"
            | "zsh"
            | "zsh.exe"
            | "ksh"
            | "ksh.exe"
            | "busybox"
    )
}

/// If `path` is absolute and inside $HOME, return the part *after* the home
/// directory; otherwise, return the path as-is. Note if `path` is the homedir,
/// this will return and empty path.
pub(crate) fn relativize_to_home<P>(path: P) -> Option<PathBuf>
where
    P: AsRef<Path>,
{
    let path = path.as_ref();
    if !path.is_absolute() {
        // If the path is not absolute, we can’t do anything with it.
        return None;
    }

    if let Some(home_dir) = std::env::var_os("HOME").map(PathBuf::from) {
        if let Ok(rel) = path.strip_prefix(&home_dir) {
            return Some(rel.to_path_buf());
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::strip_bash_lc_and_escape;
    use std::path::PathBuf;

    fn home_rc_path(name: &str) -> String {
        let home = std::env::var_os("HOME").expect("HOME should be set for tui exec_command tests");
        PathBuf::from(home).join(name).to_string_lossy().to_string()
    }

    #[test]
    fn strip_bash_lc_and_escape_hides_profile_wrapper() {
        let bashrc = home_rc_path(".bashrc");
        let command = vec![
            "/bin/bash".to_string(),
            "-lc".to_string(),
            format!("source {bashrc} && (sed -n '1,220p' file.txt)"),
        ];

        assert_eq!(strip_bash_lc_and_escape(&command), "sed -n '1,220p' file.txt");
    }

    #[test]
    fn strip_bash_lc_and_escape_hides_multiline_profile_wrapper() {
        let bashrc = home_rc_path(".bashrc");
        let command = vec![
            "/bin/bash".to_string(),
            "-lc".to_string(),
            format!(
                "set +m; source {bashrc} && {{\napply_patch <<'PATCH'\n*** Begin Patch\n*** End Patch\nPATCH\n}}"
            ),
        ];

        assert_eq!(
            strip_bash_lc_and_escape(&command),
            "apply_patch <<'PATCH'\n*** Begin Patch\n*** End Patch\nPATCH"
        );
    }

    #[test]
    fn strip_bash_lc_and_escape_preserves_user_set_plus_m_command() {
        let command = vec![
            "/bin/bash".to_string(),
            "-lc".to_string(),
            "set +m; echo done".to_string(),
        ];

        assert_eq!(strip_bash_lc_and_escape(&command), "set +m; echo done");
    }

    #[test]
    fn strip_bash_lc_and_escape_preserves_user_source_command() {
        let command = vec![
            "/bin/bash".to_string(),
            "-lc".to_string(),
            "source script.sh && echo done".to_string(),
        ];

        assert_eq!(
            strip_bash_lc_and_escape(&command),
            "source script.sh && echo done"
        );
    }

    #[test]
    fn strip_bash_lc_and_escape_preserves_other_bashrc_paths() {
        let command = vec![
            "/bin/bash".to_string(),
            "-lc".to_string(),
            "source /tmp/project/.bashrc && echo done".to_string(),
        ];

        assert_eq!(
            strip_bash_lc_and_escape(&command),
            "source /tmp/project/.bashrc && echo done"
        );
    }
}
