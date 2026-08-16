
use code_core::util as core_util;

pub(crate) fn strip_bash_lc_and_escape(command: &[String]) -> String {
    core_util::strip_bash_lc_and_escape(command)
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
    fn strip_bash_lc_and_escape_shows_raw_shell_script_without_quotes() {
        let command = vec!["git status --short".to_string()];

        assert_eq!(strip_bash_lc_and_escape(&command), "git status --short");
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
