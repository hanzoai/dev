use once_cell::sync::Lazy;
use tree_sitter::{Parser, Query, QueryCursor, StreamingIterator};
use tree_sitter_bash::LANGUAGE as BASH;

use crate::ExtractHeredocError;
use crate::EmbeddedApplyPatch;

/// Locate an apply_patch heredoc anywhere in the script (not anchored to be the only statement).
pub(crate) fn find_embedded_apply_patch(script: &str) -> Result<Option<EmbeddedApplyPatch>, ExtractHeredocError> {
    static QUERY: Lazy<Query> = Lazy::new(|| {
        let language = BASH.into();
        #[expect(clippy::expect_used)]
        Query::new(
            &language,
            r#"
            (
              redirected_statement @stmt
                body: (
                  command
                    name: (command_name (word) @apply_name)
                )
                (#any-of? @apply_name "apply_patch" "applypatch")
                redirect: (heredoc_redirect
                              . (heredoc_start)
                              . (heredoc_body) @heredoc
                              . (heredoc_end)
                              .)
            )

            (
              redirected_statement @stmt
                body: (
                  list
                    . (command
                        name: (command_name (word) @cd_name) .
                        argument: [
                          (word) @cd_path
                          (string (string_content) @cd_path)
                          (raw_string) @cd_raw_string
                        ] .)
                    "&&"
                    . (command name: (command_name (word) @apply_name))
                    .
                )
                (#eq? @cd_name "cd")
                (#any-of? @apply_name "apply_patch" "applypatch")
                redirect: (heredoc_redirect
                              . (heredoc_start)
                              . (heredoc_body) @heredoc
                              . (heredoc_end)
                              .)
            )
            "#,
        )
        .expect("valid bash query (embedded apply_patch)")
    });

    let lang = BASH.into();
    let mut parser = Parser::new();
    parser
        .set_language(&lang)
        .map_err(ExtractHeredocError::FailedToLoadBashGrammar)?;
    let tree = parser
        .parse(script, None)
        .ok_or(ExtractHeredocError::FailedToParsePatchIntoAst)?;

    let bytes = script.as_bytes();
    let root = tree.root_node();
    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(&QUERY, root, bytes);

    while let Some(m) = matches.next() {
        let mut heredoc_text: Option<String> = None;
        let mut cd_path: Option<String> = None;
        let mut stmt_range: Option<(usize, usize)> = None;

        for capture in m.captures.iter() {
            let name = QUERY.capture_names()[capture.index as usize];
            match name {
                "heredoc" => {
                    let text = capture
                        .node
                        .utf8_text(bytes)
                        .map_err(ExtractHeredocError::HeredocNotUtf8)?
                        .trim_end_matches('\n')
                        .to_string();
                    heredoc_text = Some(text);
                }
                "cd_path" => {
                    let text = capture
                        .node
                        .utf8_text(bytes)
                        .map_err(ExtractHeredocError::HeredocNotUtf8)?
                        .to_string();
                    cd_path = Some(text);
                }
                "cd_raw_string" => {
                    let raw = capture
                        .node
                        .utf8_text(bytes)
                        .map_err(ExtractHeredocError::HeredocNotUtf8)?;
                    let trimmed = raw
                        .strip_prefix('\'')
                        .and_then(|s| s.strip_suffix('\''))
                        .unwrap_or(raw);
                    cd_path = Some(trimmed.to_string());
                }
                "stmt" => {
                    stmt_range = Some((capture.node.start_byte(), capture.node.end_byte()));
                }
                _ => {}
            }
        }

        if let (Some(heredoc), Some(range)) = (heredoc_text, stmt_range) {
            return Ok(Some(EmbeddedApplyPatch { patch_body: heredoc, cd_path, stmt_byte_range: range }));
        }
    }

    Ok(None)
}
