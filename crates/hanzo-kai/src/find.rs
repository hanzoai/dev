//! Evidence from search: where a message, a name or a string in a failing run is in the tree.
//!
//! The loop asks a [`Finder`] and nothing else; the engine behind it is Hanzo's one search, tgrep
//! (`hanzo-trigram`, the library hanzo-mcp's `fs search_text` and `code search_symbol` run on),
//! linked in process. A search is a call, not a spawn and not an MCP hop.

use std::path::Path;

/// A line of the tree that spells what was asked for.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Found {
    /// Relative to the root.
    pub file: String,
    /// 1-based.
    pub line: u32,
    /// The line reads like a declaration of the symbol asked for.
    pub definition: bool,
    pub excerpt: String,
}

/// Text and symbol search over a tree, best first.
pub trait Finder: Send + Sync {
    /// Lines matching the literal `text`.
    fn text(&self, root: &Path, text: &str, limit: usize) -> Result<Vec<Found>, String>;
    /// Lines spelling the identifier `name`, its definitions first.
    fn symbol(&self, root: &Path, name: &str, limit: usize) -> Result<Vec<Found>, String>;
}

/// tgrep, in process.
pub struct Tgrep;

fn found(hits: Vec<hanzo_trigram::Hit>) -> Vec<Found> {
    hits.into_iter()
        .filter(|h| h.entry.is_none() && h.place.is_none())
        .map(|h| Found {
            file: h.path.to_string_lossy().into_owned(),
            line: u32::try_from(h.line).unwrap_or(0),
            definition: h.mark == hanzo_trigram::Mark::Definition,
            excerpt: h.excerpt,
        })
        .collect()
}

impl Finder for Tgrep {
    fn text(&self, root: &Path, text: &str, limit: usize) -> Result<Vec<Found>, String> {
        let mut q = hanzo_trigram::Query::text(root, text);
        q.fixed = true;
        q.limit = Some(limit);
        hanzo_trigram::search(&q).map(found).map_err(|e| e.to_string())
    }

    fn symbol(&self, root: &Path, name: &str, limit: usize) -> Result<Vec<Found>, String> {
        let mut q = hanzo_trigram::Query::symbol(root, name);
        q.limit = Some(limit);
        hanzo_trigram::search(&q).map(found).map_err(|e| e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn tgrep_finds_the_line_that_spells_a_message_and_the_definition_of_a_name() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("pkg")).unwrap();
        std::fs::write(
            dir.path().join("pkg/parse.py"),
            "def parse_flag(raw):\n    if not raw:\n        raise ValueError(\"flag must not be empty\")\n    return raw\n",
        )
        .unwrap();
        std::fs::write(dir.path().join("pkg/use.py"), "from pkg.parse import parse_flag\nparse_flag('x')\n").unwrap();
        let text = Tgrep.text(dir.path(), "flag must not be empty", 5).unwrap();
        assert_eq!(text.len(), 1, "{text:?}");
        assert_eq!((text[0].file.as_str(), text[0].line), ("pkg/parse.py", 3));
        let symbol = Tgrep.symbol(dir.path(), "parse_flag", 5).unwrap();
        assert!(symbol[0].definition && symbol[0].file == "pkg/parse.py", "{symbol:?}");
        assert!(symbol.iter().any(|f| !f.definition && f.file == "pkg/use.py"));
    }
}
