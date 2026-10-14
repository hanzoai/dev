//! Matching: a query compiled once, then run over files read from disk.

use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use globset::Glob;
use globset::GlobBuilder;
use globset::GlobSet;
use globset::GlobSetBuilder;
use rayon::prelude::*;
use regex::Regex;
use regex::RegexBuilder;
use tgrep_core::encoding::EncodingMode;
use tgrep_core::walker;

use crate::Error;
use crate::Hit;
use crate::Kind;
use crate::Mark;
use crate::Query;

/// Declaration keywords that put the declared name right after them.
const DECLARE: &str = "fn|def|class|struct|enum|union|trait|interface|type|mod|module|namespace|\
                       const|static|let|var|val|function|typedef|macro_rules!";

/// A query compiled for files.
pub(crate) struct Plan {
    pub kind: Kind,
    /// The regex as tgrep's planner and server receive it.
    pub pattern: String,
    /// Line matcher for text and symbol queries.
    pub line: Option<Regex>,
    /// Declaration matcher for symbol queries.
    pub declare: Option<Regex>,
    /// Name matcher for name queries.
    pub name: Option<Names>,
    /// File filter for text and symbol queries: the globs to keep, the `!` globs to drop.
    pub keep: Option<GlobSet>,
    pub drop: Option<GlobSet>,
    pub ignore_case: bool,
    pub context: usize,
}

/// A name glob: matched against the file name when it has no `/`, else against the
/// path relative to the searched directory.
pub(crate) struct Names {
    set: GlobSet,
    base: bool,
}

impl Names {
    pub fn matches(&self, rel: &str) -> bool {
        if self.base {
            self.set.is_match(rel.rsplit('/').next().unwrap_or(rel))
        } else {
            self.set.is_match(rel)
        }
    }
}

impl Plan {
    pub fn new(query: &Query) -> Result<Plan, Error> {
        let pattern = match query.kind {
            Kind::Text if query.fixed => regex::escape(&query.pattern),
            Kind::Text | Kind::Name => query.pattern.clone(),
            Kind::Symbol => format!(r"\b{}\b", regex::escape(&query.pattern)),
        };
        let compile = |pattern: &str| {
            RegexBuilder::new(pattern)
                .case_insensitive(query.ignore_case)
                .multi_line(true)
                .crlf(true)
                .build()
                .map_err(|error| Error::Pattern(error.to_string()))
        };
        let (line, declare, name) = match query.kind {
            Kind::Text => (Some(compile(&pattern)?), None, None),
            Kind::Symbol => {
                let name = regex::escape(&query.pattern);
                let declare = format!(
                    r"\b(?:{DECLARE})\s+(?:mut\s+)?{name}\b|\bfunc\s+(?:\([^)]*\)\s*)?{name}\b|^\s*#\s*define\s+{name}\b|\b{name}\s*[:=]\s*(?:function\b|async\b|lambda\b|\([^)]*\)\s*=>)"
                );
                (Some(compile(&pattern)?), Some(compile(&declare)?), None)
            }
            Kind::Name => {
                let glob = GlobBuilder::new(&query.pattern)
                    .case_insensitive(query.ignore_case)
                    .literal_separator(true)
                    .build()
                    .map_err(|error| Error::Pattern(error.to_string()))?;
                let set = GlobSetBuilder::new()
                    .add(glob)
                    .build()
                    .map_err(|error| Error::Pattern(error.to_string()))?;
                let base = !query.pattern.contains('/');
                (None, None, Some(Names { set, base }))
            }
        };
        let set = |globs: Vec<&str>| -> Result<Option<GlobSet>, Error> {
            if globs.is_empty() {
                return Ok(None);
            }
            let mut set = GlobSetBuilder::new();
            for glob in globs {
                let glob = if glob.contains('/') {
                    Glob::new(glob)
                } else {
                    Glob::new(&format!("**/{glob}"))
                }
                .map_err(|error| Error::Pattern(error.to_string()))?;
                set.add(glob);
            }
            set.build()
                .map(Some)
                .map_err(|error| Error::Pattern(error.to_string()))
        };
        let keep = set(query
            .glob
            .iter()
            .filter(|glob| !glob.starts_with('!'))
            .map(String::as_str)
            .collect())?;
        let drop = set(query
            .glob
            .iter()
            .filter_map(|glob| glob.strip_prefix('!'))
            .collect())?;
        Ok(Plan {
            kind: query.kind,
            pattern,
            line,
            declare,
            name,
            keep,
            drop,
            ignore_case: query.ignore_case,
            context: query.context,
        })
    }

    /// Whether a text or symbol query reads the file at `rel` (relative to the
    /// searched directory).
    pub fn admits(&self, rel: &str) -> bool {
        self.keep.as_ref().is_none_or(|set| set.is_match(rel))
            && !self.drop.as_ref().is_some_and(|set| set.is_match(rel))
    }

    /// The hits in one file's text. `path` is the absolute path the hits carry.
    pub fn lines(&self, path: &Path, text: &str) -> Vec<Hit> {
        let Some(regex) = &self.line else {
            return Vec::new();
        };
        let mut found: Vec<(usize, usize)> = Vec::new();
        let mut at = 0;
        while at <= text.len() {
            let Some(hit) = regex.find_at(text, at) else {
                break;
            };
            // The empty line after a final terminator is not a line.
            if hit.start() == text.len() && (text.is_empty() || text.ends_with('\n')) {
                break;
            }
            let start = text[..hit.start()].rfind('\n').map_or(0, |i| i + 1);
            let end = text[hit.start()..]
                .find('\n')
                .map_or(text.len(), |i| hit.start() + i);
            let line = text[start..end]
                .strip_suffix('\r')
                .unwrap_or(&text[start..end]);
            if regex.is_match(line) {
                found.push((start, end));
            }
            if end >= text.len() {
                break;
            }
            at = end + 1;
        }
        if found.is_empty() {
            return Vec::new();
        }
        let lines: Vec<&str> = if self.context > 0 {
            text.split('\n')
                .map(|line| line.strip_suffix('\r').unwrap_or(line))
                .collect()
        } else {
            Vec::new()
        };
        let mut number = 1u64;
        let mut counted = 0;
        found
            .into_iter()
            .map(|(start, end)| {
                number +=
                    memchr::memchr_iter(b'\n', &text.as_bytes()[counted..start]).count() as u64;
                counted = start;
                let excerpt = text[start..end]
                    .strip_suffix('\r')
                    .unwrap_or(&text[start..end])
                    .to_string();
                let mark = match &self.declare {
                    None => Mark::Match,
                    Some(declare) if declare.is_match(&excerpt) => Mark::Definition,
                    Some(_) => Mark::Reference,
                };
                let index = (number - 1) as usize;
                let (before, after) = if self.context > 0 {
                    let first = index.saturating_sub(self.context);
                    let last = (index + 1 + self.context).min(lines.len());
                    (
                        lines[first..index]
                            .iter()
                            .map(ToString::to_string)
                            .collect(),
                        lines[(index + 1).min(last)..last]
                            .iter()
                            .map(ToString::to_string)
                            .collect(),
                    )
                } else {
                    (Vec::new(), Vec::new())
                };
                Hit {
                    path: path.to_path_buf(),
                    entry: None,
                    place: None,
                    line: Some(number),
                    excerpt,
                    before,
                    after,
                    mark,
                    score: 0,
                }
            })
            .collect()
    }

    /// A name hit for `path` when its relative form matches.
    pub fn file(&self, path: &Path, rel: &str) -> Option<Hit> {
        let names = self.name.as_ref()?;
        names.matches(rel).then(|| Hit {
            path: path.to_path_buf(),
            entry: None,
            place: None,
            line: None,
            excerpt: String::new(),
            before: Vec::new(),
            after: Vec::new(),
            mark: Mark::File,
            score: 0,
        })
    }
}

/// The file's searchable text: decoded as tgrep decodes it, `None` when it is
/// binary (holds a NUL) or unreadable.
pub(crate) fn read(path: &Path) -> Option<String> {
    let bytes = std::fs::read(path).ok()?;
    let (text, _) = tgrep_core::encoding::decode_owned_with_fixups(bytes, EncodingMode::Auto);
    (!text.as_bytes().contains(&0)).then_some(text)
}

/// Searches the given absolute files from disk, in parallel.
pub(crate) fn grep(plan: &Plan, files: &[PathBuf]) -> Vec<Hit> {
    files
        .par_iter()
        .flat_map_iter(|path| match read(path) {
            Some(text) => plan.lines(path, &text),
            None => Vec::new(),
        })
        .collect()
}

/// `rel` of `path` beneath `scope`, `/`-separated.
pub(crate) fn relative(scope: &Path, path: &Path) -> String {
    path.strip_prefix(scope)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

/// Answers a query by walking `scope`: tgrep's walker, with `.gitignore`, hidden
/// files and binary extensions applied as `rg` applies them.
pub(crate) fn scan(plan: &Plan, scope: &Path) -> Result<Vec<Hit>, Error> {
    if scope.is_file() {
        let rel = scope
            .file_name()
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_default();
        return Ok(match plan.kind {
            Kind::Name => plan.file(scope, &rel).into_iter().collect(),
            Kind::Text | Kind::Symbol => grep(plan, &[scope.to_path_buf()]),
        });
    }
    let walk = walker::walk_dir(scope, &walker::WalkOptions::default());
    Ok(match plan.kind {
        Kind::Name => walk
            .listed_files
            .iter()
            .filter_map(|path| plan.file(path, &relative(scope, path)))
            .collect(),
        Kind::Text | Kind::Symbol => {
            let files: Vec<PathBuf> = walk
                .files
                .into_iter()
                .filter(|path| plan.admits(&relative(scope, path)))
                .collect();
            grep(plan, &files)
        }
    })
}

/// The files under `scope` an index may not hold as they are: the `touched` ones
/// that are not hidden, and, with `since`, every file whose content or name
/// changed at or after it (a walk: names and metadata, no contents).
pub(crate) fn dirty(
    scope: &Path,
    touched: &[PathBuf],
    since: Option<SystemTime>,
) -> BTreeSet<PathBuf> {
    let mut dirty: BTreeSet<PathBuf> = touched
        .iter()
        .filter(|path| path.starts_with(scope))
        .filter(|path| {
            !relative(scope, path)
                .split('/')
                .any(|part| part.starts_with('.'))
        })
        .cloned()
        .collect();
    if let Some(since) = since {
        let walk = walker::walk_dir(scope, &walker::WalkOptions::default());
        dirty.extend(
            walk.listed_files
                .par_iter()
                .filter(|path| {
                    std::fs::symlink_metadata(path)
                        .is_ok_and(|metadata| changed(&metadata) >= since)
                })
                .cloned()
                .collect::<Vec<_>>(),
        );
    }
    dirty
}

/// The dirty files a text or symbol query reads: present, admitted, not binary.
pub(crate) fn fresh(plan: &Plan, scope: &Path, dirty: &BTreeSet<PathBuf>) -> Vec<PathBuf> {
    dirty
        .iter()
        .filter(|path| path.is_file() && !walker::is_binary_extension(path))
        .filter(|path| plan.admits(&relative(scope, path)))
        .cloned()
        .collect()
}

/// The later of a file's content change and its status change: a rename moves the
/// second, not the first.
fn changed(metadata: &std::fs::Metadata) -> SystemTime {
    let modified = metadata.modified().unwrap_or(UNIX_EPOCH);
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        let status = UNIX_EPOCH
            + Duration::new(
                u64::try_from(metadata.ctime()).unwrap_or(0),
                u32::try_from(metadata.ctime_nsec()).unwrap_or(0),
            );
        modified.max(status)
    }
    #[cfg(not(unix))]
    {
        modified
    }
}
