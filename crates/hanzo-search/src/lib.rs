//! Dev's one search engine: text, file names and symbols, over tgrep's trigram index.
//!
//! Every search in Dev goes through [`Search::query`]: the agent's `grep` tool
//! (crate `hanzo-grep`), the loop, and the `hanzo-search` command. There is no
//! second path. The engine is [tgrep](https://github.com/microsoft/tgrep) (MIT),
//! linked as the `tgrep-core` library and pinned by exact commit in `Cargo.toml`;
//! nothing is spawned.
//!
//! # API
//!
//! ```no_run
//! use hanzo_search::{Query, Search};
//!
//! let search = Search::new();
//! let hits = search.query(&Query::text("/repo", "fn main"))?;
//! for hit in &hits {
//!     println!("{}:{}:{}", hit.path.display(), hit.line.unwrap_or(0), hit.excerpt);
//! }
//! search.touched(std::path::Path::new("/repo/src/lib.rs")); // Dev wrote it
//! let status = search.status();
//! # Ok::<(), hanzo_search::Error>(())
//! ```
//!
//! A [`Query`] has one of three kinds:
//!
//! - [`Kind::Text`]: a regex (or a literal with `fixed`) matched line by line, as `rg`.
//! - [`Kind::Name`]: a glob over file paths, as `rg --files -g`. A glob without `/`
//!   matches the file name at any depth; one with `/` matches the path relative to
//!   the searched directory.
//! - [`Kind::Symbol`]: an identifier matched on word boundaries. Each hit is marked
//!   [`Mark::Definition`] when the line reads like a declaration of it (`fn x`,
//!   `def x`, `class x`, `const x`, `func (r) x`, `x = function`, `#define x`, ...)
//!   and [`Mark::Reference`] otherwise. This is textual: it finds the lines that
//!   spell the name, not every semantic caller, and it can call a comment a
//!   reference. A [`Resolver`] installed with [`Search::with_resolver`] (Dev's
//!   LSP or code graph) confirms or drops the candidates.
//!
//! # Ranking
//!
//! Results are ordered by file score, highest first, then path, then line. The
//! score is per file:
//!
//! - Text: the number of matching lines in the file.
//! - Symbol: definitions × 2³² + references, so every file that defines the name
//!   comes before every file that only uses it.
//! - Name: `u64::MAX` less the path's depth, so shallower paths come first.
//!
//! `limit` keeps the first `limit` hits of that order.
//!
//! # Output
//!
//! [`render`] prints hits the way `rg -n --no-heading` does: `path:line:text` for
//! a match, `path-line-text` for a context line, `--` between context groups that
//! are not adjacent, `path` for a name hit, `path:N` for a count. A hit inside a
//! document or archive (from the tgrep fork's document adapters) prints its
//! archive members after the path, each behind `!`, and its place before the
//! line: `docs/a.zip!inner/b.docx:page 2:14:text`. JSON prints one object per hit
//! with the fields of [`Hit`].
//!
//! # Engines and fallback
//!
//! A query path belongs to the nearest enclosing directory with a tgrep index
//! (`.tgrep/`). For that root, in order:
//!
//! 1. a running tgrep server (found by `.tgrep/serve.json`) answers over its
//!    loopback JSON-RPC;
//! 2. else the on-disk index answers in process: trigram candidates, then the
//!    regex over each candidate's current bytes;
//! 3. else, or with no index at all, the tree is scanned with tgrep's walker,
//!    which applies `.gitignore`, hidden-file and binary rules as `rg` does.
//!
//! [`Index::Skip`] forces the scan; [`Index::Only`] refuses it.
//!
//! # Freshness
//!
//! An index can lag the disk; a search after Dev wrote a file must still see the
//! write. So, before a root's index answers, its dirty files are read from disk
//! instead, and the index's rows for them are dropped:
//!
//! - every path given to [`Search::touched`] (Dev named the file it wrote);
//! - with a time given to [`Search::since`] (Dev started a tool that may write
//!   files it does not name), every file whose content or name (ctime) changed at
//!   or after it, found by a walk of the searched directory: names and metadata,
//!   no contents.
//!
//! For a server's root, a touch or `since` older than the start of the server's
//! last full reconcile no longer counts: its watcher has the write. An on-disk
//! index never catches up, so there the walk repeats on every query once `since`
//! is set; that walk is what freshness costs without a watcher. A write by another
//! process is seen by a server's watcher, and by an on-disk index only once it is
//! rebuilt, as tgrep documents.
//!
//! A row whose file no longer exists is dropped, so a deleted or renamed file is
//! never reported from the index.
//!
//! # Licence boundary
//!
//! tgrep is MIT and linked. ripgrep-all (`rga`) is AGPL-3.0 and is never used:
//! not linked, copied, called, shipped, or used as a comparator.

mod find;
mod index;
mod render;
mod server;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;
use std::time::SystemTime;

use serde::Deserialize;
use serde::Serialize;

pub use render::Format;
pub use render::render;

/// What a query looks for.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Kind {
    /// A regex, or a literal with [`Query::fixed`], matched per line.
    #[default]
    Text,
    /// A glob over file paths.
    Name,
    /// An identifier on word boundaries, its definitions ranked first.
    Symbol,
}

/// Which engine may answer.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Index {
    /// A server, else the on-disk index, else a scan.
    #[default]
    Auto,
    /// Scan the files; never read an index.
    Skip,
    /// A server or the on-disk index; a path with neither is an error.
    Only,
}

/// One search.
#[derive(Clone, Debug, Default)]
pub struct Query {
    pub kind: Kind,
    /// The regex, glob or identifier.
    pub pattern: String,
    /// Where relative paths resolve and what hit paths are shown relative to.
    pub root: PathBuf,
    /// Files or directories to search; empty searches `root`.
    pub paths: Vec<PathBuf>,
    /// Text and symbol queries read only files whose path matches one of these.
    pub glob: Vec<String>,
    /// The pattern is a literal.
    pub fixed: bool,
    pub ignore_case: bool,
    /// Lines of context on each side of a text or symbol hit.
    pub context: usize,
    /// The most hits returned, best first.
    pub limit: Option<usize>,
    pub index: Index,
}

impl Query {
    /// A regex over the files under `root`.
    pub fn text(root: impl Into<PathBuf>, pattern: impl Into<String>) -> Query {
        Query {
            kind: Kind::Text,
            pattern: pattern.into(),
            root: root.into(),
            ..Query::default()
        }
    }

    /// A glob over the file paths under `root`.
    pub fn name(root: impl Into<PathBuf>, glob: impl Into<String>) -> Query {
        Query {
            kind: Kind::Name,
            pattern: glob.into(),
            root: root.into(),
            ..Query::default()
        }
    }

    /// The textual definitions and references of an identifier under `root`.
    pub fn symbol(root: impl Into<PathBuf>, name: impl Into<String>) -> Query {
        Query {
            kind: Kind::Symbol,
            pattern: name.into(),
            root: root.into(),
            ..Query::default()
        }
    }
}

/// What a hit is.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Mark {
    /// A line matching a text query.
    Match,
    /// A file matching a name query.
    File,
    /// A line that reads like a declaration of the symbol.
    Definition,
    /// Any other line spelling the symbol.
    Reference,
}

/// One result.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Hit {
    /// Relative to the query's root when beneath it, else absolute.
    pub path: PathBuf,
    /// Archive members from the outer file inward, joined by `!`
    /// (`inner/b.docx` inside `a.zip`). `None` for a plain file.
    pub entry: Option<String>,
    /// Where in a document: `page 2`, `slide 3`, `sheet Data!B4`. `None` for text.
    pub place: Option<String>,
    /// 1-based. `None` for a name hit.
    pub line: Option<u64>,
    /// The matching line without its terminator; the path for a name hit.
    pub excerpt: String,
    /// Up to `context` lines before the hit, in order.
    pub before: Vec<String>,
    /// Up to `context` lines after the hit, in order.
    pub after: Vec<String>,
    pub mark: Mark,
    /// The file's score; see the crate docs for the ranking.
    pub score: u64,
}

/// Confirms textual symbol candidates semantically.
///
/// [`Kind::Symbol`] hits are lines that spell the name. A resolver (an LSP, a code
/// graph) returns the ones that really define or use the symbol, and may change
/// their [`Mark`]. Hits it drops are gone; the result is ranked again after.
pub trait Resolver: Send + Sync {
    fn resolve(&self, symbol: &str, hits: Vec<Hit>) -> Vec<Hit>;
}

/// Why a query failed.
#[derive(Debug)]
pub enum Error {
    /// The pattern or a glob does not compile.
    Pattern(String),
    /// A path cannot be read.
    Io(PathBuf, std::io::Error),
    /// [`Index::Only`] and no index answers for this path.
    Unindexed(PathBuf),
    /// An index cannot be built.
    Build(PathBuf, String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Pattern(message) => write!(f, "{message}"),
            Error::Io(path, error) => write!(f, "{}: {error}", path.display()),
            Error::Unindexed(path) => write!(f, "{}: no index answers here", path.display()),
            Error::Build(path, message) => write!(f, "{}: {message}", path.display()),
        }
    }
}

impl std::error::Error for Error {}

/// One indexed root as the engine sees it.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Root {
    pub path: PathBuf,
    /// Pid of the tgrep server answering for this root.
    pub server: Option<u32>,
    /// A complete index can answer.
    pub ready: bool,
    pub files: u64,
    /// Index size on disk.
    pub bytes: u64,
    /// When the index last caught up with the disk: the server's last reconcile, or
    /// the on-disk index's last write.
    pub refreshed: Option<SystemTime>,
    /// How long ago that was.
    pub lag: Option<Duration>,
}

/// The engine's state.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Status {
    /// Every indexed root a query has reached, and the ones given to [`Search::status_of`].
    pub roots: Vec<Root>,
    /// Paths given to [`Search::touched`].
    pub touched: usize,
    /// The earliest time given to [`Search::since`].
    pub since: Option<SystemTime>,
}

#[derive(Default)]
struct Dirty {
    touched: BTreeMap<PathBuf, SystemTime>,
    since: Option<SystemTime>,
}

/// The engine. Cheap to call; holds the dirty set and the roots it has met.
#[derive(Default)]
pub struct Search {
    dirty: Mutex<Dirty>,
    roots: Mutex<BTreeSet<PathBuf>>,
    resolver: Option<Arc<dyn Resolver>>,
}

impl Search {
    pub fn new() -> Search {
        Search::default()
    }

    /// Confirms [`Kind::Symbol`] hits through `resolver`.
    pub fn with_resolver(mut self, resolver: Arc<dyn Resolver>) -> Search {
        self.resolver = Some(resolver);
        self
    }

    /// Dev wrote, created, renamed or deleted `path` (absolute). Searches read it
    /// from disk, never from an index, unless a server reconciled after the write.
    pub fn touched(&self, path: &Path) {
        let path = canonical(path);
        if let Ok(mut dirty) = self.dirty.lock() {
            dirty.touched.insert(path, SystemTime::now());
        }
    }

    /// A tool that may write files it does not name started at `at` (a shell
    /// command). Files changed at or after `at` bypass an index, unless a server
    /// reconciled after `at`.
    pub fn since(&self, at: SystemTime) {
        if let Ok(mut dirty) = self.dirty.lock() {
            dirty.since = Some(dirty.since.map_or(at, |since| since.min(at)));
        }
    }

    /// Runs a query. See the crate docs for kinds, ranking and freshness.
    pub fn query(&self, query: &Query) -> Result<Vec<Hit>, Error> {
        if query.pattern.is_empty() {
            return Err(Error::Pattern("empty pattern".to_string()));
        }
        let plan = find::Plan::new(query)?;
        let root = absolute(&query.root);
        let scopes: Vec<PathBuf> = if query.paths.is_empty() {
            vec![root.clone()]
        } else {
            query
                .paths
                .iter()
                .map(|path| absolute(&root.join(path)))
                .collect()
        };
        let mut hits = Vec::new();
        for scope in scopes {
            let scope = scope
                .canonicalize()
                .map_err(|error| Error::Io(scope.clone(), error))?;
            hits.extend(self.scope(query, &plan, &scope)?);
        }
        let mut seen = BTreeSet::new();
        hits.retain(|hit| seen.insert((hit.path.clone(), hit.entry.clone(), hit.line)));
        rank(&mut hits);
        if let (Kind::Symbol, Some(resolver)) = (query.kind, &self.resolver) {
            hits = resolver.resolve(&query.pattern, hits);
            rank(&mut hits);
        }
        if let Some(limit) = query.limit {
            hits.truncate(limit);
        }
        let shown = root.canonicalize().unwrap_or(root);
        for hit in &mut hits {
            if let Ok(rel) = hit.path.strip_prefix(&shown) {
                hit.path = if rel.as_os_str().is_empty() {
                    PathBuf::from(".")
                } else {
                    rel.to_path_buf()
                };
            }
            if hit.mark == Mark::File {
                hit.excerpt = hit.path.display().to_string();
            }
        }
        Ok(hits)
    }

    /// The roots queries have reached, with their index state.
    pub fn status(&self) -> Status {
        let roots: Vec<PathBuf> = self
            .roots
            .lock()
            .map(|roots| roots.iter().cloned().collect())
            .unwrap_or_default();
        self.report(roots)
    }

    /// [`Search::status`] plus the root that would answer for `path`.
    pub fn status_of(&self, path: &Path) -> Status {
        let mut roots: BTreeSet<PathBuf> = self
            .roots
            .lock()
            .map(|roots| roots.clone())
            .unwrap_or_default();
        if let Some(root) = index::locate(&absolute(path)) {
            roots.insert(root);
        }
        self.report(roots.into_iter().collect())
    }

    /// Builds or rebuilds the on-disk index for `path` in `path/.tgrep`. Fails when
    /// a tgrep server owns that index.
    pub fn prepare(&self, path: &Path) -> Result<Root, Error> {
        let root = absolute(path)
            .canonicalize()
            .map_err(|error| Error::Io(path.to_path_buf(), error))?;
        index::build(&root)?;
        self.note(&root);
        Ok(index::root(&root))
    }

    fn report(&self, roots: Vec<PathBuf>) -> Status {
        let (touched, since) = self
            .dirty
            .lock()
            .map(|dirty| (dirty.touched.len(), dirty.since))
            .unwrap_or_default();
        Status {
            roots: roots.iter().map(|root| index::root(root)).collect(),
            touched,
            since,
        }
    }

    fn note(&self, root: &Path) {
        if let Ok(mut roots) = self.roots.lock() {
            roots.insert(root.to_path_buf());
        }
    }

    /// What bypasses `root`'s index: the paths touched under it, and the earliest
    /// unnamed write, each only when it began at or after `after` (an index that
    /// reconciled at `after` holds what came before).
    fn dirt(&self, root: &Path, after: Option<SystemTime>) -> (Vec<PathBuf>, Option<SystemTime>) {
        let fresh = |when: &SystemTime| after.is_none_or(|at| *when >= at);
        self.dirty
            .lock()
            .map(|dirty| {
                (
                    dirty
                        .touched
                        .iter()
                        .filter(|(path, when)| path.starts_with(root) && fresh(when))
                        .map(|(path, _)| path.clone())
                        .collect(),
                    dirty.since.filter(fresh),
                )
            })
            .unwrap_or_default()
    }

    fn scope(&self, query: &Query, plan: &find::Plan, scope: &Path) -> Result<Vec<Hit>, Error> {
        if query.index == Index::Skip || scope.is_file() {
            return find::scan(plan, scope);
        }
        let Some(root) = index::locate(scope) else {
            return match query.index {
                Index::Only => Err(Error::Unindexed(scope.to_path_buf())),
                _ => find::scan(plan, scope),
            };
        };
        self.note(&root);
        if let Some(server) = server::attach(&root) {
            let (touched, since) = self.dirt(&root, server.reconciled());
            let dirty = find::dirty(scope, &touched, since);
            if let Ok(hits) = server.query(plan, &root, scope, &dirty) {
                return Ok(hits);
            }
        }
        let (touched, since) = self.dirt(&root, None);
        let dirty = find::dirty(scope, &touched, since);
        match index::query(plan, &root, scope, &dirty) {
            Some(hits) => hits,
            None => match query.index {
                Index::Only => Err(Error::Unindexed(scope.to_path_buf())),
                _ => find::scan(plan, scope),
            },
        }
    }
}

/// Orders hits by file score, then path, then line, after scoring each file.
fn rank(hits: &mut [Hit]) {
    let mut scores: BTreeMap<(PathBuf, Option<String>), u64> = BTreeMap::new();
    for hit in hits.iter() {
        let score = scores
            .entry((hit.path.clone(), hit.entry.clone()))
            .or_insert(0);
        *score = match hit.mark {
            Mark::Match => *score + 1,
            Mark::Definition => *score + (1 << 32),
            Mark::Reference => *score + 1,
            Mark::File => u64::MAX - hit.path.components().count() as u64,
        };
    }
    for hit in hits.iter_mut() {
        hit.score = scores
            .get(&(hit.path.clone(), hit.entry.clone()))
            .copied()
            .unwrap_or(0);
    }
    hits.sort_by(|a, b| {
        b.score
            .cmp(&a.score)
            .then_with(|| a.path.cmp(&b.path))
            .then_with(|| a.entry.cmp(&b.entry))
            .then_with(|| a.line.cmp(&b.line))
    });
}

fn absolute(path: &Path) -> PathBuf {
    std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf())
}

/// `path` with symlinks resolved, through its parent when it no longer exists.
fn canonical(path: &Path) -> PathBuf {
    let path = absolute(path);
    if let Ok(path) = path.canonicalize() {
        return path;
    }
    match (
        path.parent().and_then(|parent| parent.canonicalize().ok()),
        path.file_name(),
    ) {
        (Some(parent), Some(name)) => parent.join(name),
        _ => path,
    }
}
