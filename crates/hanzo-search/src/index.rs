//! tgrep's on-disk index: where it is, building it, and answering from it in process.

use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use tgrep_core::builder;
use tgrep_core::meta;
use tgrep_core::meta::IndexMeta;
use tgrep_core::path_index;
use tgrep_core::query;
use tgrep_core::reader::IndexReader;

use crate::Error;
use crate::Hit;
use crate::Kind;
use crate::Root;
use crate::find;
use crate::find::Plan;
use crate::server;

/// The nearest directory at or above `path` that holds a tgrep index.
pub(crate) fn locate(path: &Path) -> Option<PathBuf> {
    let start = if path.is_file() { path.parent()? } else { path };
    start
        .ancestors()
        .find(|dir| builder::default_index_dir(dir).join("meta.json").is_file())
        .map(Path::to_path_buf)
}

/// Builds the index for `root` into `root/.tgrep`, holding tgrep's writer lock, as
/// `tgrep index` does.
pub(crate) fn build(root: &Path) -> Result<(), Error> {
    let fail = |message: String| Error::Build(root.to_path_buf(), message);
    let dir = builder::default_index_dir(root);
    std::fs::create_dir_all(&dir).map_err(|error| fail(error.to_string()))?;
    let lock = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(dir.join("serve.lock"))
        .map_err(|error| fail(error.to_string()))?;
    lock.try_lock()
        .map_err(|_| fail("an index server or build owns this index".to_string()))?;
    builder::build_index_with_options(root, Some(&dir), &builder::BuildOptions::default())
        .map_err(|error| fail(error.to_string()))?;
    Ok(())
}

/// The index state of `root`.
pub(crate) fn root(root: &Path) -> Root {
    let dir = builder::default_index_dir(root);
    let meta = IndexMeta::load(&dir).ok();
    let server = server::attach(root);
    let status = server.as_ref().and_then(server::Server::status);
    let ready = match &status {
        Some(status) => status["hidden_complete"].as_bool() == Some(true),
        None => meta.as_ref().is_some_and(usable),
    };
    let files = status
        .as_ref()
        .and_then(|status| status["num_files"].as_u64())
        .or(meta.as_ref().map(|meta| meta.num_files))
        .unwrap_or(0);
    let refreshed = match &server {
        Some(server) => server.reconciled(),
        None => meta
            .as_ref()
            .map(|meta| UNIX_EPOCH + Duration::from_secs(meta.updated_at)),
    };
    let bytes = std::fs::read_dir(&dir)
        .map(|entries| {
            entries
                .filter_map(Result::ok)
                .filter_map(|entry| entry.metadata().ok())
                .filter(std::fs::Metadata::is_file)
                .map(|metadata| metadata.len())
                .sum()
        })
        .unwrap_or(0);
    Root {
        path: root.to_path_buf(),
        server: server.map(|server| server.pid),
        ready,
        files,
        bytes,
        refreshed,
        lag: refreshed.and_then(|at| SystemTime::now().duration_since(at).ok()),
    }
}

fn usable(meta: &IndexMeta) -> bool {
    meta.complete && meta.hidden_complete && meta.version == meta::INDEX_FORMAT_VERSION
}

/// `/`-separated path of `path` under `root`, empty for `root` itself.
pub(crate) fn indexed(root: &Path, path: &Path) -> String {
    find::relative(root, path)
}

/// Answers from the on-disk index of `root`, or `None` when it cannot (missing,
/// partial, or from another tgrep format).
///
/// Candidates come from the trigram index and are matched against their current
/// bytes; a candidate that no longer exists is skipped. `dirty` files are searched
/// from disk instead of the index.
pub(crate) fn query(
    plan: &Plan,
    root: &Path,
    scope: &Path,
    dirty: &BTreeSet<PathBuf>,
) -> Option<Result<Vec<Hit>, Error>> {
    let dir = builder::default_index_dir(root);
    let meta = IndexMeta::load(&dir).ok().filter(usable)?;
    let reader = IndexReader::open(&dir).ok()?;
    let names = path_index::read_filename_index(&dir).ok()??;
    let visibility = names.visibility?;
    if !visibility.covers_index(&meta, reader.file_table_id()) {
        return None;
    }
    let prefix = match indexed(root, scope) {
        rel if rel.is_empty() => String::new(),
        rel => format!("{rel}/"),
    };
    let stale: BTreeSet<String> = dirty.iter().map(|path| indexed(root, path)).collect();
    let current = |path: &&str| {
        path.starts_with(&prefix)
            && visibility.paths.is_visible(path, &prefix, false)
            && !stale.contains(*path)
    };

    if plan.kind == Kind::Name {
        let mut hits: Vec<Hit> = reader
            .all_paths()
            .iter()
            .chain(names.paths.iter())
            .map(String::as_str)
            .filter(current)
            .filter_map(|path| plan.file(&root.join(path), &path[prefix.len()..]))
            .filter(|hit| hit.path.symlink_metadata().is_ok())
            .collect();
        hits.extend(
            dirty
                .iter()
                .filter(|path| path.is_file())
                .filter_map(|path| plan.file(path, &find::relative(scope, path))),
        );
        return Some(Ok(hits));
    }

    let trigrams = match query::build_query_plan(&plan.pattern, plan.ignore_case) {
        Ok(trigrams) => trigrams,
        Err(error) => return Some(Err(Error::Pattern(error))),
    };
    let ids = if trigrams.is_match_all() {
        reader.all_file_ids()
    } else {
        query::execute_plan_with_masks(&trigrams, &|trigram| {
            reader.lookup_trigram_with_masks(trigram)
        })
    };
    let mut files: Vec<PathBuf> = ids
        .iter()
        .filter_map(|&id| reader.file_path(id))
        .filter(current)
        .filter(|path| plan.admits(&path[prefix.len()..]))
        .map(|path| root.join(path))
        .collect();
    files.extend(find::fresh(plan, scope, dirty));
    Some(Ok(find::grep(plan, &files)))
}
