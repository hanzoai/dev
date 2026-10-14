//! `hanzo-search`: Dev's search engine from the command line, printing what the
//! agent's `grep` tool returns. Exit 0 with hits, 1 without, 2 on error, as `rg`.
//!
//! ```text
//! hanzo-search [-i] [-F] [-n] [-C N] [-g GLOB]... [-l | --count | --json] [--limit N]
//!              [--no-index | --index-only] [--symbol] PATTERN [PATH...]
//! hanzo-search --files GLOB [PATH...]
//! hanzo-search --prepare PATH
//! hanzo-search --stats [PATH...]
//! ```

use std::io::Write;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use hanzo_search::Format;
use hanzo_search::Index;
use hanzo_search::Kind;
use hanzo_search::Query;
use hanzo_search::Search;
use hanzo_search::render;

#[derive(Parser)]
#[command(
    name = "hanzo-search",
    about = "Search text, file names and symbols through Dev's engine"
)]
struct Args {
    /// Case-insensitive.
    #[arg(short = 'i', long = "ignore-case")]
    ignore_case: bool,
    /// The pattern is a literal.
    #[arg(short = 'F', long = "fixed-strings")]
    fixed: bool,
    /// Print line numbers.
    #[arg(short = 'n', long = "line-number")]
    numbers: bool,
    /// Lines of context around each hit.
    #[arg(short = 'C', long = "context", default_value_t = 0)]
    context: usize,
    /// Read only files matching this glob; `!GLOB` excludes. Repeatable.
    #[arg(short = 'g', long = "glob")]
    glob: Vec<String>,
    /// Print each file with a hit.
    #[arg(short = 'l', long = "files-with-matches")]
    files_only: bool,
    /// Print hits per file.
    #[arg(short = 'c', long = "count")]
    count: bool,
    /// One JSON object per hit.
    #[arg(long)]
    json: bool,
    /// The most hits, best first.
    #[arg(long)]
    limit: Option<usize>,
    /// Scan the files; never read an index.
    #[arg(long = "no-index", conflicts_with = "index_only")]
    no_index: bool,
    /// Answer from a server or an on-disk index only; fail where there is none.
    #[arg(long = "index-only")]
    index_only: bool,
    /// PATTERN is an identifier: its definitions, then its references.
    #[arg(long)]
    symbol: bool,
    /// List files whose path matches GLOB instead of searching contents.
    #[arg(long, value_name = "GLOB", conflicts_with = "symbol")]
    files: Option<String>,
    /// Build the index for PATH (and nothing else).
    #[arg(long, value_name = "PATH", conflicts_with_all = ["files", "stats"])]
    prepare: Option<PathBuf>,
    /// Print the engine's state for the given paths.
    #[arg(long)]
    stats: bool,
    /// PATTERN then PATHs; only PATHs with --files or --stats.
    args: Vec<String>,
}

fn main() -> ExitCode {
    let args = Args::parse();
    match run(args) {
        Ok(true) => ExitCode::from(0),
        Ok(false) => ExitCode::from(1),
        Err(error) => {
            eprintln!("hanzo-search: {error}");
            ExitCode::from(2)
        }
    }
}

fn run(args: Args) -> Result<bool, Box<dyn std::error::Error>> {
    let search = Search::new();
    let cwd = std::env::current_dir()?;
    if let Some(path) = &args.prepare {
        let root = search.prepare(path)?;
        emit(&stats(&[root], args.json))?;
        return Ok(true);
    }
    if args.stats {
        let paths = if args.args.is_empty() {
            vec![cwd]
        } else {
            args.args.iter().map(|path| cwd.join(path)).collect()
        };
        let mut roots = Vec::new();
        for path in &paths {
            for root in search.status_of(path).roots {
                if !roots.contains(&root) {
                    roots.push(root);
                }
            }
        }
        emit(&stats(&roots, args.json))?;
        return Ok(!roots.is_empty());
    }
    let (kind, pattern, paths) = match args.files {
        Some(glob) => (Kind::Name, glob, args.args),
        None => {
            let mut rest = args.args.into_iter();
            let pattern = rest.next().ok_or("a PATTERN is required")?;
            let kind = if args.symbol {
                Kind::Symbol
            } else {
                Kind::Text
            };
            (kind, pattern, rest.collect())
        }
    };
    let query = Query {
        kind,
        pattern,
        root: cwd,
        paths: paths.into_iter().map(PathBuf::from).collect(),
        glob: args.glob,
        fixed: args.fixed,
        ignore_case: args.ignore_case,
        context: args.context,
        limit: args.limit,
        index: if args.no_index {
            Index::Skip
        } else if args.index_only {
            Index::Only
        } else {
            Index::Auto
        },
    };
    let hits = search.query(&query)?;
    let format = if args.json {
        Format::Json
    } else if args.files_only {
        Format::Files
    } else if args.count {
        Format::Count
    } else if args.numbers {
        Format::Lines
    } else {
        Format::Bare
    };
    emit(&render(&hits, format))?;
    Ok(!hits.is_empty())
}

/// Writes `text` to stdout; a reader that hung up is not an error.
fn emit(text: &str) -> std::io::Result<()> {
    let mut stdout = std::io::stdout().lock();
    match stdout
        .write_all(text.as_bytes())
        .and_then(|()| stdout.flush())
    {
        Err(error) if error.kind() == std::io::ErrorKind::BrokenPipe => Ok(()),
        other => other,
    }
}

/// One line per root: `path files=N bytes=N ready=yes|no server=PID|none lag=Ns|none`.
fn stats(roots: &[hanzo_search::Root], json: bool) -> String {
    let mut out = String::new();
    for root in roots {
        if json {
            if let Ok(line) = serde_json::to_string(root) {
                out.push_str(&line);
                out.push('\n');
            }
            continue;
        }
        out.push_str(&format!(
            "{} files={} bytes={} ready={} server={} lag={}\n",
            root.path.display(),
            root.files,
            root.bytes,
            if root.ready { "yes" } else { "no" },
            root.server
                .map_or("none".to_string(), |pid| pid.to_string()),
            root.lag
                .map_or("none".to_string(), |lag| format!("{}s", lag.as_secs())),
        ));
    }
    out
}
