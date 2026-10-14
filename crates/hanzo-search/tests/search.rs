//! The engine against a naive scan, rg, its own index, a tgrep server, and edits.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::process::Child;
use std::process::Command;
use std::process::Stdio;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use hanzo_search::Format;
use hanzo_search::Hit;
use hanzo_search::Index;
use hanzo_search::Mark;
use hanzo_search::Query;
use hanzo_search::Resolver;
use hanzo_search::Search;
use hanzo_search::render;
use pretty_assertions::assert_eq;
use regex::Regex;

fn write(root: &Path, rel: &str, text: &str) {
    let path = root.join(rel);
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(path, text).unwrap();
}

/// A small repository: code, an ignored directory and file, a hidden file, a binary.
fn tree() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    std::fs::create_dir(root.join(".git")).unwrap();
    write(root, ".gitignore", "ignored/\n*.log\n");
    write(
        root,
        "src/main.rs",
        "fn main() {\n    let needle = 1;\n    println!(\"{needle}\");\n}\n",
    );
    write(
        root,
        "src/lib.rs",
        "pub fn needle() -> u32 {\n    7\n}\n// needle again\n",
    );
    write(root, "src/deep/mod.rs", "mod x;\nconst NEEDLE: u8 = 1;\n");
    write(
        root,
        "docs/notes.txt",
        "a needle in a haystack\nnothing here\r\nneedle at crlf\r\n",
    );
    write(root, "ignored/skip.rs", "needle ignored\n");
    write(root, "debug.log", "needle in a log\n");
    write(root, ".hidden.rs", "needle hidden\n");
    std::fs::write(root.join("blob.bin"), b"needle\0binary").unwrap();
    dir
}

/// What rg reports, written out by hand: every visible, non-ignored text file,
/// line by line.
fn naive(root: &Path, pattern: &str) -> BTreeSet<(String, u64, String)> {
    let regex = Regex::new(pattern).unwrap();
    let mut out = BTreeSet::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(&dir).unwrap() {
            let path = entry.unwrap().path();
            let rel = path
                .strip_prefix(root)
                .unwrap()
                .to_string_lossy()
                .into_owned();
            let name = path.file_name().unwrap().to_string_lossy().into_owned();
            if name.starts_with('.') || rel == "ignored" || name.ends_with(".log") {
                continue;
            }
            if path.is_dir() {
                stack.push(path);
                continue;
            }
            let bytes = std::fs::read(&path).unwrap();
            if bytes.contains(&0) {
                continue;
            }
            let text = String::from_utf8(bytes).unwrap();
            for (i, line) in text.lines().enumerate() {
                if regex.is_match(line) {
                    out.insert((rel.clone(), i as u64 + 1, line.to_string()));
                }
            }
        }
    }
    out
}

fn lines(hits: &[Hit]) -> BTreeSet<(String, u64, String)> {
    hits.iter()
        .map(|hit| {
            (
                hit.path.to_string_lossy().into_owned(),
                hit.line.unwrap(),
                hit.excerpt.clone(),
            )
        })
        .collect()
}

fn names(hits: &[Hit]) -> Vec<String> {
    hits.iter()
        .map(|hit| hit.path.to_string_lossy().into_owned())
        .collect()
}

fn text(root: &Path, pattern: &str, index: Index) -> Vec<Hit> {
    Search::new()
        .query(&Query {
            index,
            ..Query::text(root, pattern)
        })
        .unwrap()
}

#[test]
fn a_scan_finds_what_a_naive_scan_finds() {
    let dir = tree();
    for pattern in ["needle", "(?i)NEEDLE", r"fn \w+\(", "crlf$", "^nothing"] {
        assert_eq!(
            lines(&text(dir.path(), pattern, Index::Skip)),
            naive(dir.path(), pattern),
            "{pattern}"
        );
    }
}

#[test]
fn a_scan_finds_what_rg_finds() {
    let Ok(output) = Command::new("rg").arg("--version").output() else {
        return;
    };
    assert!(output.status.success());
    let dir = tree();
    // Not `crlf$`: tgrep strips a line's `\r` as `rg --crlf` does, and plain rg does not.
    for pattern in ["needle", r"fn \w+\(", "^nothing"] {
        let output = Command::new("rg")
            .args(["-n", "--no-heading", "--", pattern, "."])
            .current_dir(dir.path())
            .output()
            .unwrap();
        let rg: BTreeSet<(String, u64, String)> = String::from_utf8(output.stdout)
            .unwrap()
            .lines()
            .map(|line| {
                let mut parts = line.splitn(3, ':');
                let path = parts.next().unwrap().trim_start_matches("./").to_string();
                let number = parts.next().unwrap().parse().unwrap();
                (path, number, parts.next().unwrap().to_string())
            })
            .collect();
        assert_eq!(
            lines(&text(dir.path(), pattern, Index::Skip)),
            rg,
            "{pattern}"
        );
    }
}

#[test]
fn with_no_index_auto_scans_and_only_refuses() {
    let dir = tree();
    assert_eq!(
        lines(&text(dir.path(), "needle", Index::Auto)),
        naive(dir.path(), "needle")
    );
    let error = Search::new()
        .query(&Query {
            index: Index::Only,
            ..Query::text(dir.path(), "needle")
        })
        .unwrap_err();
    assert!(
        matches!(error, hanzo_search::Error::Unindexed(_)),
        "{error}"
    );
}

#[test]
fn the_index_answers_as_the_scan_does() {
    let dir = tree();
    let search = Search::new();
    let root = search.prepare(dir.path()).unwrap();
    assert!(root.ready);
    assert_eq!(root.server, None);
    for pattern in ["needle", r"fn \w+\(", "crlf$", "NEEDLE|haystack", "x"] {
        let indexed = search
            .query(&Query {
                index: Index::Only,
                ..Query::text(dir.path(), pattern)
            })
            .unwrap();
        assert_eq!(lines(&indexed), naive(dir.path(), pattern), "{pattern}");
    }
    let status = search.status();
    assert_eq!(status.roots.len(), 1);
    assert!(status.roots[0].files > 0 && status.roots[0].bytes > 0);
}

#[test]
fn an_edit_is_found_by_the_next_search() {
    let dir = tree();
    let search = Search::new();
    search.prepare(dir.path()).unwrap();
    let path = dir.path().join("src/lib.rs");
    let mut source = std::fs::read_to_string(&path).unwrap();
    source.push_str("// zq_unique_8213\n");
    std::fs::write(&path, &source).unwrap();
    search.touched(&path);
    let hits = search
        .query(&Query {
            index: Index::Only,
            ..Query::text(dir.path(), "zq_unique_8213")
        })
        .unwrap();
    assert_eq!(names(&hits), vec!["src/lib.rs"]);
    assert_eq!(hits[0].line, Some(5));
}

#[test]
fn a_write_after_since_is_found_and_an_unnamed_one_waits_for_the_index() {
    let dir = tree();
    let search = Search::new();
    search.prepare(dir.path()).unwrap();
    // Another process wrote this: an on-disk index has no watcher, so only a scan
    // sees it until the index is rebuilt.
    write(dir.path(), "src/new.rs", "mod y_fresh;\n");
    assert_eq!(text(dir.path(), "y_fresh", Index::Only), Vec::new());
    assert_eq!(
        names(&text(dir.path(), "y_fresh", Index::Skip)),
        vec!["src/new.rs"]
    );
    // Dev started a command that may write anything: what changed from then on is
    // read from disk, a same-size edit within the same second included.
    search.since(SystemTime::now() - Duration::from_millis(5));
    write(
        dir.path(),
        "src/main.rs",
        "fn main() { grown_token_31(); }\n",
    );
    write(
        dir.path(),
        "src/deep/mod.rs",
        "mod y;\nconst NEEDLE: u8 = 1;\n",
    );
    let only = |pattern: &str| {
        names(
            &search
                .query(&Query {
                    index: Index::Only,
                    ..Query::text(dir.path(), pattern)
                })
                .unwrap(),
        )
    };
    assert_eq!(only("grown_token_31"), vec!["src/main.rs"]);
    assert_eq!(only(r"mod y\b"), vec!["src/deep/mod.rs"]);
}

#[test]
fn a_deleted_or_renamed_file_is_not_reported_stale() {
    let dir = tree();
    let search = Search::new();
    search.prepare(dir.path()).unwrap();
    std::fs::remove_file(dir.path().join("src/main.rs")).unwrap();
    search.touched(&dir.path().join("src/main.rs"));
    std::fs::rename(
        dir.path().join("src/lib.rs"),
        dir.path().join("src/moved.rs"),
    )
    .unwrap();
    search.touched(&dir.path().join("src/lib.rs"));
    search.touched(&dir.path().join("src/moved.rs"));
    let hits = search
        .query(&Query {
            index: Index::Only,
            ..Query::text(dir.path(), "needle")
        })
        .unwrap();
    let found: BTreeSet<String> = names(&hits).into_iter().collect();
    assert!(!found.contains("src/main.rs"), "{found:?}");
    assert!(!found.contains("src/lib.rs"), "{found:?}");
    assert!(found.contains("src/moved.rs"), "{found:?}");
    assert_eq!(lines(&hits), naive(dir.path(), "needle"));
}

#[test]
fn a_root_outside_the_query_root_answers_for_its_paths() {
    let here = tree();
    let there = tree();
    let search = Search::new();
    search.prepare(there.path()).unwrap();
    write(there.path(), "src/far.rs", "far_token_55\n");
    search.touched(&there.path().join("src/far.rs"));
    let hits = search
        .query(&Query {
            paths: vec![there.path().join("src")],
            index: Index::Only,
            ..Query::text(here.path(), "far_token_55")
        })
        .unwrap();
    assert_eq!(hits.len(), 1);
    assert_eq!(
        hits[0].path,
        there.path().canonicalize().unwrap().join("src/far.rs")
    );
    let status = search.status();
    assert_eq!(
        status
            .roots
            .iter()
            .map(|root| root.path.clone())
            .collect::<Vec<_>>(),
        vec![there.path().canonicalize().unwrap()]
    );
    assert_eq!(status.touched, 1);
}

#[test]
fn names_match_by_file_name_or_path_and_stay_fresh() {
    let dir = tree();
    let search = Search::new();
    let name = |glob: &str, index: Index| {
        names(
            &search
                .query(&Query {
                    index,
                    ..Query::name(dir.path(), glob)
                })
                .unwrap(),
        )
    };
    assert_eq!(
        name("*.rs", Index::Skip),
        vec!["src/lib.rs", "src/main.rs", "src/deep/mod.rs"]
    );
    assert_eq!(
        name("src/*.rs", Index::Skip),
        vec!["src/lib.rs", "src/main.rs"]
    );
    assert_eq!(name("notes.*", Index::Skip), vec!["docs/notes.txt"]);
    assert_eq!(name("*.bin", Index::Skip), vec!["blob.bin"]);
    search.prepare(dir.path()).unwrap();
    write(dir.path(), "src/added.rs", "\n");
    search.touched(&dir.path().join("src/added.rs"));
    std::fs::remove_file(dir.path().join("src/main.rs")).unwrap();
    assert_eq!(
        name("src/*.rs", Index::Only),
        vec!["src/added.rs", "src/lib.rs"]
    );
}

struct Drop1;

impl Resolver for Drop1 {
    fn resolve(&self, _symbol: &str, hits: Vec<Hit>) -> Vec<Hit> {
        hits.into_iter()
            .filter(|hit| !hit.excerpt.contains("not really"))
            .collect()
    }
}

#[test]
fn a_symbol_ranks_its_definitions_first_and_a_resolver_confirms() {
    let dir = tempfile::tempdir().unwrap();
    write(
        dir.path(),
        "a_use.rs",
        "widget();\nwidget(); // twice\nwidgets();\n",
    );
    write(dir.path(), "b_def.rs", "pub fn widget() {}\n");
    write(
        dir.path(),
        "c_py.py",
        "def widget(x):\n    return widget # not really\n",
    );
    let query = Query::symbol(dir.path(), "widget");
    let hits = Search::new().query(&query).unwrap();
    let got: Vec<(String, u64, Mark)> = hits
        .iter()
        .map(|hit| {
            (
                hit.path.to_string_lossy().into_owned(),
                hit.line.unwrap(),
                hit.mark,
            )
        })
        .collect();
    assert_eq!(
        got,
        vec![
            ("c_py.py".to_string(), 1, Mark::Definition),
            ("c_py.py".to_string(), 2, Mark::Reference),
            ("b_def.rs".to_string(), 1, Mark::Definition),
            ("a_use.rs".to_string(), 1, Mark::Reference),
            ("a_use.rs".to_string(), 2, Mark::Reference),
        ]
    );
    let resolved = Search::new()
        .with_resolver(Arc::new(Drop1))
        .query(&query)
        .unwrap();
    assert_eq!(resolved.len(), 4);
    assert!(
        resolved
            .iter()
            .all(|hit| !hit.excerpt.contains("not really"))
    );
}

#[test]
fn text_ranks_files_by_hits_then_path_then_line() {
    let dir = tempfile::tempdir().unwrap();
    write(dir.path(), "a.txt", "k\n");
    write(dir.path(), "b.txt", "k\nk\nk\n");
    write(dir.path(), "c.txt", "k\n");
    let hits = Search::new()
        .query(&Query {
            limit: Some(4),
            ..Query::text(dir.path(), "k")
        })
        .unwrap();
    let got: Vec<(String, u64, u64)> = hits
        .iter()
        .map(|hit| {
            (
                hit.path.to_string_lossy().into_owned(),
                hit.line.unwrap(),
                hit.score,
            )
        })
        .collect();
    assert_eq!(
        got,
        vec![
            ("b.txt".to_string(), 1, 3),
            ("b.txt".to_string(), 2, 3),
            ("b.txt".to_string(), 3, 3),
            ("a.txt".to_string(), 1, 1),
        ]
    );
}

#[test]
fn render_prints_rg_lines_with_merged_context() {
    let dir = tempfile::tempdir().unwrap();
    write(
        dir.path(),
        "f.txt",
        "1\n2 hit\n3\n4 hit\n5\n6\n7\n8 hit\n9\n",
    );
    let hits = Search::new()
        .query(&Query {
            context: 1,
            ..Query::text(dir.path(), "hit")
        })
        .unwrap();
    assert_eq!(
        render(&hits, Format::Lines),
        "f.txt-1-1\nf.txt:2:2 hit\nf.txt-3-3\nf.txt:4:4 hit\nf.txt-5-5\n--\nf.txt-7-7\nf.txt:8:8 hit\nf.txt-9-9\n"
    );
    assert_eq!(render(&hits, Format::Count), "f.txt:3\n");
    assert_eq!(render(&hits, Format::Files), "f.txt\n");
    let json = render(&hits[..1], Format::Json);
    let value: serde_json::Value = serde_json::from_str(json.trim()).unwrap();
    assert_eq!(value["path"], "f.txt");
    assert_eq!(value["line"], 2);
    assert_eq!(value["mark"], "match");
    assert_eq!(value["entry"], serde_json::Value::Null);
}

#[test]
fn a_document_location_prints_before_the_line() {
    let hit = Hit {
        path: PathBuf::from("docs/a.zip"),
        entry: Some("inner/b.docx".to_string()),
        place: Some("page 2".to_string()),
        line: Some(14),
        excerpt: "text".to_string(),
        before: Vec::new(),
        after: Vec::new(),
        mark: Mark::Match,
        score: 1,
    };
    assert_eq!(
        render(&[hit], Format::Lines),
        "docs/a.zip!inner/b.docx:page 2:14:text\n"
    );
}

/// A `tgrep serve` from PATH, stopped on drop. It stands in for the per-user
/// service; Dev's code never starts it.
struct Served(Child);

impl Drop for Served {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn serve(root: &Path, search: &Search) -> Option<Served> {
    let child = Command::new("tgrep")
        .args(["serve", "--no-watch", "."])
        .current_dir(root)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .ok()?;
    let served = Served(child);
    let deadline = Instant::now() + Duration::from_secs(120);
    while Instant::now() < deadline {
        let status = search.status_of(root);
        if status
            .roots
            .iter()
            .any(|root| root.server.is_some() && root.ready)
        {
            return Some(served);
        }
        std::thread::sleep(Duration::from_millis(100));
    }
    panic!("tgrep serve did not become ready");
}

#[test]
fn a_running_server_answers_and_its_lag_is_bypassed() {
    let dir = tree();
    let search = Search::new();
    let Some(_served) = serve(dir.path(), &search) else {
        return;
    };
    let status = search.status_of(dir.path());
    assert_eq!(status.roots.len(), 1);
    assert!(status.roots[0].server.is_some());
    assert_eq!(
        lines(&text(dir.path(), "needle", Index::Only)),
        naive(dir.path(), "needle")
    );

    // The server watches nothing: it cannot see this edit until told.
    let path = dir.path().join("src/lib.rs");
    let mut source = std::fs::read_to_string(&path).unwrap();
    source.push_str("// served_token_771\n");
    std::fs::write(&path, &source).unwrap();
    let query = Query {
        index: Index::Only,
        ..Query::text(dir.path(), "served_token_771")
    };
    assert_eq!(search.query(&query).unwrap(), Vec::new(), "the server lags");
    search.touched(&path);
    assert_eq!(names(&search.query(&query).unwrap()), vec!["src/lib.rs"]);

    // A shell command wrote a file nobody named.
    search.since(SystemTime::now() - Duration::from_millis(5));
    write(dir.path(), "src/shell.rs", "served_token_771\n");
    let found: BTreeSet<String> = names(&search.query(&query).unwrap()).into_iter().collect();
    assert_eq!(
        found,
        BTreeSet::from(["src/lib.rs".to_string(), "src/shell.rs".to_string()])
    );

    // Deleted under the server: its rows are dropped.
    std::fs::remove_file(dir.path().join("src/main.rs")).unwrap();
    let hits = text(dir.path(), "needle", Index::Only);
    assert!(!names(&hits).contains(&"src/main.rs".to_string()));

    let named = search
        .query(&Query {
            index: Index::Only,
            ..Query::name(dir.path(), "src/*.rs")
        })
        .unwrap();
    assert_eq!(names(&named), vec!["src/lib.rs", "src/shell.rs"]);
}

/// Document and archive fixtures, written by hand: stored zip, tar, gzip with a
/// stored deflate block, and a two-page PDF.
mod docs {
    fn crc32(data: &[u8]) -> u32 {
        let mut crc = 0xFFFF_FFFFu32;
        for &byte in data {
            crc ^= u32::from(byte);
            for _ in 0..8 {
                crc = if crc & 1 == 1 {
                    (crc >> 1) ^ 0xEDB8_8320
                } else {
                    crc >> 1
                };
            }
        }
        !crc
    }

    pub fn zip(files: &[(&str, &[u8])]) -> Vec<u8> {
        let mut out = Vec::new();
        let mut central = Vec::new();
        for (name, data) in files {
            let offset = out.len() as u32;
            let crc = crc32(data);
            let head = |signature: u32, central: bool| {
                let mut h = Vec::new();
                h.extend(signature.to_le_bytes());
                if central {
                    h.extend(20u16.to_le_bytes());
                }
                h.extend(20u16.to_le_bytes());
                h.extend([0u8; 4]); // flags, stored
                h.extend([0u8; 4]); // time, date
                h.extend(crc.to_le_bytes());
                h.extend((data.len() as u32).to_le_bytes());
                h.extend((data.len() as u32).to_le_bytes());
                h.extend((name.len() as u16).to_le_bytes());
                h.extend(0u16.to_le_bytes());
                if central {
                    h.extend([0u8; 8]); // comment, disk, attributes
                    h.extend([0u8; 4]);
                    h.extend(offset.to_le_bytes());
                }
                h.extend(name.as_bytes());
                h
            };
            out.extend(head(0x0403_4b50, false));
            out.extend(*data);
            central.extend(head(0x0201_4b50, true));
        }
        let start = out.len() as u32;
        out.extend(&central);
        out.extend(0x0605_4b50u32.to_le_bytes());
        out.extend([0u8; 4]);
        out.extend((files.len() as u16).to_le_bytes());
        out.extend((files.len() as u16).to_le_bytes());
        out.extend((central.len() as u32).to_le_bytes());
        out.extend(start.to_le_bytes());
        out.extend(0u16.to_le_bytes());
        out
    }

    pub fn tar(files: &[(&str, &[u8])]) -> Vec<u8> {
        let mut out = Vec::new();
        for (name, data) in files {
            let mut header = [0u8; 512];
            header[..name.len()].copy_from_slice(name.as_bytes());
            header[100..108].copy_from_slice(b"0000644\0");
            header[108..116].copy_from_slice(b"0000000\0");
            header[116..124].copy_from_slice(b"0000000\0");
            header[124..136].copy_from_slice(format!("{:011o}\0", data.len()).as_bytes());
            header[136..148].copy_from_slice(b"00000000000\0");
            header[156] = b'0';
            header[257..263].copy_from_slice(b"ustar\0");
            header[263..265].copy_from_slice(b"00");
            header[148..156].copy_from_slice(b"        ");
            let sum: u32 = header.iter().map(|&b| u32::from(b)).sum();
            header[148..156].copy_from_slice(format!("{sum:06o}\0 ").as_bytes());
            out.extend(header);
            out.extend(*data);
            out.extend(std::iter::repeat_n(0u8, (512 - data.len() % 512) % 512));
        }
        out.extend([0u8; 1024]);
        out
    }

    pub fn gzip(data: &[u8]) -> Vec<u8> {
        let mut out = vec![0x1f, 0x8b, 8, 0, 0, 0, 0, 0, 0, 255];
        let mut chunks = data.chunks(65_535).peekable();
        if chunks.peek().is_none() {
            out.extend([1, 0, 0, 0xff, 0xff]);
        }
        while let Some(chunk) = chunks.next() {
            out.push(u8::from(chunks.peek().is_none()));
            let len = chunk.len() as u16;
            out.extend(len.to_le_bytes());
            out.extend((!len).to_le_bytes());
            out.extend(chunk);
        }
        out.extend(crc32(data).to_le_bytes());
        out.extend((data.len() as u32).to_le_bytes());
        out
    }

    pub fn docx(paragraphs: &[&str]) -> Vec<u8> {
        let body: String = paragraphs
            .iter()
            .map(|text| format!("<w:p><w:r><w:t>{text}</w:t></w:r></w:p>"))
            .collect();
        let document = format!(
            "<?xml version=\"1.0\"?><w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body>{body}</w:body></w:document>"
        );
        zip(&[
            ("[Content_Types].xml", b"<?xml version=\"1.0\"?><Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"/>"),
            ("word/document.xml", document.as_bytes()),
        ])
    }

    pub fn pptx(slides: &[&str]) -> Vec<u8> {
        let parts: Vec<(String, String)> = slides
            .iter()
            .enumerate()
            .map(|(i, text)| {
                (
                    format!("ppt/slides/slide{}.xml", i + 1),
                    format!("<?xml version=\"1.0\"?><p:sld xmlns:p=\"http://schemas.openxmlformats.org/presentationml/2006/main\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><p:cSld><p:spTree><p:sp><p:txBody><a:p><a:r><a:t>{text}</a:t></a:r></a:p></p:txBody></p:sp></p:spTree></p:cSld></p:sld>"),
                )
            })
            .collect();
        let files: Vec<(&str, &[u8])> = parts
            .iter()
            .map(|(name, xml)| (name.as_str(), xml.as_bytes()))
            .collect();
        zip(&files)
    }

    pub fn xlsx(sheet: &str, cell: &str, text: &str) -> Vec<u8> {
        let workbook = format!(
            "<?xml version=\"1.0\"?><workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheets><sheet name=\"{sheet}\" sheetId=\"1\" r:id=\"rId1\"/></sheets></workbook>"
        );
        let rels = "<?xml version=\"1.0\"?><Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet1.xml\"/></Relationships>";
        let data = format!(
            "<?xml version=\"1.0\"?><worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><sheetData><row r=\"{row}\"><c r=\"{cell}\" t=\"inlineStr\"><is><t>{text}</t></is></c></row></sheetData></worksheet>",
            row = cell.trim_start_matches(char::is_alphabetic)
        );
        zip(&[
            ("xl/workbook.xml", workbook.as_bytes()),
            ("xl/_rels/workbook.xml.rels", rels.as_bytes()),
            ("xl/worksheets/sheet1.xml", data.as_bytes()),
        ])
    }

    pub fn pdf(pages: &[&str]) -> Vec<u8> {
        let mut objects: Vec<String> = Vec::new();
        let kids: Vec<String> = (0..pages.len())
            .map(|i| format!("{} 0 R", 4 + 2 * i))
            .collect();
        objects.push("<< /Type /Catalog /Pages 2 0 R >>".to_string());
        objects.push(format!(
            "<< /Type /Pages /Kids [{}] /Count {} >>",
            kids.join(" "),
            pages.len()
        ));
        objects.push("<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>".to_string());
        for (i, text) in pages.iter().enumerate() {
            let stream = format!("BT /F1 12 Tf 72 720 Td ({text}) Tj ET");
            objects.push(format!(
                "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 3 0 R >> >> /Contents {} 0 R >>",
                5 + 2 * i
            ));
            objects.push(format!(
                "<< /Length {} >>\nstream\n{stream}\nendstream",
                stream.len()
            ));
        }
        let mut out = b"%PDF-1.4\n".to_vec();
        let mut offsets = Vec::new();
        for (i, object) in objects.iter().enumerate() {
            offsets.push(out.len());
            out.extend(format!("{} 0 obj\n{object}\nendobj\n", i + 1).as_bytes());
        }
        let xref = out.len();
        out.extend(format!("xref\n0 {}\n0000000000 65535 f \n", objects.len() + 1).as_bytes());
        for offset in offsets {
            out.extend(format!("{offset:010} 00000 n \n").as_bytes());
        }
        out.extend(
            format!(
                "trailer\n<< /Size {} /Root 1 0 R >>\nstartxref\n{xref}\n%%EOF\n",
                objects.len() + 1
            )
            .as_bytes(),
        );
        out
    }
}

/// What a document or archive hit must say: the file, the members inside it, and the
/// place in the document.
fn located(hits: &[Hit]) -> BTreeSet<(String, Option<String>, Option<String>)> {
    hits.iter()
        .map(|hit| {
            (
                hit.path.to_string_lossy().into_owned(),
                hit.entry.clone(),
                hit.place.clone(),
            )
        })
        .collect()
}

#[test]
#[ignore = "needs the document adapters of the tgrep fork (hanzoai/tgrep) in tgrep-core"]
fn documents_and_archives_report_where_the_hit_is() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    std::fs::write(
        root.join("a.pdf"),
        docs::pdf(&["first page", "second doctoken"]),
    )
    .unwrap();
    std::fs::write(root.join("b.docx"), docs::docx(&["intro", "body doctoken"])).unwrap();
    std::fs::write(
        root.join("c.pptx"),
        docs::pptx(&["title", "slide doctoken"]),
    )
    .unwrap();
    std::fs::write(
        root.join("d.xlsx"),
        docs::xlsx("Data", "B4", "cell doctoken"),
    )
    .unwrap();
    let docx = docs::docx(&["zipped doctoken"]);
    std::fs::write(root.join("e.zip"), docs::zip(&[("inner/b.docx", &docx)])).unwrap();
    let zipped = docs::zip(&[("deep.txt", b"nested doctoken\n")]);
    let tar = docs::tar(&[
        ("x/inner.zip", &zipped),
        ("plain.txt", b"tarred doctoken\n"),
    ]);
    std::fs::write(root.join("f.tar.gz"), docs::gzip(&tar)).unwrap();

    let search = Search::new();
    let expect = BTreeSet::from([
        ("a.pdf".to_string(), None, Some("page 2".to_string())),
        ("b.docx".to_string(), None, Some("paragraph 2".to_string())),
        ("c.pptx".to_string(), None, Some("slide 2".to_string())),
        (
            "d.xlsx".to_string(),
            None,
            Some("sheet Data!B4".to_string()),
        ),
        (
            "e.zip".to_string(),
            Some("inner/b.docx".to_string()),
            Some("paragraph 1".to_string()),
        ),
        (
            "f.tar.gz".to_string(),
            Some("x/inner.zip!deep.txt".to_string()),
            None,
        ),
        ("f.tar.gz".to_string(), Some("plain.txt".to_string()), None),
    ]);
    for index in [Index::Skip, Index::Auto] {
        if index == Index::Auto {
            search.prepare(root).unwrap();
        }
        let hits = search
            .query(&Query {
                index,
                ..Query::text(root, "doctoken")
            })
            .unwrap();
        assert_eq!(located(&hits), expect, "{index:?}");
    }
}
