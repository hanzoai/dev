//! A running tgrep server: found by its `serve.json`, asked over loopback JSON-RPC.
//!
//! The server is tgrep's own (`search`, `files` and `status` methods, one JSON
//! object per line). Its index updates asynchronously from a file watcher, so the
//! caller's dirty files are searched from disk and the server's rows for them
//! are dropped.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::net::SocketAddr;
use std::net::TcpStream;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use serde_json::Value;
use serde_json::json;
use tgrep_core::builder;

use crate::Hit;
use crate::Kind;
use crate::Mark;
use crate::find;
use crate::find::Plan;
use crate::index;

/// How long a server has to accept a connection before it counts as absent.
const CONNECT: Duration = Duration::from_millis(250);
/// How long a query may take.
const ANSWER: Duration = Duration::from_secs(120);

pub(crate) struct Server {
    pub pid: u32,
    address: SocketAddr,
}

/// The server serving `root`'s index, when one accepts a connection.
pub(crate) fn attach(root: &Path) -> Option<Server> {
    let info: Value = serde_json::from_slice(
        &std::fs::read(builder::default_index_dir(root).join("serve.json")).ok()?,
    )
    .ok()?;
    let port = u16::try_from(info["port"].as_u64()?).ok()?;
    let pid = u32::try_from(info["pid"].as_u64()?).ok()?;
    let address = SocketAddr::from(([127, 0, 0, 1], port));
    TcpStream::connect_timeout(&address, CONNECT).ok()?;
    Some(Server { pid, address })
}

impl Server {
    fn call(&self, method: &str, params: Value) -> Result<Value, String> {
        let stream = TcpStream::connect_timeout(&self.address, CONNECT)
            .map_err(|error| error.to_string())?;
        stream
            .set_read_timeout(Some(ANSWER))
            .map_err(|error| error.to_string())?;
        let request = json!({"jsonrpc": "2.0", "method": method, "params": params, "id": 1});
        (&stream)
            .write_all(format!("{request}\n").as_bytes())
            .map_err(|error| error.to_string())?;
        let mut line = String::new();
        BufReader::new(&stream)
            .read_line(&mut line)
            .map_err(|error| error.to_string())?;
        let mut response: Value = serde_json::from_str(&line).map_err(|error| error.to_string())?;
        if let Some(error) = response.get("error") {
            return Err(error["message"]
                .as_str()
                .unwrap_or("server error")
                .to_string());
        }
        Ok(response["result"].take())
    }

    pub fn status(&self) -> Option<Value> {
        self.call("status", json!({})).ok()
    }

    /// When the server's last full reconcile began: every write before it is in the index.
    pub fn reconciled(&self) -> Option<SystemTime> {
        let status = self.status()?;
        let finished = status["last_reconcile_at"].as_u64()?;
        let took = status["last_reconcile_duration_ms"].as_u64().unwrap_or(0);
        // Second resolution: step back a second so the start is never late.
        Some(UNIX_EPOCH + Duration::from_secs(finished.saturating_sub(took / 1000 + 1)))
    }

    /// Answers `plan` for `scope` under `root`, reading `dirty` files from disk.
    pub fn query(
        &self,
        plan: &Plan,
        root: &Path,
        scope: &Path,
        dirty: &BTreeSet<PathBuf>,
    ) -> Result<Vec<Hit>, String> {
        let prefix = index::indexed(root, scope);
        let keep = |path: &Path| !dirty.contains(path) && path.is_file();

        let mut hits = Vec::new();
        if plan.kind == Kind::Name {
            let result = self.call("files", json!({"hidden": false, "scope": prefix}))?;
            covered(&result)?;
            for file in result["files"].as_array().ok_or("no files in reply")? {
                let file = file.as_str().ok_or("a file that is not a string")?;
                let path = root.join(file);
                if let Some(hit) = plan.file(&path, &find::relative(scope, &path))
                    && keep(&path)
                {
                    hits.push(hit);
                }
            }
            hits.extend(
                dirty
                    .iter()
                    .filter(|path| path.is_file())
                    .filter_map(|path| plan.file(path, &find::relative(scope, path))),
            );
            return Ok(hits);
        }

        let result = self.call(
            "search",
            json!({
                "pattern": plan.pattern,
                "case_insensitive": plan.ignore_case,
                "before_context": plan.context,
                "after_context": plan.context,
                "scope": prefix,
                "hidden": false,
            }),
        )?;
        covered(&result)?;
        // Rows by file: every line the server sent, and which of them matched.
        let mut files: BTreeMap<String, (BTreeMap<u64, String>, Vec<u64>)> = BTreeMap::new();
        for row in result["matches"].as_array().ok_or("no matches in reply")? {
            let kind = row["type"].as_str().unwrap_or("");
            let (Some(file), Some(line), Some(content)) = (
                row["file"].as_str(),
                row["line"].as_u64(),
                row["content"].as_str(),
            ) else {
                continue;
            };
            if kind != "match" && kind != "context" {
                continue;
            }
            let entry = files.entry(file.to_string()).or_default();
            entry
                .0
                .insert(line, content.trim_end_matches(['\n', '\r']).to_string());
            if kind == "match" {
                entry.1.push(line);
            }
        }
        for (file, (lines, matched)) in files {
            let path = root.join(&file);
            if !plan.admits(&find::relative(scope, &path)) || !keep(&path) {
                continue;
            }
            for line in matched {
                let excerpt = lines.get(&line).cloned().unwrap_or_default();
                let run = |step: i64| -> Vec<String> {
                    let mut out = Vec::new();
                    let mut at = line as i64 + step;
                    while out.len() < plan.context && at > 0 {
                        match lines.get(&(at as u64)) {
                            Some(text) => out.push(text.clone()),
                            None => break,
                        }
                        at += step;
                    }
                    out
                };
                let mut before = run(-1);
                before.reverse();
                let mark = match &plan.declare {
                    None => Mark::Match,
                    Some(declare) if declare.is_match(&excerpt) => Mark::Definition,
                    Some(_) => Mark::Reference,
                };
                hits.push(Hit {
                    path: path.clone(),
                    entry: None,
                    place: None,
                    line: Some(line),
                    excerpt,
                    before,
                    after: run(1),
                    mark,
                    score: 0,
                });
            }
        }
        hits.extend(find::grep(plan, &find::fresh(plan, scope, dirty)));
        Ok(hits)
    }
}

/// The server's reply covers hidden-file visibility, as tgrep's own client requires.
fn covered(result: &Value) -> Result<(), String> {
    match result["hidden_complete"].as_bool() {
        Some(true) => Ok(()),
        _ => Err("the server's index is not complete".to_string()),
    }
}
