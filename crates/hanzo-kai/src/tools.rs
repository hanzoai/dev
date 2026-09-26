//! Deterministic tools over a repository: search, read, apply a patch, run the tests, git.
//! A flow's solver nodes call them; the loop's context selection uses [`search`].

use serde_json::Value;
use serde_json::json;
use std::collections::HashSet;
use std::io::Read;
use std::path::Path;
use std::process::Command;
use std::process::Stdio;
use std::time::Duration;
use std::time::Instant;

/// How much of a file a search reads.
pub const HEAD: usize = 64 * 1024;
/// How many files a search scores.
const FILES: usize = 20_000;
/// How much of a tool's output is kept: its tail, where failures print.
pub const TAIL: usize = 6000;

/// A file a search found, and how well it matched.
#[derive(Debug, Clone, PartialEq)]
pub struct Hit {
    pub path: String,
    pub score: f64,
}

/// Lowercase words of three or more characters: letters, digits and underscores.
pub fn words(text: &str) -> HashSet<String> {
    text.split(|c: char| !(c.is_alphanumeric() || c == '_'))
        .filter(|w| w.chars().count() >= 3)
        .map(str::to_lowercase)
        .collect()
}

/// The repository's files, as git lists them; every file under `root` when it is not a
/// repository.
pub fn files(root: &Path) -> Vec<String> {
    let listed = Command::new("git")
        .args([
            "ls-files",
            "--cached",
            "--others",
            "--exclude-standard",
            "-z",
        ])
        .current_dir(root)
        .stderr(Stdio::null())
        .output();
    if let Ok(out) = listed
        && out.status.success()
    {
        return out
            .stdout
            .split(|b| *b == 0)
            .filter(|p| !p.is_empty())
            .map(|p| String::from_utf8_lossy(p).into_owned())
            .take(FILES)
            .collect();
    }
    let mut out = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let name = entry.file_name();
            if name.to_string_lossy().starts_with('.') || name == "target" || name == "node_modules"
            {
                continue;
            }
            if path.is_dir() {
                stack.push(path);
            } else if let Ok(rel) = path.strip_prefix(root) {
                out.push(rel.to_string_lossy().into_owned());
                if out.len() >= FILES {
                    return out;
                }
            }
        }
    }
    out.sort();
    out
}

/// The first `n` bytes of `path` under `root` as text; `None` for a binary or unreadable file.
pub fn head(root: &Path, path: &str, n: usize) -> Option<String> {
    let mut buf = Vec::with_capacity(n.min(1 << 16));
    std::fs::File::open(root.join(path))
        .ok()?
        .take(n as u64)
        .read_to_end(&mut buf)
        .ok()?;
    if buf.contains(&0) {
        return None;
    }
    Some(String::from_utf8_lossy(&buf).into_owned())
}

/// Files under `root` ranked by the words they share with `query`: a word in the path counts
/// three, one in the first `read` bytes counts one. At most `top`, best first; none that
/// share nothing.
pub fn search(root: &Path, query: &str, top: usize, read: usize) -> Vec<Hit> {
    let wanted = words(query);
    if wanted.is_empty() {
        return Vec::new();
    }
    let mut hits: Vec<Hit> = files(root)
        .into_iter()
        .filter_map(|path| {
            let in_path = words(&path).intersection(&wanted).count() as f64;
            let in_text = if read == 0 {
                0.0
            } else {
                let text = head(root, &path, read)?;
                words(&text).intersection(&wanted).count() as f64
            };
            let score = 3.0 * in_path + in_text;
            (score > 0.0).then_some(Hit { path, score })
        })
        .collect();
    hits.sort_by(|a, b| {
        b.score
            .total_cmp(&a.score)
            .then(a.path.len().cmp(&b.path.len()))
            .then(a.path.cmp(&b.path))
    });
    hits.truncate(top);
    hits
}

/// The last `n` characters of `text`.
pub fn tail(text: &str, n: usize) -> String {
    let count = text.chars().count();
    if count <= n {
        return text.to_string();
    }
    text.chars().skip(count - n).collect()
}

/// Runs `sh -c script` in `root`, killing it after `limit`: `{code, output, ms}`, output
/// being the tail of stdout and stderr together.
pub fn shell(root: &Path, script: &str, limit: Duration) -> Value {
    let started = Instant::now();
    let mut command = Command::new("sh");
    command
        .arg("-c")
        .arg(format!("exec 2>&1\n{script}"))
        .current_dir(root)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    // Its own process group, so a timeout stops everything the script started.
    #[cfg(unix)]
    std::os::unix::process::CommandExt::process_group(&mut command, 0);
    let child = command.spawn();
    let mut child = match child {
        Ok(c) => c,
        Err(e) => return json!({"code": -1, "output": e.to_string(), "ms": 0}),
    };
    let Some(mut stdout) = child.stdout.take() else {
        let _ = child.kill();
        return json!({"code": -1, "output": "no output pipe", "ms": 0});
    };
    let reader = std::thread::spawn(move || {
        let mut out = Vec::new();
        let _ = stdout.read_to_end(&mut out);
        out
    });
    let code = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status.code().unwrap_or(-1),
            Ok(None) if started.elapsed() > limit => {
                #[cfg(unix)]
                let _ = Command::new("kill")
                    .args(["-9", &format!("-{}", child.id())])
                    .status();
                let _ = child.kill();
                let _ = child.wait();
                break -2;
            }
            Ok(None) => std::thread::sleep(Duration::from_millis(20)),
            Err(_) => break -1,
        }
    };
    let out = reader.join().unwrap_or_default();
    let mut output = tail(&String::from_utf8_lossy(&out), TAIL);
    if code == -2 {
        output.push_str(&format!("\n[killed after {}s]", limit.as_secs()));
    }
    json!({"code": code, "output": output, "ms": started.elapsed().as_millis() as u64})
}

/// Runs the tests: `{passed, code, output, ms}`.
pub fn test(root: &Path, command: &str, limit: Duration) -> Value {
    let mut run = shell(root, command, limit);
    let passed = run["code"] == 0;
    run["passed"] = Value::Bool(passed);
    run
}

/// The unified diff inside a model's answer: a fenced block when there is one, else the
/// text from the first `diff --git` or `--- ` line.
pub fn diff_of(answer: &str) -> String {
    let mut fenced = String::new();
    let mut inside = false;
    for line in answer.lines() {
        if line.trim_start().starts_with("```") {
            if inside {
                break;
            }
            inside = true;
            continue;
        }
        if inside {
            fenced.push_str(line);
            fenced.push('\n');
        }
    }
    let text = if fenced.trim().is_empty() {
        answer
    } else {
        fenced.as_str()
    };
    let start = text
        .find("diff --git")
        .or_else(|| text.find("--- "))
        .unwrap_or(0);
    let mut diff = text[start..].to_string();
    if !diff.ends_with('\n') {
        diff.push('\n');
    }
    diff
}

/// Applies a model's patch with `git apply`: `{applied, files, error}`.
pub fn patch(root: &Path, answer: &str) -> Value {
    let diff = diff_of(answer);
    let files: Vec<String> = diff
        .lines()
        .filter_map(|l| l.strip_prefix("+++ "))
        .map(|p| p.trim().trim_start_matches("b/").to_string())
        .filter(|p| p != "/dev/null")
        .collect();
    if files.is_empty() {
        return json!({"applied": false, "files": [], "error": "no diff in the answer"});
    }
    let mut child = match Command::new("git")
        .args(["apply", "--recount", "--whitespace=nowarn", "-"])
        .current_dir(root)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => return json!({"applied": false, "files": files, "error": e.to_string()}),
    };
    if let Some(mut stdin) = child.stdin.take() {
        use std::io::Write;
        let _ = stdin.write_all(diff.as_bytes());
    }
    match child.wait_with_output() {
        Ok(out) if out.status.success() => json!({"applied": true, "files": files}),
        Ok(out) => json!({
            "applied": false,
            "files": files,
            "error": tail(&String::from_utf8_lossy(&out.stderr), 2000),
        }),
        Err(e) => json!({"applied": false, "files": files, "error": e.to_string()}),
    }
}

/// Undoes a patch [`patch`] applied from `answer`: `git apply -R`.
pub fn revert(root: &Path, answer: &str) -> bool {
    let diff = diff_of(answer);
    let Ok(mut child) = Command::new("git")
        .args(["apply", "-R", "--recount", "--whitespace=nowarn", "-"])
        .current_dir(root)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
    else {
        return false;
    };
    if let Some(mut stdin) = child.stdin.take() {
        use std::io::Write;
        let _ = stdin.write_all(diff.as_bytes());
    }
    child.wait().is_ok_and(|s| s.success())
}

/// Git commands a flow may run: those that only read.
const GIT: &[&str] = &["diff", "status", "log", "show", "rev-parse", "ls-files"];

/// Runs `git args` in `root` when the subcommand only reads: `{code, output}`.
pub fn git(root: &Path, args: &[String]) -> Result<Value, String> {
    let sub = args.first().map(String::as_str).unwrap_or_default();
    if !GIT.contains(&sub) {
        return Err(format!("git {sub}: a flow runs only {}", GIT.join(", ")));
    }
    let out = Command::new("git")
        .args(args)
        .current_dir(root)
        .stdin(Stdio::null())
        .output()
        .map_err(|e| e.to_string())?;
    let mut text = String::from_utf8_lossy(&out.stdout).into_owned();
    text.push_str(&String::from_utf8_lossy(&out.stderr));
    Ok(json!({"code": out.status.code().unwrap_or(-1), "output": tail(&text, TAIL)}))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_fenced_diff_is_taken_from_the_answer() {
        let answer =
            "Here is the fix:\n```diff\n--- a/x.txt\n+++ b/x.txt\n@@ -1 +1 @@\n-1\n+2\n```\nDone.";
        assert_eq!(
            diff_of(answer),
            "--- a/x.txt\n+++ b/x.txt\n@@ -1 +1 @@\n-1\n+2\n"
        );
    }

    #[test]
    fn search_ranks_a_path_named_in_the_query_first() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("src")).unwrap();
        std::fs::write(dir.path().join("src/math.rs"), "pub fn add() {}").unwrap();
        std::fs::write(dir.path().join("src/text.rs"), "pub fn words() {}").unwrap();
        std::fs::write(dir.path().join("README.md"), "math notes").unwrap();
        let hits = search(dir.path(), "panicked at src/math.rs:5: add failed", 8, HEAD);
        assert_eq!(hits[0].path, "src/math.rs");
        assert!(
            hits.iter()
                .all(|h| h.path != "src/text.rs" || h.score > 0.0)
        );
    }

    #[test]
    fn git_runs_only_what_reads() {
        let dir = tempfile::tempdir().unwrap();
        assert!(git(dir.path(), &["push".to_string()]).is_err());
        assert!(git(dir.path(), &["status".to_string()]).is_ok());
    }

    #[test]
    fn a_slow_command_is_killed() {
        let dir = tempfile::tempdir().unwrap();
        let run = shell(dir.path(), "sleep 5", Duration::from_millis(200));
        assert_eq!(run["code"], -2);
    }
}
