//! Deterministic tools over a repository: search, read, apply a patch, run the tests. A flow's
//! steps call them; the loop's context selection uses [`search`].

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
    shell_until(root, script, limit, &|| false)
}

/// [`shell`], also killed as soon as `stop` says so (code -3).
pub fn shell_until(root: &Path, script: &str, limit: Duration, stop: &dyn Fn() -> bool) -> Value {
    let started = Instant::now();
    let mut command = Command::new("sh");
    command
        .arg("-c")
        .arg(format!("exec 2>&1\n{script}"))
        .current_dir(root)
        // An edit is read at once: Python validates a cached module by the source's second and
        // size, so a same-size edit within the second of the last run would run stale code.
        .env("PYTHONDONTWRITEBYTECODE", "1")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    // A cache a test framework keeps in the working directory is the run's own, kept outside the
    // tree and gone with the run, so no run sees another's examples and none lands in the tree.
    let caches = tempfile::tempdir().ok();
    if let Some(dir) = &caches
        && std::env::var_os("HYPOTHESIS_STORAGE_DIRECTORY").is_none()
    {
        command.env("HYPOTHESIS_STORAGE_DIRECTORY", dir.path().join("hypothesis"));
    }
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
            Ok(None) if started.elapsed() > limit || stop() => {
                let timed = started.elapsed() > limit;
                #[cfg(unix)]
                // `--`: without it `kill -9 -PGID` reads the group as an option and kills nothing.
                let _ = Command::new("kill")
                    .args(["-9", "--", &format!("-{}", child.id())])
                    .status();
                let _ = child.kill();
                let _ = child.wait();
                break if timed { -2 } else { -3 };
            }
            Ok(None) => std::thread::sleep(Duration::from_millis(5)),
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
/// The files under a repository that git neither tracks nor ignores, and the untracked
/// directories as git names them (`dir/`): what `git status` would show beside the change.
/// `None` outside a repository.
fn untracked(root: &Path, directories: bool) -> Option<HashSet<String>> {
    let mut git = Command::new("git");
    git.args(["ls-files", "--others", "--exclude-standard", "-z"]);
    if directories {
        git.arg("--directory");
    }
    let out = git.current_dir(root).stderr(Stdio::null()).output().ok().filter(|o| o.status.success())?;
    Some(out.stdout.split(|b| *b == 0).filter(|p| !p.is_empty()).map(|p| String::from_utf8_lossy(p).into_owned()).collect())
}

/// What a repository holds beside its tracked files before a run in it: the run's own files are
/// what it adds to that ([`Litter::sweep`]).
pub struct Litter {
    files: Option<HashSet<String>>,
    entries: HashSet<String>,
}

impl Litter {
    pub fn before(root: &Path) -> Litter {
        Litter { files: untracked(root, false), entries: untracked(root, true).unwrap_or_default() }
    }

    /// Removes the files a run in `root` wrote that were not there before it and that git neither
    /// tracks nor ignores, with the directories they emptied that were not there before either:
    /// they are the run's, never an edit's. What `.gitignore` names is left alone, and so is
    /// anything outside a repository. The paths removed.
    pub fn sweep(&self, root: &Path) -> Vec<String> {
        let (Some(before), Some(now)) = (&self.files, untracked(root, false)) else {
            return Vec::new();
        };
        let mut gone: Vec<String> = now.difference(before).filter(|p| std::fs::remove_file(root.join(p)).is_ok()).cloned().collect();
        gone.sort();
        for p in &gone {
            let mut dir = Path::new(p).parent();
            while let Some(d) = dir.filter(|d| !d.as_os_str().is_empty()) {
                let name = format!("{}/", d.to_string_lossy());
                if self.entries.contains(&name) || std::fs::remove_dir(root.join(d)).is_err() {
                    break;
                }
                dir = d.parent();
            }
        }
        gone
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    fn git(root: &Path, args: &[&str]) {
        let ok = Command::new("git")
            .args(["-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false"])
            .args(args)
            .current_dir(root)
            .output()
            .unwrap()
            .status
            .success();
        assert!(ok, "git {args:?}");
    }

    #[test]
    fn what_a_test_run_writes_beside_the_tree_is_swept_and_nothing_else() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::write(root.join("m.py"), "x = 1\n").unwrap();
        std::fs::write(root.join(".gitignore"), "cache/\n").unwrap();
        git(root, &["init", "-q"]);
        git(root, &["add", "."]);
        git(root, &["commit", "-q", "-m", "start"]);
        // Already there before the run: an untracked file and an empty directory, both kept.
        std::fs::write(root.join("notes.txt"), "mine").unwrap();
        std::fs::create_dir_all(root.join("empty")).unwrap();
        let litter = Litter::before(root);
        let run = shell(root, "mkdir -p .hypothesis/examples/ab cache && echo e > .hypothesis/examples/ab/1 && echo o > out.log && echo c > cache/x && echo 'x = 2' > m.py", Duration::from_secs(10));
        assert_eq!(run["code"], 0);
        let gone = litter.sweep(root);
        assert_eq!(gone, [".hypothesis/examples/ab/1", "out.log"]);
        assert!(!root.join(".hypothesis").exists(), "the directories it emptied go too");
        assert!(root.join("notes.txt").exists() && root.join("empty").is_dir(), "what was there before stays");
        assert!(root.join("cache/x").exists(), "what .gitignore names is left alone");
        assert_eq!(std::fs::read_to_string(root.join("m.py")).unwrap(), "x = 2\n", "a tracked file is never touched");
        // Outside a repository nothing is swept.
        let plain = tempfile::tempdir().unwrap();
        let litter = Litter::before(plain.path());
        std::fs::write(plain.path().join("new.txt"), "n").unwrap();
        assert!(litter.sweep(plain.path()).is_empty());
    }

    #[test]
    fn a_runs_test_framework_cache_is_its_own_and_outside_the_tree() {
        let dir = tempfile::tempdir().unwrap();
        let run = shell(dir.path(), "echo \"$HYPOTHESIS_STORAGE_DIRECTORY\"", Duration::from_secs(10));
        let at = run["output"].as_str().unwrap().trim().to_string();
        if std::env::var_os("HYPOTHESIS_STORAGE_DIRECTORY").is_none() {
            assert!(!at.is_empty() && !Path::new(&at).starts_with(dir.path()), "{at}");
            assert!(!Path::new(&at).parent().unwrap().exists(), "gone with the run: {at}");
        }
    }

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
    fn a_slow_command_is_killed_with_everything_it_started() {
        let dir = tempfile::tempdir().unwrap();
        let began = Instant::now();
        let run = shell(dir.path(), "sleep 8 & sleep 8; wait", Duration::from_millis(200));
        assert_eq!(run["code"], -2);
        assert!(began.elapsed() < Duration::from_secs(3), "{:?}: the children held the pipe", began.elapsed());
    }

    #[test]
    fn a_command_stopped_from_outside_ends_at_once() {
        let dir = tempfile::tempdir().unwrap();
        let began = Instant::now();
        let stop = || began.elapsed() > Duration::from_millis(100);
        let run = shell_until(dir.path(), "sleep 8 & sleep 8; wait", Duration::from_secs(60), &stop);
        assert_eq!(run["code"], -3);
        assert!(began.elapsed() < Duration::from_secs(3), "{:?}", began.elapsed());
    }
}
