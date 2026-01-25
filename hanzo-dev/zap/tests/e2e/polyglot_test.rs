//! Polyglot E2E Test Suite for ZAP
//!
//! Validates that agents implemented in different languages all produce
//! consistent results when executing the same tools via ZAP protocol.
//!
//! Languages tested:
//! - Rust (native)
//! - Python (hanzo-tools via subprocess)
//! - Node.js (subprocess)
//! - Go (subprocess)
//! - Ruby (subprocess)
//! - Elixir (subprocess)

use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;

/// Result from a language-specific agent
#[derive(Debug)]
struct AgentResult {
    language: String,
    tool: String,
    success: bool,
    output: String,
    duration_ms: u64,
}

/// Run the Rust agent directly
fn run_rust_agent(tool: &str, args: &str) -> AgentResult {
    let start = std::time::Instant::now();
    // Would call ZAP dispatcher directly
    let success = true;
    let output = format!("Rust: {}({})", tool, args);

    AgentResult {
        language: "rust".to_string(),
        tool: tool.to_string(),
        success,
        output,
        duration_ms: start.elapsed().as_millis() as u64,
    }
}

/// Run an agent in a subprocess
fn run_subprocess_agent(
    language: &str,
    script_path: &PathBuf,
    tool: &str,
    args: &str,
) -> AgentResult {
    let start = std::time::Instant::now();

    let (cmd, cmd_args): (&str, Vec<&str>) = match language {
        "python" => ("python3", vec![script_path.to_str().unwrap(), tool, args]),
        "node" => ("npx", vec!["ts-node", script_path.to_str().unwrap(), tool, args]),
        "go" => ("go", vec!["run", script_path.to_str().unwrap(), tool, args]),
        "ruby" => ("ruby", vec![script_path.to_str().unwrap(), tool, args]),
        "elixir" => ("elixir", vec![script_path.to_str().unwrap(), tool, args]),
        _ => panic!("Unknown language: {}", language),
    };

    let result = Command::new(cmd)
        .args(&cmd_args)
        .output();

    match result {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();

            AgentResult {
                language: language.to_string(),
                tool: tool.to_string(),
                success: output.status.success(),
                output: if output.status.success() { stdout } else { stderr },
                duration_ms: start.elapsed().as_millis() as u64,
            }
        }
        Err(e) => AgentResult {
            language: language.to_string(),
            tool: tool.to_string(),
            success: false,
            output: e.to_string(),
            duration_ms: start.elapsed().as_millis() as u64,
        },
    }
}

/// Test specification
struct TestSpec {
    tool: &'static str,
    args: &'static str,
    expected_contains: &'static str,
}

/// Run all polyglot tests
fn run_polyglot_tests() -> Vec<(TestSpec, HashMap<String, AgentResult>)> {
    let specs = vec![
        TestSpec {
            tool: "read_file",
            args: "Cargo.toml",
            expected_contains: "[package]",
        },
        TestSpec {
            tool: "list_dir",
            args: ".",
            expected_contains: "name",
        },
        TestSpec {
            tool: "git_status",
            args: "",
            expected_contains: "branch",
        },
    ];

    let examples_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples");

    let mut results = Vec::new();

    for spec in specs {
        let mut lang_results = HashMap::new();

        // Rust (native)
        lang_results.insert(
            "rust".to_string(),
            run_rust_agent(spec.tool, spec.args),
        );

        // Python
        let python_path = examples_dir.join("python/agent.py");
        if python_path.exists() {
            lang_results.insert(
                "python".to_string(),
                run_subprocess_agent("python", &python_path, spec.tool, spec.args),
            );
        }

        // Node.js
        let node_path = examples_dir.join("node/agent.ts");
        if node_path.exists() {
            lang_results.insert(
                "node".to_string(),
                run_subprocess_agent("node", &node_path, spec.tool, spec.args),
            );
        }

        // Go
        let go_path = examples_dir.join("go/agent.go");
        if go_path.exists() {
            lang_results.insert(
                "go".to_string(),
                run_subprocess_agent("go", &go_path, spec.tool, spec.args),
            );
        }

        // Ruby
        let ruby_path = examples_dir.join("ruby/agent.rb");
        if ruby_path.exists() {
            lang_results.insert(
                "ruby".to_string(),
                run_subprocess_agent("ruby", &ruby_path, spec.tool, spec.args),
            );
        }

        // Elixir
        let elixir_path = examples_dir.join("elixir/agent.exs");
        if elixir_path.exists() {
            lang_results.insert(
                "elixir".to_string(),
                run_subprocess_agent("elixir", &elixir_path, spec.tool, spec.args),
            );
        }

        results.push((spec, lang_results));
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore] // Run with: cargo test --test polyglot -- --ignored
    fn test_polyglot_consistency() {
        let results = run_polyglot_tests();

        for (spec, lang_results) in results {
            println!("\nTool: {} (args: {})", spec.tool, spec.args);
            println!("Expected to contain: {}", spec.expected_contains);
            println!("-".repeat(60));

            for (lang, result) in &lang_results {
                let status = if result.success { "✓" } else { "✗" };
                println!(
                    "  {} {} ({}ms): {}",
                    status,
                    lang,
                    result.duration_ms,
                    if result.success {
                        &result.output[..result.output.len().min(50)]
                    } else {
                        &result.output
                    }
                );

                if result.success {
                    assert!(
                        result.output.contains(spec.expected_contains),
                        "{} output should contain '{}'",
                        lang,
                        spec.expected_contains
                    );
                }
            }
        }
    }
}

fn main() {
    println!("ZAP Polyglot E2E Test Runner");
    println!("============================\n");

    let results = run_polyglot_tests();

    let mut total = 0;
    let mut passed = 0;

    for (spec, lang_results) in results {
        println!("\n\x1b[1mTool: {}\x1b[0m (args: {})", spec.tool, spec.args);
        println!("Expected: contains '{}'", spec.expected_contains);
        println!("{}", "-".repeat(60));

        for (lang, result) in &lang_results {
            total += 1;
            let (status, color) = if result.success {
                passed += 1;
                ("PASS", "\x1b[32m")
            } else {
                ("FAIL", "\x1b[31m")
            };

            println!(
                "  {}[{}]\x1b[0m {:8} {:>6}ms",
                color, status, lang, result.duration_ms
            );
        }
    }

    println!("\n{}", "=".repeat(60));
    println!(
        "Total: {}/{} passed ({:.1}%)",
        passed,
        total,
        (passed as f64 / total as f64) * 100.0
    );
}
