//! GitHub Copilot CLI commands for Hanzo Dev
//!
//! This module provides command-line interface for GitHub Copilot integration,
//! allowing users to interact with Copilot directly from the dev CLI.

use clap::Parser;
use clap::Subcommand;
use hanzo_core::config::Config;
use hanzo_core::copilot_integration::CopilotConfig;
use hanzo_core::copilot_integration::CopilotIntegration;
use hanzo_core::error::CodexErr;
use hanzo_core::error::Result;
use std::collections::HashMap;
use std::io::Read;
use std::io::Write;
use std::io::{self};
use std::path::Path;

#[derive(Parser, Debug)]
#[command(name = "copilot")]
#[command(about = "GitHub Copilot integration for code assistance")]
pub struct CopilotArgs {
    #[command(subcommand)]
    pub command: CopilotCommand,
}

#[derive(Subcommand, Debug)]
pub enum CopilotCommand {
    /// Interactive chat with GitHub Copilot
    Chat {
        /// Initial message or prompt
        message: Option<String>,
        /// File to provide as context
        #[arg(short, long)]
        file: Option<String>,
        /// Continue previous conversation
        #[arg(short, long)]
        continue_session: bool,
    },
    /// Get code suggestions
    Suggest {
        /// Code context or partial code
        context: Option<String>,
        /// Programming language
        #[arg(short, long)]
        language: Option<String>,
        /// Read context from file
        #[arg(short, long)]
        file: Option<String>,
        /// Number of suggestions to generate
        #[arg(short, long, default_value = "3")]
        count: usize,
    },
    /// Review code and provide suggestions
    Review {
        /// File to review
        file: String,
        /// Review specific diff (Git format)
        #[arg(short, long)]
        diff: bool,
        /// Output format: text, json, markdown
        #[arg(long, default_value = "text")]
        format: String,
    },
    /// Generate documentation
    Docs {
        /// File or code to document
        input: String,
        /// Documentation type: comment, readme, api
        #[arg(short, long, default_value = "comment")]
        doc_type: String,
        /// Output file (if not specified, prints to stdout)
        #[arg(short, long)]
        output: Option<String>,
    },
    /// Explain code functionality
    Explain {
        /// Code to explain (use - for stdin)
        code: String,
        /// File containing code to explain
        #[arg(short, long)]
        file: Option<String>,
        /// Explanation level: basic, detailed, expert
        #[arg(short, long, default_value = "detailed")]
        level: String,
    },
    /// Generate shell commands for tasks
    Shell {
        /// Task description
        task: String,
        /// Operating system context
        #[arg(long, default_value = "auto")]
        os: String,
        /// Show explanation for commands
        #[arg(short, long)]
        explain: bool,
    },
    /// Generate Git commit messages
    Commit {
        /// Use staged changes
        #[arg(short, long)]
        staged: bool,
        /// Specific diff to analyze
        #[arg(short, long)]
        diff: Option<String>,
        /// Commit message style: conventional, simple, detailed
        #[arg(long, default_value = "conventional")]
        style: String,
    },
    /// Setup and configure Copilot
    Setup {
        /// Install Copilot extension
        #[arg(long)]
        install: bool,
        /// Configure authentication
        #[arg(long)]
        auth: bool,
        /// Check installation status
        #[arg(long)]
        check: bool,
    },
    /// Auto-complete code at cursor
    Complete {
        /// File containing code
        file: String,
        /// Line number (1-based)
        #[arg(short, long)]
        line: usize,
        /// Column number (1-based)
        #[arg(short, long)]
        column: usize,
        /// Context window size
        #[arg(long, default_value = "10")]
        context: usize,
    },
}

/// Execute Copilot CLI commands
pub async fn execute_copilot_command(args: CopilotArgs, config: &Config) -> Result<()> {
    let copilot_config = CopilotConfig::from(config);
    let copilot = CopilotIntegration::new(copilot_config)?;

    match args.command {
        CopilotCommand::Chat {
            message,
            file,
            continue_session,
        } => handle_chat_command(&copilot, message, file, continue_session).await,
        CopilotCommand::Suggest {
            context,
            language,
            file,
            count,
        } => handle_suggest_command(&copilot, context, language, file, count).await,
        CopilotCommand::Review { file, diff, format } => {
            handle_review_command(&copilot, file, diff, format).await
        }
        CopilotCommand::Docs {
            input,
            doc_type,
            output,
        } => handle_docs_command(&copilot, input, doc_type, output).await,
        CopilotCommand::Explain { code, file, level } => {
            handle_explain_command(&copilot, code, file, level).await
        }
        CopilotCommand::Shell { task, os, explain } => {
            handle_shell_command(&copilot, task, os, explain).await
        }
        CopilotCommand::Commit {
            staged,
            diff,
            style,
        } => handle_commit_command(&copilot, staged, diff, style).await,
        CopilotCommand::Setup {
            install,
            auth,
            check,
        } => handle_setup_command(&copilot, install, auth, check).await,
        CopilotCommand::Complete {
            file,
            line,
            column,
            context,
        } => handle_complete_command(&copilot, file, line, column, context).await,
    }
}

async fn handle_chat_command(
    copilot: &CopilotIntegration,
    message: Option<String>,
    file: Option<String>,
    continue_session: bool,
) -> Result<()> {
    if !copilot.check_availability().await {
        eprintln!("GitHub Copilot is not available. Run 'dev copilot setup --check' for details.");
        return Ok(());
    }

    // Store the original message state for later check
    let is_interactive = message.is_none();

    let prompt = match message {
        Some(msg) => msg,
        None => {
            print!("Enter your message for Copilot: ");
            io::stdout().flush()?;
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            input.trim().to_string()
        }
    };

    let mut context = HashMap::new();
    if let Some(file_path) = file {
        context.insert("file_path".to_string(), file_path);
    }

    let response = copilot.start_chat_session(&prompt, Some(context)).await?;
    println!("Copilot: {}", response);

    // Interactive mode if no message was provided
    if is_interactive || continue_session {
        loop {
            print!("\nYour message (or 'exit' to quit): ");
            io::stdout().flush()?;
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            let input = input.trim();

            if input == "exit" || input.is_empty() {
                break;
            }

            let response = copilot.start_chat_session(input, None).await?;
            println!("Copilot: {}", response);
        }
    }

    Ok(())
}

async fn handle_suggest_command(
    copilot: &CopilotIntegration,
    context: Option<String>,
    language: Option<String>,
    file: Option<String>,
    count: usize,
) -> Result<()> {
    if !copilot.check_availability().await {
        eprintln!("GitHub Copilot is not available. Run 'dev copilot setup --check' for details.");
        return Ok(());
    }

    let context_text = match (context, file) {
        (Some(ctx), None) => ctx,
        (None, Some(file_path)) => match std::fs::read_to_string(&file_path) {
            Ok(content) => content,
            Err(_e) => {
                return Err(CodexErr::UnsupportedOperation(format!(
                    "Failed to read file {}",
                    file_path
                )));
            }
        },
        (Some(ctx), Some(_)) => {
            eprintln!("Warning: Both context and file provided. Using context.");
            ctx
        }
        (None, None) => {
            print!("Enter code context: ");
            io::stdout().flush()?;
            let mut input = String::new();
            io::stdin().read_to_string(&mut input)?;
            input
        }
    };

    let suggestions = copilot
        .get_code_suggestions(&context_text, language.as_deref(), None)
        .await?;

    if suggestions.is_empty() {
        println!("No suggestions available.");
        return Ok(());
    }

    for (i, suggestion) in suggestions.iter().take(count).enumerate() {
        println!("--- Suggestion {} ---", i + 1);
        println!("{}", suggestion.text);
        if let Some(reasoning) = &suggestion.reasoning {
            println!("Reasoning: {}", reasoning);
        }
        println!();
    }

    Ok(())
}

async fn handle_review_command(
    copilot: &CopilotIntegration,
    file: String,
    diff: bool,
    format: String,
) -> Result<()> {
    if !copilot.check_availability().await {
        eprintln!("GitHub Copilot is not available. Run 'dev copilot setup --check' for details.");
        return Ok(());
    }

    let diff_content = if diff {
        // Get git diff for the file
        let output = std::process::Command::new("git")
            .args(["diff", &file])
            .output();

        let output = match output {
            Ok(output) => output,
            Err(_e) => {
                return Err(CodexErr::UnsupportedOperation(
                    "Failed to get git diff".to_string(),
                ));
            }
        };

        if !output.status.success() {
            return Err(CodexErr::UnsupportedOperation(format!(
                "Git diff failed: {}",
                String::from_utf8_lossy(&output.stderr)
            )));
        }

        Some(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        None
    };

    let review = copilot.review_code(&file, diff_content.as_deref()).await?;

    match format.as_str() {
        "json" => {
            let json = serde_json::json!({
                "file": file,
                "review": review,
                "has_diff": diff_content.is_some()
            });
            println!("{}", serde_json::to_string_pretty(&json)?);
        }
        "markdown" => {
            println!("# Code Review: {}\n", file);
            println!("{}", review);
        }
        _ => {
            println!("Code Review for {}:", file);
            println!("{}", review);
        }
    }

    Ok(())
}

async fn handle_docs_command(
    copilot: &CopilotIntegration,
    input: String,
    doc_type: String,
    output: Option<String>,
) -> Result<()> {
    if !copilot.check_availability().await {
        eprintln!("GitHub Copilot is not available. Run 'dev copilot setup --check' for details.");
        return Ok(());
    }

    let code = if Path::new(&input).exists() {
        match std::fs::read_to_string(&input) {
            Ok(content) => content,
            Err(_e) => {
                return Err(CodexErr::UnsupportedOperation(format!(
                    "Failed to read file {}",
                    input
                )));
            }
        }
    } else {
        input
    };

    let documentation = copilot.generate_documentation(&code, &doc_type).await?;

    match output {
        Some(output_file) => match std::fs::write(&output_file, &documentation) {
            Ok(_) => println!("Documentation written to {}", output_file),
            Err(_e) => {
                return Err(CodexErr::UnsupportedOperation(format!(
                    "Failed to write to {}",
                    output_file
                )));
            }
        },
        None => {
            println!("{}", documentation);
        }
    }

    Ok(())
}

async fn handle_explain_command(
    copilot: &CopilotIntegration,
    code: String,
    file: Option<String>,
    level: String,
) -> Result<()> {
    if !copilot.check_availability().await {
        eprintln!("GitHub Copilot is not available. Run 'dev copilot setup --check' for details.");
        return Ok(());
    }

    let code_content = match file {
        Some(file_path) => match std::fs::read_to_string(&file_path) {
            Ok(content) => content,
            Err(_e) => {
                return Err(CodexErr::UnsupportedOperation(format!(
                    "Failed to read file {}",
                    file_path
                )));
            }
        },
        None => {
            if code == "-" {
                let mut input = String::new();
                io::stdin().read_to_string(&mut input)?;
                input
            } else {
                code
            }
        }
    };

    let explanation = copilot.explain_code(&code_content).await?;

    println!("Code Explanation ({} level):", level);
    println!("{}", explanation);

    Ok(())
}

async fn handle_shell_command(
    copilot: &CopilotIntegration,
    task: String,
    _os: String,
    explain: bool,
) -> Result<()> {
    if !copilot.check_availability().await {
        eprintln!("GitHub Copilot is not available. Run 'dev copilot setup --check' for details.");
        return Ok(());
    }

    let commands = copilot.suggest_commands(&task).await?;

    if commands.is_empty() {
        println!("No commands suggested for task: {}", task);
        return Ok(());
    }

    println!("Suggested commands for: {}", task);
    println!();

    for (i, command) in commands.iter().enumerate() {
        println!("{}. {}", i + 1, command);

        if explain {
            // Get explanation for the command
            let explanation = copilot
                .explain_code(&format!("Shell command: {}", command))
                .await?;
            println!("   → {}", explanation);
        }
        println!();
    }

    Ok(())
}

async fn handle_commit_command(
    copilot: &CopilotIntegration,
    staged: bool,
    diff: Option<String>,
    style: String,
) -> Result<()> {
    if !copilot.check_availability().await {
        eprintln!("GitHub Copilot is not available. Run 'dev copilot setup --check' for details.");
        return Ok(());
    }

    let diff_content = match diff {
        Some(d) => d,
        None => {
            let git_args = if staged {
                vec!["diff", "--cached"]
            } else {
                vec!["diff"]
            };

            let output = std::process::Command::new("git").args(&git_args).output();

            let output = match output {
                Ok(output) => output,
                Err(_e) => {
                    return Err(CodexErr::UnsupportedOperation(
                        "Failed to get git diff".to_string(),
                    ));
                }
            };

            if !output.status.success() {
                return Err(CodexErr::UnsupportedOperation(format!(
                    "Git diff failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                )));
            }

            String::from_utf8_lossy(&output.stdout).to_string()
        }
    };

    if diff_content.trim().is_empty() {
        println!("No changes to commit.");
        return Ok(());
    }

    let commit_message = copilot.generate_commit_message(&diff_content).await?;

    println!(
        "Suggested commit message ({}style):",
        if style == "conventional" {
            "conventional "
        } else {
            ""
        }
    );
    println!("{}", commit_message);

    Ok(())
}

async fn handle_setup_command(
    copilot: &CopilotIntegration,
    install: bool,
    auth: bool,
    check: bool,
) -> Result<()> {
    if check || (!install && !auth) {
        println!("Checking GitHub Copilot setup...");

        if copilot.check_gh_cli().await {
            println!("✓ GitHub CLI is installed and authenticated");
        } else {
            println!("✗ GitHub CLI is not available or not authenticated");
            println!("  Install: https://cli.github.com/");
            println!("  Authenticate: gh auth login");
        }

        if copilot.check_copilot_extension().await {
            println!("✓ GitHub Copilot extension is installed");
        } else {
            println!("✗ GitHub Copilot extension is not installed");
            println!("  Install: gh extension install github/copilot");
        }

        if copilot.check_availability().await {
            println!("✓ GitHub Copilot is ready to use");
        } else {
            println!("✗ GitHub Copilot is not available");
        }

        return Ok(());
    }

    if install {
        println!("Installing GitHub Copilot extension...");
        copilot.install_copilot_extension().await?;
        println!("✓ GitHub Copilot extension installed");
    }

    if auth {
        println!("Configuring GitHub Copilot authentication...");
        copilot.configure_copilot().await?;
        println!("✓ GitHub Copilot authenticated");
    }

    Ok(())
}

async fn handle_complete_command(
    copilot: &CopilotIntegration,
    file: String,
    line: usize,
    column: usize,
    context: usize,
) -> Result<()> {
    if !copilot.check_availability().await {
        eprintln!("GitHub Copilot is not available. Run 'dev copilot setup --check' for details.");
        return Ok(());
    }

    let content = match std::fs::read_to_string(&file) {
        Ok(content) => content,
        Err(_e) => {
            return Err(CodexErr::UnsupportedOperation(format!(
                "Failed to read file {}",
                file
            )));
        }
    };

    let lines: Vec<&str> = content.lines().collect();

    if line == 0 || line > lines.len() {
        return Err(CodexErr::UnsupportedOperation(format!(
            "Invalid line number: {} (file has {} lines)",
            line,
            lines.len()
        )));
    }

    let line_content = lines[line - 1];
    if column > line_content.len() {
        return Err(CodexErr::UnsupportedOperation(format!(
            "Invalid column: {} (line has {} characters)",
            column,
            line_content.len()
        )));
    }

    let prefix = &line_content[..column - 1];

    // Get surrounding context
    let start_line = line.saturating_sub(context);
    let end_line = std::cmp::min(lines.len(), line + context);
    let context_lines: Vec<String> = lines[start_line..end_line]
        .iter()
        .map(|s| s.to_string())
        .collect();

    let file_ext = Path::new(&file)
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_string());

    let completions = copilot
        .auto_complete(prefix, file_ext.as_deref(), context_lines)
        .await?;

    if completions.is_empty() {
        println!("No completions available.");
        return Ok(());
    }

    println!("Completions for position {}:{} in {}:", line, column, file);
    for (i, completion) in completions.iter().enumerate() {
        println!("{}. {}", i + 1, completion);
    }

    Ok(())
}
