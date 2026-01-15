use clap::{Parser, Subcommand};
use anyhow::Result;
use std::process::Command;

#[derive(Parser)]
#[command(name = "dev")]
#[command(about = "Hanzo Dev - AI-powered development assistant")]
#[command(version = "0.6.0")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// GitHub Copilot integration
    Copilot {
        #[command(subcommand)]
        action: CopilotCommand,
    },
    /// Check version and installation
    Version,
    /// Setup and install dependencies
    Setup,
}

#[derive(Subcommand)]
enum CopilotCommand {
    /// Interactive chat with GitHub Copilot
    Chat {
        /// Message to send to Copilot
        message: Option<String>,
    },
    /// Check Copilot setup status
    Setup {
        /// Check installation status
        #[arg(long)]
        check: bool,
        /// Install Copilot extension
        #[arg(long)]
        install: bool,
    },
    /// Get code suggestions
    Suggest {
        /// Code or description to get suggestions for
        input: String,
    },
    /// Generate shell commands from description
    Shell {
        /// Task description
        task: String,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Copilot { action }) => {
            handle_copilot_command(action).await
        }
        Some(Commands::Version) => {
            println!("Hanzo Dev v{}", env!("CARGO_PKG_VERSION"));
            println!("AI-powered development assistant with GitHub Copilot integration");
            println!("Repository: https://github.com/hanzoai/dev");
            Ok(())
        }
        Some(Commands::Setup) => {
            println!("Setting up Hanzo Dev...");
            setup_hanzo_dev().await
        }
        None => {
            // Default: show help and check if we can run the full version
            println!("Hanzo Dev v{}", env!("CARGO_PKG_VERSION"));
            println!("AI-powered development assistant");
            println!();
            println!("This is a minimal version. For the full Hanzo Dev experience:");
            println!("  npm install -g @hanzo/dev");
            println!();
            println!("Available commands:");
            println!("  dev copilot chat    - Interactive chat with GitHub Copilot");
            println!("  dev copilot setup   - Setup GitHub Copilot integration");
            println!("  dev version         - Show version information");
            println!();
            Ok(())
        }
    }
}

async fn handle_copilot_command(action: CopilotCommand) -> Result<()> {
    match action {
        CopilotCommand::Chat { message } => {
            if !check_gh_copilot_available() {
                println!("❌ GitHub Copilot is not available.");
                println!("Run 'dev copilot setup --check' for setup instructions.");
                return Ok(());
            }

            let prompt = match message {
                Some(msg) => msg,
                None => {
                    println!("Enter your message for GitHub Copilot:");
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input)?;
                    input.trim().to_string()
                }
            };

            println!("Sending to GitHub Copilot: {}", prompt);
            
            // Use gh copilot CLI
            let output = Command::new("gh")
                .args(["copilot", "chat", &prompt])
                .output();

            match output {
                Ok(output) if output.status.success() => {
                    println!("{}", String::from_utf8_lossy(&output.stdout));
                }
                Ok(output) => {
                    eprintln!("Copilot command failed: {}", String::from_utf8_lossy(&output.stderr));
                }
                Err(e) => {
                    eprintln!("Failed to run gh copilot: {}", e);
                    println!("Make sure GitHub CLI and Copilot extension are installed.");
                }
            }

            Ok(())
        }
        CopilotCommand::Setup { check, install } => {
            if check || (!install) {
                println!("Checking GitHub Copilot setup...");
                check_copilot_setup();
            }
            
            if install {
                println!("Installing GitHub Copilot extension...");
                install_copilot().await?;
            }
            
            Ok(())
        }
        CopilotCommand::Suggest { input } => {
            if !check_gh_copilot_available() {
                println!("❌ GitHub Copilot is not available.");
                return Ok(());
            }

            println!("Getting suggestions for: {}", input);
            
            let output = Command::new("gh")
                .args(["copilot", "suggest", &input])
                .output();

            match output {
                Ok(output) if output.status.success() => {
                    println!("{}", String::from_utf8_lossy(&output.stdout));
                }
                Ok(output) => {
                    eprintln!("Suggestion failed: {}", String::from_utf8_lossy(&output.stderr));
                }
                Err(e) => {
                    eprintln!("Failed to get suggestions: {}", e);
                }
            }

            Ok(())
        }
        CopilotCommand::Shell { task } => {
            if !check_gh_copilot_available() {
                println!("❌ GitHub Copilot is not available.");
                return Ok(());
            }

            println!("Generating shell commands for: {}", task);
            
            let output = Command::new("gh")
                .args(["copilot", "suggest", "--type", "shell", &task])
                .output();

            match output {
                Ok(output) if output.status.success() => {
                    println!("Suggested commands:");
                    println!("{}", String::from_utf8_lossy(&output.stdout));
                }
                Ok(output) => {
                    eprintln!("Command generation failed: {}", String::from_utf8_lossy(&output.stderr));
                }
                Err(e) => {
                    eprintln!("Failed to generate commands: {}", e);
                }
            }

            Ok(())
        }
    }
}

fn check_gh_copilot_available() -> bool {
    // Check if gh CLI is available
    let gh_check = Command::new("gh")
        .args(["--version"])
        .output();
    
    if gh_check.is_err() {
        return false;
    }

    // Check if copilot extension is installed
    let copilot_check = Command::new("gh")
        .args(["extension", "list"])
        .output();

    match copilot_check {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            stdout.contains("github/copilot") || stdout.contains("copilot")
        }
        Err(_) => false,
    }
}

fn check_copilot_setup() {
    println!("Checking GitHub Copilot setup...");
    
    // Check GitHub CLI
    match Command::new("gh").args(["--version"]).output() {
        Ok(output) if output.status.success() => {
            println!("✅ GitHub CLI is installed");
            let version = String::from_utf8_lossy(&output.stdout);
            println!("   {}", version.lines().next().unwrap_or(""));
        }
        _ => {
            println!("❌ GitHub CLI is not installed");
            println!("   Install from: https://cli.github.com/");
            return;
        }
    }

    // Check GitHub CLI authentication
    match Command::new("gh").args(["auth", "status"]).output() {
        Ok(output) if output.status.success() => {
            println!("✅ GitHub CLI is authenticated");
        }
        _ => {
            println!("❌ GitHub CLI is not authenticated");
            println!("   Run: gh auth login");
            return;
        }
    }

    // Check Copilot extension
    match Command::new("gh").args(["extension", "list"]).output() {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if stdout.contains("github/copilot") || stdout.contains("copilot") {
                println!("✅ GitHub Copilot extension is installed");
            } else {
                println!("❌ GitHub Copilot extension is not installed");
                println!("   Install with: gh extension install github/copilot");
            }
        }
        Err(_) => {
            println!("❌ Could not check Copilot extension status");
        }
    }
}

async fn install_copilot() -> Result<()> {
    println!("Installing GitHub Copilot extension...");
    
    let output = Command::new("gh")
        .args(["extension", "install", "github/copilot"])
        .output();

    match output {
        Ok(output) if output.status.success() => {
            println!("✅ GitHub Copilot extension installed successfully!");
        }
        Ok(output) => {
            eprintln!("❌ Installation failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        Err(e) => {
            eprintln!("❌ Failed to install extension: {}", e);
        }
    }

    Ok(())
}

async fn setup_hanzo_dev() -> Result<()> {
    println!("Setting up Hanzo Dev...");
    
    // Check for full installation
    println!("Checking for full Hanzo Dev installation...");
    
    if Command::new("npm").args(["list", "-g", "@hanzo/dev"]).output().is_ok() {
        println!("✅ Full Hanzo Dev is already installed via npm");
        println!("   You can use the full 'dev' command with all features");
    } else {
        println!("ℹ️  This is the minimal Rust version of Hanzo Dev");
        println!("   For the full experience, install via npm:");
        println!("   npm install -g @hanzo/dev");
    }
    
    // Setup GitHub Copilot
    println!("\nSetting up GitHub Copilot integration...");
    check_copilot_setup();
    
    Ok(())
}