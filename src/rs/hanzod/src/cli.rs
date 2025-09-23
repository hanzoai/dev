//! CLI interface for hanzod - hanzo run command

use anyhow::{Result, anyhow};
use clap::{Parser, Subcommand};
use std::process::Command;

#[derive(Parser)]
#[command(name = "hanzo")]
#[command(about = "Hanzo container runtime manager", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Run a container or compose stack
    Run {
        /// Image or compose directory to run
        image: String,

        /// Command to execute in container
        #[arg(trailing_var_arg = true)]
        command: Vec<String>,

        /// Run in detached mode
        #[arg(short, long)]
        detach: bool,

        /// Port mappings (e.g., 8080:80)
        #[arg(short, long)]
        port: Vec<String>,

        /// Environment variables
        #[arg(short, long)]
        env: Vec<String>,

        /// Volume mounts
        #[arg(short, long)]
        volume: Vec<String>,
    },

    /// List running workloads
    Ps {
        /// Show all containers (including stopped)
        #[arg(short, long)]
        all: bool,
    },

    /// Stop a running workload
    Stop {
        /// Container or workload ID
        id: String,
    },

    /// Show logs from a workload
    Logs {
        /// Container or workload ID
        id: String,

        /// Follow log output
        #[arg(short, long)]
        follow: bool,
    },

    /// Execute command in running container
    Exec {
        /// Container ID
        id: String,

        /// Command to execute
        #[arg(trailing_var_arg = true)]
        command: Vec<String>,
    },

    /// Pull an OCI image
    Pull {
        /// Image to pull
        image: String,
    },

    /// Show available runtimes
    Runtimes,

    /// Start the daemon
    Daemon {
        /// Port to listen on
        #[arg(short, long, default_value = "3690")]
        port: u16,
    },
}

/// Detects if the image is a compose stack or single container
pub fn detect_workload_type(image: &str) -> WorkloadKind {
    // Check if it's a directory with docker-compose.yml
    if std::path::Path::new(image).is_dir() {
        let compose_file = std::path::Path::new(image).join("docker-compose.yml");
        let compose_yaml = std::path::Path::new(image).join("docker-compose.yaml");

        if compose_file.exists() || compose_yaml.exists() {
            return WorkloadKind::ComposeStack;
        }
    }

    // Special handling for known stacks
    if image == "supabase/supabase" || image.contains("supabase") {
        return WorkloadKind::SupabaseStack;
    }

    // Default to single container
    WorkloadKind::SingleContainer
}

#[derive(Debug, Clone)]
pub enum WorkloadKind {
    SingleContainer,
    ComposeStack,
    SupabaseStack,
}

/// Handles the 'hanzo run' command
pub async fn handle_run(
    image: &str,
    command: Vec<String>,
    detach: bool,
    ports: Vec<String>,
    env: Vec<String>,
    volumes: Vec<String>,
) -> Result<()> {
    let workload_type = detect_workload_type(image);

    match workload_type {
        WorkloadKind::SupabaseStack => {
            println!("🚀 Starting Supabase stack...");
            handle_supabase_stack().await
        }
        WorkloadKind::ComposeStack => {
            println!("📦 Starting compose stack from {}", image);
            handle_compose_stack(image).await
        }
        WorkloadKind::SingleContainer => {
            println!("🐳 Running container: {}", image);
            handle_single_container(image, command, detach, ports, env, volumes).await
        }
    }
}

/// Handles running Supabase stack
async fn handle_supabase_stack() -> Result<()> {
    println!("📥 Setting up Supabase...");

    // Check if Supabase repo exists locally
    let supabase_dir = "/tmp/supabase";
    if !std::path::Path::new(supabase_dir).exists() {
        println!("📦 Cloning Supabase repository...");
        Command::new("git")
            .args(&["clone", "https://github.com/supabase/supabase.git", supabase_dir])
            .status()?;
    }

    // Navigate to docker directory
    let docker_dir = format!("{}/docker", supabase_dir);

    // Copy example env
    let env_file = format!("{}/.env", docker_dir);
    if !std::path::Path::new(&env_file).exists() {
        println!("📝 Creating environment configuration...");
        Command::new("cp")
            .args(&[
                &format!("{}/.env.example", docker_dir),
                &env_file,
            ])
            .status()?;
    }

    // Pull all images first
    println!("📥 Pulling Supabase images...");
    Command::new("docker")
        .current_dir(&docker_dir)
        .args(&["compose", "pull"])
        .status()?;

    // Start the stack
    println!("🚀 Starting Supabase services...");
    let status = Command::new("docker")
        .current_dir(&docker_dir)
        .args(&["compose", "up", "-d"])
        .status()?;

    if status.success() {
        println!("✅ Supabase is running!");
        println!("📊 Studio: http://localhost:54323");
        println!("🔌 API: http://localhost:54321");
        println!("🗄️  Database: postgresql://postgres:postgres@localhost:54322/postgres");

        // Record in hanzod
        record_supabase_deployment().await?;
    } else {
        return Err(anyhow!("Failed to start Supabase"));
    }

    Ok(())
}

/// Records Supabase deployment in hanzod
async fn record_supabase_deployment() -> Result<()> {
    // Send workload to hanzod API
    let client = reqwest::Client::new();
    let workload = serde_json::json!({
        "id": format!("supabase-{}", chrono::Utc::now().timestamp()),
        "workload_type": {
            "Compute": {
                "image": "supabase/supabase",
                "command": ["compose", "up"]
            }
        },
        "resources": {
            "memory_mb": 4096,
            "cpu_cores": 2.0
        }
    });

    let response = client
        .post("http://localhost:3690/v1/workloads")
        .json(&workload)
        .send()
        .await?;

    if response.status().is_success() {
        println!("📝 Recorded in hanzod");
    }

    Ok(())
}

/// Handles compose stack
async fn handle_compose_stack(path: &str) -> Result<()> {
    Command::new("docker")
        .current_dir(path)
        .args(&["compose", "up", "-d"])
        .status()?;

    Ok(())
}

/// Handles single container
async fn handle_single_container(
    image: &str,
    command: Vec<String>,
    detach: bool,
    ports: Vec<String>,
    env: Vec<String>,
    volumes: Vec<String>,
) -> Result<()> {
    // First, ensure image is pulled
    println!("📥 Pulling image: {}", image);
    let pull_status = Command::new("docker")
        .args(&["pull", image])
        .status()?;

    if !pull_status.success() {
        return Err(anyhow!("Failed to pull image: {}", image));
    }

    // Build docker run command
    let mut args = vec!["run"];

    if detach {
        args.push("-d");
    }

    // Add port mappings
    for port in &ports {
        args.push("-p");
        args.push(port);
    }

    // Add environment variables
    for env_var in &env {
        args.push("-e");
        args.push(env_var);
    }

    // Add volume mounts
    for volume in &volumes {
        args.push("-v");
        args.push(volume);
    }

    // Add image
    args.push(image);

    // Add command if specified
    for cmd in &command {
        args.push(cmd);
    }

    // Run the container
    println!("🏃 Starting container...");
    let status = Command::new("docker")
        .args(&args)
        .status()?;

    if status.success() {
        println!("✅ Container started successfully");

        // Record in hanzod
        let client = reqwest::Client::new();
        let workload = serde_json::json!({
            "id": format!("{}-{}", image.replace('/', "-"), chrono::Utc::now().timestamp()),
            "workload_type": {
                "Compute": {
                    "image": image,
                    "command": command
                }
            },
            "resources": {
                "memory_mb": 1024,
                "cpu_cores": 1.0
            }
        });

        let _ = client
            .post("http://localhost:3690/v1/workloads")
            .json(&workload)
            .send()
            .await;
    } else {
        return Err(anyhow!("Failed to start container"));
    }

    Ok(())
}

/// Lists running containers
pub async fn handle_ps(all: bool) -> Result<()> {
    let mut args = vec!["ps"];
    if all {
        args.push("-a");
    }

    Command::new("docker")
        .args(&args)
        .status()?;

    Ok(())
}

/// Shows container logs
pub async fn handle_logs(id: &str, follow: bool) -> Result<()> {
    let mut args = vec!["logs", id];
    if follow {
        args.push("-f");
    }

    Command::new("docker")
        .args(&args)
        .status()?;

    Ok(())
}

/// Stops a container
pub async fn handle_stop(id: &str) -> Result<()> {
    println!("🛑 Stopping container: {}", id);

    Command::new("docker")
        .args(&["stop", id])
        .status()?;

    println!("✅ Container stopped");
    Ok(())
}

/// Executes command in container
pub async fn handle_exec(id: &str, command: Vec<String>) -> Result<()> {
    let mut args = vec!["exec", "-it", id];
    for cmd in &command {
        args.push(cmd);
    }

    Command::new("docker")
        .args(&args)
        .status()?;

    Ok(())
}

/// Pulls an OCI image
pub async fn handle_pull(image: &str) -> Result<()> {
    println!("📥 Pulling image: {}", image);

    let status = Command::new("docker")
        .args(&["pull", image])
        .status()?;

    if status.success() {
        println!("✅ Image pulled successfully");
    } else {
        return Err(anyhow!("Failed to pull image"));
    }

    Ok(())
}