//! Hanzo Node CLI - RPC-based compute node for Hanzo Platform

use std::env;

use hanzo_node::{HanzoNode, NodeConfig, Result, VERSION, PKG_NAME};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

fn print_version() {
    println!("{PKG_NAME} {VERSION}");
}

fn print_help() {
    println!("{PKG_NAME} {VERSION}");
    println!();
    println!("USAGE:");
    println!("    hanzo-node [OPTIONS]");
    println!();
    println!("OPTIONS:");
    println!("    --node-id <ID>          Node identifier (default: random UUID)");
    println!("    --grpc-addr <ADDR>      gRPC server address (default: 0.0.0.0:50051)");
    println!("    --http-addr <ADDR>      HTTP health check address (default: 0.0.0.0:8080)");
    println!("    --p2p-addr <ADDR>       P2P listen address (default: /ip4/0.0.0.0/tcp/9000)");
    println!("    --data-dir <PATH>       Data directory (default: ./data)");
    println!("    --network-id <ID>       Network ID (43114=mainnet, 43113=testnet, 1337=local)");
    println!("    --bootstrap <ADDR>      Bootstrap peer address (can be repeated)");
    println!("    --operator <ADDR>       Operator wallet address");
    println!("    --mlx                   Enable MLX acceleration (macOS only)");
    println!("    -h, --help              Print help");
    println!("    -V, --version           Print version");
    println!();
    println!("ENVIRONMENT VARIABLES:");
    println!("    HANZO_NODE_ID           Node identifier");
    println!("    HANZO_GRPC_ADDR         gRPC server address");
    println!("    HANZO_HTTP_ADDR         HTTP health check address");
    println!("    HANZO_P2P_ADDR          P2P listen address");
    println!("    HANZO_DATA_DIR          Data directory");
    println!("    HANZO_NETWORK_ID        Network ID");
    println!("    HANZO_OPERATOR          Operator wallet address");
    println!("    HANZO_MLX               Enable MLX acceleration (1/true)");
    println!("    RUST_LOG                Log level (e.g., info, debug, hanzo_node=debug)");
}

fn parse_args() -> Result<Option<NodeConfig>> {
    let args: Vec<String> = env::args().collect();
    let mut config = NodeConfig::default();

    // Apply environment variables first
    if let Ok(v) = env::var("HANZO_NODE_ID") {
        config.node_id = v;
    }
    if let Ok(v) = env::var("HANZO_GRPC_ADDR") {
        config.grpc_addr = v;
    }
    if let Ok(v) = env::var("HANZO_HTTP_ADDR") {
        config.http_addr = v;
    }
    if let Ok(v) = env::var("HANZO_P2P_ADDR") {
        config.p2p_listen_addrs = vec![v];
    }
    if let Ok(v) = env::var("HANZO_DATA_DIR") {
        config.data_dir = v;
    }
    if let Ok(v) = env::var("HANZO_NETWORK_ID") {
        config.network_id = v;
    }
    if let Ok(v) = env::var("HANZO_OPERATOR") {
        config.operator_address = Some(v);
    }
    if let Ok(v) = env::var("HANZO_MLX") {
        config.mlx_enabled = v == "1" || v.to_lowercase() == "true";
    }

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_help();
                return Ok(None);
            }
            "-V" | "--version" => {
                print_version();
                return Ok(None);
            }
            "--node-id" => {
                i += 1;
                if i >= args.len() {
                    return Err(hanzo_node::Error::Config("--node-id requires a value".into()));
                }
                config.node_id = args[i].clone();
            }
            "--grpc-addr" => {
                i += 1;
                if i >= args.len() {
                    return Err(hanzo_node::Error::Config("--grpc-addr requires a value".into()));
                }
                config.grpc_addr = args[i].clone();
            }
            "--http-addr" => {
                i += 1;
                if i >= args.len() {
                    return Err(hanzo_node::Error::Config("--http-addr requires a value".into()));
                }
                config.http_addr = args[i].clone();
            }
            "--p2p-addr" => {
                i += 1;
                if i >= args.len() {
                    return Err(hanzo_node::Error::Config("--p2p-addr requires a value".into()));
                }
                config.p2p_listen_addrs = vec![args[i].clone()];
            }
            "--data-dir" => {
                i += 1;
                if i >= args.len() {
                    return Err(hanzo_node::Error::Config("--data-dir requires a value".into()));
                }
                config.data_dir = args[i].clone();
            }
            "--network-id" => {
                i += 1;
                if i >= args.len() {
                    return Err(hanzo_node::Error::Config("--network-id requires a value".into()));
                }
                config.network_id = args[i].clone();
            }
            "--bootstrap" => {
                i += 1;
                if i >= args.len() {
                    return Err(hanzo_node::Error::Config("--bootstrap requires a value".into()));
                }
                config.bootstrap_peers.push(args[i].clone());
            }
            "--operator" => {
                i += 1;
                if i >= args.len() {
                    return Err(hanzo_node::Error::Config("--operator requires a value".into()));
                }
                config.operator_address = Some(args[i].clone());
            }
            "--mlx" => {
                config.mlx_enabled = true;
            }
            arg if arg.starts_with('-') => {
                return Err(hanzo_node::Error::Config(format!("Unknown option: {arg}")));
            }
            _ => {
                // Ignore positional arguments for now
            }
        }
        i += 1;
    }

    Ok(Some(config))
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")))
        .init();

    // Parse arguments
    let config = match parse_args()? {
        Some(c) => c,
        None => return Ok(()), // --help or --version was printed
    };

    tracing::info!(
        node_id = %config.node_id,
        grpc_addr = %config.grpc_addr,
        http_addr = %config.http_addr,
        network_id = %config.network_id,
        "Starting Hanzo Node"
    );

    // Create and start the node
    let node = HanzoNode::new(config).await?;

    // Handle shutdown signals
    let shutdown_node = node.config().node_id.clone();
    tokio::spawn(async move {
        let mut sigint = tokio::signal::unix::signal(tokio::signal::unix::SignalKind::interrupt())
            .map_err(|e| tracing::error!("Failed to register SIGINT handler: {e}"))
            .ok();
        let mut sigterm = tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .map_err(|e| tracing::error!("Failed to register SIGTERM handler: {e}"))
            .ok();

        tokio::select! {
            _ = async {
                if let Some(ref mut s) = sigint {
                    s.recv().await
                } else {
                    std::future::pending::<()>().await;
                    None
                }
            } => {
                tracing::info!(node_id = %shutdown_node, "Received SIGINT, shutting down");
            }
            _ = async {
                if let Some(ref mut s) = sigterm {
                    s.recv().await
                } else {
                    std::future::pending::<()>().await;
                    None
                }
            } => {
                tracing::info!(node_id = %shutdown_node, "Received SIGTERM, shutting down");
            }
        }
    });

    // Start the node (blocks until shutdown)
    node.start().await?;

    Ok(())
}
