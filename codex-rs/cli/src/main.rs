use clap::CommandFactory;
use clap::Parser;
use clap_complete::Shell;
use clap_complete::generate;
use codex_arg0::arg0_dispatch_or_else;
use codex_chatgpt::apply_command::ApplyCommand;
use codex_chatgpt::apply_command::run_apply_command;
use codex_cli::LandlockCommand;
use codex_cli::SeatbeltCommand;
use codex_cli::login::run_login_status;
use codex_cli::login::run_login_with_api_key;
use codex_cli::login::run_login_with_chatgpt;
use codex_cli::login::run_logout;
use codex_cli::proto;
use codex_common::CliConfigOverrides;
use codex_exec::Cli as ExecCli;
use codex_tui::Cli as TuiCli;
use std::path::PathBuf;

use crate::proto::ProtoCli;

/// Codex CLI
///
/// If no subcommand is specified, options will be forwarded to the interactive CLI.
#[derive(Debug, Parser)]
#[clap(
    author,
    name = "code",
    version = option_env!("CODE_VERSION").unwrap_or(env!("CARGO_PKG_VERSION")),
    // If a sub‑command is given, ignore requirements of the default args.
    subcommand_negates_reqs = true,
    // The executable is sometimes invoked via a platform‑specific name like
    // `codex-x86_64-unknown-linux-musl`, but the help output should always use
    // the generic `codex` command name that users run.
    bin_name = "code"
)]
struct MultitoolCli {
    #[clap(flatten)]
    pub config_overrides: CliConfigOverrides,

    #[clap(flatten)]
    interactive: TuiCli,

    #[clap(subcommand)]
    subcommand: Option<Subcommand>,
}

#[derive(Debug, clap::Subcommand)]
enum Subcommand {
    /// Run Codex non-interactively.
    #[clap(visible_alias = "e")]
    Exec(ExecCli),

    /// Manage login.
    Login(LoginCommand),

    /// Remove stored authentication credentials.
    Logout(LogoutCommand),

    /// Experimental: run Codex as an MCP server.
    Mcp,

    /// Run the Protocol stream via stdin/stdout
    #[clap(visible_alias = "p")]
    Proto(ProtoCli),

    /// Generate shell completion scripts.
    Completion(CompletionCommand),

    /// Internal debugging commands.
    Debug(DebugArgs),

    /// Debug: replay ordering from response.json and codex-tui.log
    #[clap(hide = false)]
    OrderReplay(OrderReplayArgs),

    /// Apply the latest diff produced by Codex agent as a `git apply` to your local working tree.
    #[clap(visible_alias = "a")]
    Apply(ApplyCommand),

    /// Internal: generate TypeScript protocol bindings.
    #[clap(hide = true)]
    GenerateTs(GenerateTsCommand),
}

#[derive(Debug, Parser)]
struct CompletionCommand {
    /// Shell to generate completions for
    #[clap(value_enum, default_value_t = Shell::Bash)]
    shell: Shell,
}

#[derive(Debug, Parser)]
struct DebugArgs {
    #[command(subcommand)]
    cmd: DebugCommand,
}

#[derive(Debug, clap::Subcommand)]
enum DebugCommand {
    /// Run a command under Seatbelt (macOS only).
    Seatbelt(SeatbeltCommand),

    /// Run a command under Landlock+seccomp (Linux only).
    Landlock(LandlockCommand),
}

#[derive(Debug, Parser)]
struct LoginCommand {
    #[clap(skip)]
    config_overrides: CliConfigOverrides,

    #[arg(long = "api-key", value_name = "API_KEY")]
    api_key: Option<String>,

    #[command(subcommand)]
    action: Option<LoginSubcommand>,
}

#[derive(Debug, clap::Subcommand)]
enum LoginSubcommand {
    /// Show login status.
    Status,
}

#[derive(Debug, Parser)]
struct LogoutCommand {
    #[clap(skip)]
    config_overrides: CliConfigOverrides,
}

#[derive(Debug, Parser)]
struct GenerateTsCommand {
    /// Output directory where .ts files will be written
    #[arg(short = 'o', long = "out", value_name = "DIR")]
    out_dir: PathBuf,

    /// Optional path to the Prettier executable to format generated files
    #[arg(short = 'p', long = "prettier", value_name = "PRETTIER_BIN")]
    prettier: Option<PathBuf>,
}

#[derive(Debug, Parser)]
struct OrderReplayArgs {
    /// Path to a response.json captured under ~/.codex/debug_logs/*_response.json
    response_json: std::path::PathBuf,
    /// Path to codex-tui.log (typically ~/.codex/log/codex-tui.log)
    tui_log: std::path::PathBuf,
}

fn main() -> anyhow::Result<()> {
    arg0_dispatch_or_else(|codex_linux_sandbox_exe| async move {
        cli_main(codex_linux_sandbox_exe).await?;
        Ok(())
    })
}

async fn cli_main(codex_linux_sandbox_exe: Option<PathBuf>) -> anyhow::Result<()> {
    let cli = MultitoolCli::parse();

    match cli.subcommand {
        None => {
            let mut tui_cli = cli.interactive;
            prepend_config_flags(&mut tui_cli.config_overrides, cli.config_overrides);
            let usage = codex_tui::run_main(tui_cli, codex_linux_sandbox_exe).await?;
            if !usage.is_zero() {
                println!("{}", codex_core::protocol::FinalOutput::from(usage));
            }
        }
        Some(Subcommand::Exec(mut exec_cli)) => {
            prepend_config_flags(&mut exec_cli.config_overrides, cli.config_overrides);
            codex_exec::run_main(exec_cli, codex_linux_sandbox_exe).await?;
        }
        Some(Subcommand::Mcp) => {
            codex_mcp_server::run_main(codex_linux_sandbox_exe, cli.config_overrides).await?;
        }
        Some(Subcommand::Login(mut login_cli)) => {
            prepend_config_flags(&mut login_cli.config_overrides, cli.config_overrides);
            match login_cli.action {
                Some(LoginSubcommand::Status) => {
                    run_login_status(login_cli.config_overrides).await;
                }
                None => {
                    if let Some(api_key) = login_cli.api_key {
                        run_login_with_api_key(login_cli.config_overrides, api_key).await;
                    } else {
                        run_login_with_chatgpt(login_cli.config_overrides).await;
                    }
                }
            }
        }
        Some(Subcommand::Logout(mut logout_cli)) => {
            prepend_config_flags(&mut logout_cli.config_overrides, cli.config_overrides);
            run_logout(logout_cli.config_overrides).await;
        }
        Some(Subcommand::Proto(mut proto_cli)) => {
            prepend_config_flags(&mut proto_cli.config_overrides, cli.config_overrides);
            proto::run_main(proto_cli).await?;
        }
        Some(Subcommand::Completion(completion_cli)) => {
            print_completion(completion_cli);
        }
        Some(Subcommand::Debug(debug_args)) => match debug_args.cmd {
            DebugCommand::Seatbelt(mut seatbelt_cli) => {
                prepend_config_flags(&mut seatbelt_cli.config_overrides, cli.config_overrides);
                codex_cli::debug_sandbox::run_command_under_seatbelt(
                    seatbelt_cli,
                    codex_linux_sandbox_exe,
                )
                .await?;
            }
            DebugCommand::Landlock(mut landlock_cli) => {
                prepend_config_flags(&mut landlock_cli.config_overrides, cli.config_overrides);
                codex_cli::debug_sandbox::run_command_under_landlock(
                    landlock_cli,
                    codex_linux_sandbox_exe,
                )
                .await?;
            }
        },
        Some(Subcommand::Apply(mut apply_cli)) => {
            prepend_config_flags(&mut apply_cli.config_overrides, cli.config_overrides);
            run_apply_command(apply_cli, None).await?;
        }
        Some(Subcommand::GenerateTs(gen_cli)) => {
            codex_protocol_ts::generate_ts(&gen_cli.out_dir, gen_cli.prettier.as_deref())?;
        }
        Some(Subcommand::OrderReplay(args)) => {
            order_replay_main(args)?;
        }
    }

    Ok(())
}

/// Prepend root-level overrides so they have lower precedence than
/// CLI-specific ones specified after the subcommand (if any).
fn prepend_config_flags(
    subcommand_config_overrides: &mut CliConfigOverrides,
    cli_config_overrides: CliConfigOverrides,
) {
    subcommand_config_overrides
        .raw_overrides
        .splice(0..0, cli_config_overrides.raw_overrides);
}

fn print_completion(cmd: CompletionCommand) {
    let mut app = MultitoolCli::command();
    let name = "codex";
    generate(cmd.shell, &mut app, name, &mut std::io::stdout());
}

fn order_replay_main(args: OrderReplayArgs) -> anyhow::Result<()> {
    use anyhow::{Context, Result};
    use regex::Regex;
    use serde_json::Value;
    use std::fs;

    fn parse_response_expected(path: &std::path::Path) -> Result<Vec<(u64, u64)>> {
        let data = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
        let v: Value = serde_json::from_str(&data)?;
        let events = v.get("events").and_then(|e| e.as_array()).cloned().unwrap_or_default();
        let mut items: Vec<(u64, u64)> = Vec::new();
        for ev in events {
            let data = ev.get("data");
            if let Some(d) = data {
                let out = d.get("output_index").and_then(|x| x.as_u64());
                let seq = d.get("sequence_number").and_then(|x| x.as_u64());
                if let (Some(out), Some(seq)) = (out, seq) {
                    items.push((out, seq));
                }
            }
        }
        items.sort();
        Ok(items)
    }

    #[derive(Debug)]
    struct InsertLog { ordered: bool, req: u64, out: u64, item_seq: u64, raw: u64 }

    fn parse_tui_inserts(path: &std::path::Path) -> Result<Vec<InsertLog>> {
        let text = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
        let re = Regex::new(r"insert window: seq=(?P<seq>\d+) \((?P<kind>[OU]):(?:req=(?P<req>\d+) out=(?P<out>\d+) seq=(?P<iseq>\d+)|(?P<uval>\d+))\)").unwrap();
        let mut out = Vec::new();
        for line in text.lines() {
            if let Some(caps) = re.captures(line) {
                let seq: u64 = caps.name("seq").unwrap().as_str().parse().unwrap_or(0);
                let ordered = &caps["kind"] == "O";
                let (req, out_idx, item_seq) = if ordered {
                    let req = caps.name("req").unwrap().as_str().parse().unwrap_or(0);
                    let out_idx = caps.name("out").unwrap().as_str().parse().unwrap_or(0);
                    let iseq = caps.name("iseq").unwrap().as_str().parse().unwrap_or(0);
                    (req, out_idx, iseq)
                } else {
                    (0, 0, caps.name("uval").unwrap().as_str().parse().unwrap_or(0))
                };
                out.push(InsertLog { ordered, req, out: out_idx, item_seq, raw: seq });
            }
        }
        Ok(out)
    }

    let expected = parse_response_expected(&args.response_json)?;
    let actual = parse_tui_inserts(&args.tui_log)?;

    println!("Expected (first 20 sorted by out,seq):");
    for (i, (out, seq)) in expected.iter().take(20).enumerate() {
        println!("  {:>3}: out={} seq={}", i, out, seq);
    }

    println!("\nActual inserts (first 40):");
    for (i, log) in actual.iter().take(40).enumerate() {
        if log.ordered {
            println!("  {:>3}: O:req={} out={} seq={} (raw={})", i, log.req, log.out, log.item_seq, log.raw);
        } else {
            println!("  {:>3}: U:{}", i, log.item_seq);
        }
    }

    // Simple check: assistant (out=1) should appear before tool (out=2) within same req
    let pos_out1 = actual.iter().position(|l| l.ordered && l.req == 1 && l.out == 1);
    let pos_out2 = actual.iter().position(|l| l.ordered && l.req == 1 && l.out == 2);
    println!("\nCheck (req=1): first out=1 at {:?}, first out=2 at {:?}", pos_out1, pos_out2);
    if let (Some(p1), Some(p2)) = (pos_out1, pos_out2) {
        if p1 < p2 { println!("Result: OK (assistant precedes tool)"); } else { println!("Result: WRONG (tool precedes assistant)"); }
    }

    Ok(())
}
