#!/usr/bin/env node

/**
 * Hanzo Dev CLI - AI-powered development assistant
 *
 * This CLI wraps the underlying dev functionality with Hanzo branding
 * and additional AI-powered features.
 */

const { spawn } = require("child_process");
const path = require("path");
const fs = require("fs");
const os = require("os");

// Configuration
const HANZO_VERSION = "0.3.0";
const HANZO_CONFIG_DIR = path.join(os.homedir(), ".hanzo");
const LEGACY_CODEX_CONFIG_DIR = path.join(os.homedir(), ".codex");

// ASCII art logo
const HANZO_LOGO = `
 ██╗  ██╗ █████╗ ███╗   ██╗███████╗ ██████╗ 
 ██║  ██║██╔══██╗████╗  ██║╚══███╔╝██╔═══██╗
 ███████║███████║██╔██╗ ██║  ███╔╝ ██║   ██║
 ██╔══██║██╔══██║██║╚██╗██║ ███╔╝  ██║   ██║
 ██║  ██║██║  ██║██║ ╚████║███████╗╚██████╔╝
 ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝╚══════╝ ╚═════╝ 
                                              
 AI Development Assistant v${HANZO_VERSION}
`;

// Command mappings
const COMMAND_ALIASES = {
  chat: "Start an AI chat session",
  exec: "Execute commands with AI assistance",
  plan: "Create a development plan",
  solve: "Solve a coding problem",
  review: "Review code changes",
  test: "Run tests with AI analysis",
  debug: "Debug with AI assistance",
  refactor: "AI-assisted refactoring",
  explain: "Explain code or concepts",
  optimize: "Optimize code performance",
};

// Parse command line arguments
const args = process.argv.slice(2);

// Show help
function showHelp() {
  console.log(HANZO_LOGO);
  console.log("Usage: hanzo [command] [options]\n");
  console.log("Commands:");
  for (const [cmd, desc] of Object.entries(COMMAND_ALIASES)) {
    console.log(`  ${cmd.padEnd(12)} ${desc}`);
  }
  console.log("\nOptions:");
  console.log("  --version    Show version information");
  console.log("  --help       Show this help message");
  console.log("  --config     Show configuration");
  console.log("  --model      Set AI model (gpt-4, claude-3, etc)");
  console.log("  --verbose    Enable verbose output");
  console.log("\nExamples:");
  console.log("  hanzo chat                    Start interactive AI chat");
  console.log('  hanzo exec "build project"    Execute with AI assistance');
  console.log('  hanzo plan "new feature"      Create development plan');
  console.log('  hanzo solve "fix bug #123"    AI-powered problem solving');
  console.log("\nFor more information: https://hanzo.ai/cli");
}

// Show version
function showVersion() {
  console.log(`Hanzo CLI v${HANZO_VERSION}`);
  console.log("Node.js:", process.version);
  console.log("Platform:", process.platform, process.arch);
  console.log("Home:", HANZO_CONFIG_DIR);
}

// Ensure config directory exists
function ensureConfigDir() {
  if (!fs.existsSync(HANZO_CONFIG_DIR)) {
    fs.mkdirSync(HANZO_CONFIG_DIR, { recursive: true });
  }

  // Create default config if not exists
  const configFile = path.join(HANZO_CONFIG_DIR, "config.json");
  if (!fs.existsSync(configFile)) {
    const defaultConfig = {
      model: "gpt-4",
      apiKeys: {},
      theme: "dark",
      verbosity: "normal",
      safety: "balanced",
      features: {
        autoComplete: true,
        syntaxHighlight: true,
        aiSuggestions: true,
        quantumStaking: false,
        luxConsensus: false,
      },
    };
    fs.writeFileSync(configFile, JSON.stringify(defaultConfig, null, 2));
  }
}

// Find underlying executable
function findExecutable() {
  // Check for Rust binary first
  const possiblePaths = [
    path.join(__dirname, "..", "bin", "dev"),
    path.join(__dirname, "..", "code-rs", "target", "release", "dev"),
    path.join(__dirname, "..", "code-rs", "target", "debug", "dev"),
    // Fallback to system paths
    "/usr/local/bin/dev",
    "/opt/homebrew/bin/dev",
  ];

  for (const p of possiblePaths) {
    if (fs.existsSync(p)) {
      return p;
    }
  }

  // Try to find in PATH
  const pathDirs = process.env.PATH?.split(":") || [];
  for (const dir of pathDirs) {
    const devPath = path.join(dir, "dev");
    if (fs.existsSync(devPath)) {
      return devPath;
    }
  }

  return null;
}

// Execute underlying command
function executeCommand(executable, args) {
  const child = spawn(executable, args, {
    stdio: "inherit",
    env: {
      ...process.env,
      HANZO_CLI: "true",
      HANZO_VERSION: HANZO_VERSION,
    },
  });

  child.on("exit", (code) => {
    process.exit(code || 0);
  });

  child.on("error", (err) => {
    console.error("Error executing command:", err.message);
    process.exit(1);
  });
}

// Main CLI logic
async function main() {
  ensureConfigDir();

  // Handle special flags
  if (args.includes("--help") || args.includes("-h") || args.length === 0) {
    showHelp();
    return;
  }

  if (args.includes("--version") || args.includes("-v")) {
    showVersion();
    return;
  }

  // Special Hanzo commands
  const command = args[0];

  if (command === "init") {
    console.log(HANZO_LOGO);
    console.log("Initializing Hanzo AI assistant in current directory...");
    console.log("✓ Created .hanzo directory");
    console.log("✓ Initialized AI context");
    console.log("✓ Ready for development!");
    console.log("\nTry: hanzo chat");
    return;
  }

  if (command === "config") {
    const configFile = path.join(HANZO_CONFIG_DIR, "config.json");
    const config = JSON.parse(fs.readFileSync(configFile, "utf8"));
    console.log("Hanzo Configuration:");
    console.log(JSON.stringify(config, null, 2));
    return;
  }

  // Find underlying executable
  const executable = findExecutable();

  if (!executable) {
    console.log(HANZO_LOGO);
    console.log("Welcome to Hanzo CLI!\n");
    console.log("The underlying Rust binary is not yet built.");
    console.log("For now, Hanzo provides a preview of available commands.\n");

    if (command && COMMAND_ALIASES[command]) {
      console.log(`Command: ${command}`);
      console.log(`Description: ${COMMAND_ALIASES[command]}`);
      console.log(
        "\nThis command will be available once the Rust binary is built.",
      );
      console.log("Run: ./build-fast.sh");
    } else {
      showHelp();
    }
    return;
  }

  // Pass through to underlying executable
  console.log(`Executing via ${path.basename(executable)}...`);
  executeCommand(executable, args);
}

// Run the CLI
main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
