#!/usr/bin/env node
// Unified entry point for the Code CLI (fork of OpenAI Codex).

import path from "path";
import { fileURLToPath } from "url";

// __dirname equivalent in ESM
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const { platform, arch } = process;

let targetTriple = null;
switch (platform) {
  case "linux":
  case "android":
    switch (arch) {
      case "x64":
        targetTriple = "x86_64-unknown-linux-musl";
        break;
      case "arm64":
        targetTriple = "aarch64-unknown-linux-musl";
        break;
      default:
        break;
    }
    break;
  case "darwin":
    switch (arch) {
      case "x64":
        targetTriple = "x86_64-apple-darwin";
        break;
      case "arm64":
        targetTriple = "aarch64-apple-darwin";
        break;
      default:
        break;
    }
    break;
  case "win32":
    switch (arch) {
      case "x64":
        targetTriple = "x86_64-pc-windows-msvc.exe";
        break;
      case "arm64":
        // We do not build this today, fall through...
      default:
        break;
    }
    break;
  default:
    break;
}

if (!targetTriple) {
  throw new Error(`Unsupported platform: ${platform} (${arch})`);
}

// Prefer new 'code-*' binary names; fall back to legacy 'coder-*' if missing.
let binaryPath = path.join(__dirname, "..", "bin", `code-${targetTriple}`);
if (!existsSync(binaryPath)) {
  binaryPath = path.join(__dirname, "..", "bin", `coder-${targetTriple}`);
}

// Check if binary exists and try to fix permissions if needed
import { existsSync, chmodSync, statSync, openSync, readSync, closeSync } from "fs";
if (existsSync(binaryPath)) {
  try {
    // Ensure binary is executable on Unix-like systems
    if (platform !== "win32") {
      chmodSync(binaryPath, 0o755);
    }
  } catch (e) {
    // Ignore permission errors, will be caught below if it's a real problem
  }
} else {
  console.error(`Binary not found: ${binaryPath}`);
  console.error(`Please try reinstalling the package:`);
  console.error(`  npm uninstall -g @just-every/code`);
  console.error(`  npm install -g @just-every/code`);
  process.exit(1);
}

// Lightweight header validation to provide clearer errors before spawn
const validateBinary = (p) => {
  try {
    const st = statSync(p);
    if (!st.isFile() || st.size === 0) {
      return { ok: false, reason: "empty or not a regular file" };
    }
    const fd = openSync(p, "r");
    try {
      const buf = Buffer.alloc(4);
      const n = readSync(fd, buf, 0, 4, 0);
      if (n < 2) return { ok: false, reason: "too short" };
      if (platform === "win32") {
        if (!(buf[0] === 0x4d && buf[1] === 0x5a)) return { ok: false, reason: "invalid PE header (missing MZ)" };
      } else if (platform === "linux" || platform === "android") {
        if (!(buf[0] === 0x7f && buf[1] === 0x45 && buf[2] === 0x4c && buf[3] === 0x46)) return { ok: false, reason: "invalid ELF header" };
      } else if (platform === "darwin") {
        const isMachO = (buf[0] === 0xcf && buf[1] === 0xfa && buf[2] === 0xed && buf[3] === 0xfe) ||
                        (buf[0] === 0xca && buf[1] === 0xfe && buf[2] === 0xba && buf[3] === 0xbe);
        if (!isMachO) return { ok: false, reason: "invalid Mach-O header" };
      }
    } finally {
      closeSync(fd);
    }
    return { ok: true };
  } catch (e) {
    return { ok: false, reason: e.message };
  }
};

const validation = validateBinary(binaryPath);
if (!validation.ok) {
  console.error(`The native binary at ${binaryPath} appears invalid: ${validation.reason}`);
  console.error("This can happen if the download failed or was modified by antivirus/proxy.");
  console.error("Please try reinstalling:");
  console.error("  npm uninstall -g @just-every/code");
  console.error("  npm install -g @just-every/code");
  if (platform === "win32") {
    console.error("If the issue persists, clear npm cache and disable antivirus temporarily:");
    console.error("  npm cache clean --force");
  }
  process.exit(1);
}

// Use an asynchronous spawn instead of spawnSync so that Node is able to
// respond to signals (e.g. Ctrl-C / SIGINT) while the native binary is
// executing. This allows us to forward those signals to the child process
// and guarantees that when either the child terminates or the parent
// receives a fatal signal, both processes exit in a predictable manner.
const { spawn } = await import("child_process");

const child = spawn(binaryPath, process.argv.slice(2), {
  stdio: "inherit",
  env: { ...process.env, CODER_MANAGED_BY_NPM: "1", CODEX_MANAGED_BY_NPM: "1" },
});

child.on("error", (err) => {
  // Typically triggered when the binary is missing or not executable.
  const code = err && err.code;
  if (code === 'EACCES') {
    console.error(`Permission denied: ${binaryPath}`);
    console.error(`Try running: chmod +x "${binaryPath}"`);
    console.error(`Or reinstall the package with: npm install -g @just-every/code`);
  } else if (code === 'EFTYPE' || code === 'ENOEXEC') {
    console.error(`Failed to execute native binary: ${binaryPath}`);
    console.error("The file may be corrupt or of the wrong type. Reinstall usually fixes this:");
    console.error("  npm uninstall -g @just-every/code && npm install -g @just-every/code");
    if (platform === 'win32') {
      console.error("On Windows, ensure the .exe downloaded correctly (proxy/AV can interfere).");
      console.error("Try clearing cache: npm cache clean --force");
    }
  } else {
    console.error(err);
  }
  process.exit(1);
});

// Forward common termination signals to the child so that it shuts down
// gracefully. In the handler we temporarily disable the default behavior of
// exiting immediately; once the child has been signaled we simply wait for
// its exit event which will in turn terminate the parent (see below).
const forwardSignal = (signal) => {
  if (child.killed) {
    return;
  }
  try {
    child.kill(signal);
  } catch {
    /* ignore */
  }
};

["SIGINT", "SIGTERM", "SIGHUP"].forEach((sig) => {
  process.on(sig, () => forwardSignal(sig));
});

// When the child exits, mirror its termination reason in the parent so that
// shell scripts and other tooling observe the correct exit status.
// Wrap the lifetime of the child process in a Promise so that we can await
// its termination in a structured way. The Promise resolves with an object
// describing how the child exited: either via exit code or due to a signal.
const childResult = await new Promise((resolve) => {
  child.on("exit", (code, signal) => {
    if (signal) {
      resolve({ type: "signal", signal });
    } else {
      resolve({ type: "code", exitCode: code ?? 1 });
    }
  });
});

if (childResult.type === "signal") {
  // Re-emit the same signal so that the parent terminates with the expected
  // semantics (this also sets the correct exit code of 128 + n).
  process.kill(process.pid, childResult.signal);
} else {
  process.exit(childResult.exitCode);
}
