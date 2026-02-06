// postinstall.js - Download and extract platform-specific binary for hanzo-node

import { platform as nodePlatform, arch as nodeArch } from "os";
import path from "path";
import { fileURLToPath } from "url";
import {
  existsSync,
  mkdirSync,
  copyFileSync,
  chmodSync,
  readFileSync,
  unlinkSync,
  createWriteStream,
} from "fs";
import { execSync } from "child_process";
import { get as httpsGet } from "https";

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
      default:
        break;
    }
    break;
  default:
    break;
}

const getPlatformPackageName = () => {
  const plt = nodePlatform();
  const cpu = nodeArch();
  if (plt === "win32") return "hanzo-node-win32-x64";
  if (plt === "darwin" && cpu === "arm64") return "hanzo-node-darwin-arm64";
  if (plt === "darwin" && cpu === "x64") return "hanzo-node-darwin-x64";
  if (plt === "linux" && cpu === "x64") return "hanzo-node-linux-x64-musl";
  if (plt === "linux" && cpu === "arm64") return "hanzo-node-linux-arm64-musl";
  return null;
};

const getCacheDir = (version) => {
  const plt = nodePlatform();
  const home = process.env.HOME || process.env.USERPROFILE || "";
  let base = "";
  if (plt === "win32") {
    base = process.env.LOCALAPPDATA || path.join(home, "AppData", "Local");
  } else if (plt === "darwin") {
    base = path.join(home, "Library", "Caches");
  } else {
    base = process.env.XDG_CACHE_HOME || path.join(home, ".cache");
  }
  const dir = path.join(base, "hanzo", "node", version);
  if (!existsSync(dir)) mkdirSync(dir, { recursive: true });
  return dir;
};

const httpsDownload = (url, dest) =>
  new Promise((resolve, reject) => {
    const req = httpsGet(url, (res) => {
      const status = res.statusCode || 0;
      if (status >= 300 && status < 400 && res.headers.location) {
        return resolve(httpsDownload(res.headers.location, dest));
      }
      if (status !== 200) {
        return reject(new Error(`HTTP ${status}`));
      }
      const out = createWriteStream(dest);
      res.pipe(out);
      out.on("finish", () => out.close(resolve));
      out.on("error", (e) => {
        try {
          unlinkSync(dest);
        } catch {}
        reject(e);
      });
    });
    req.on("error", (e) => {
      try {
        unlinkSync(dest);
      } catch {}
      reject(e);
    });
    req.setTimeout(120000, () => {
      req.destroy(new Error("download timed out"));
    });
  });

export async function runPostinstall(opts = {}) {
  const { invokedByRuntime = false, skipGlobalAlias = false } = opts;

  if (!targetTriple) {
    if (!invokedByRuntime) {
      console.log(
        `hanzo-node: unsupported platform ${platform}/${arch}, skipping postinstall`,
      );
    }
    return;
  }

  const binDir = path.join(__dirname, "bin");
  if (!existsSync(binDir)) mkdirSync(binDir, { recursive: true });

  const binaryPath = path.join(binDir, `hanzo-${targetTriple}`);
  const legacyBinaryPath = path.join(binDir, `dev-${targetTriple}`);

  // Already present?
  if (existsSync(binaryPath) || existsSync(legacyBinaryPath)) {
    if (!invokedByRuntime) {
      console.log("hanzo-node: binary already present");
    }
    return;
  }

  // Try platform package first
  const pkgName = getPlatformPackageName();
  if (pkgName) {
    try {
      const req = (await import("module")).createRequire(import.meta.url);
      const pkgJson = req.resolve(`${pkgName}/package.json`);
      const pkgDir = path.dirname(pkgJson);
      const src = path.join(pkgDir, "bin", `hanzo-${targetTriple}`);
      if (existsSync(src)) {
        copyFileSync(src, binaryPath);
        if (platform !== "win32") {
          try {
            chmodSync(binaryPath, 0o755);
          } catch {}
        }
        if (!invokedByRuntime) {
          console.log(`hanzo-node: installed from ${pkgName}`);
        }
        return;
      }
    } catch (e) {
      // Fall through to download
    }
  }

  // Download from GitHub release
  try {
    const pkg = JSON.parse(
      readFileSync(path.join(__dirname, "package.json"), "utf8"),
    );
    const version = pkg.version;

    const isWin = platform === "win32";
    const binaryName = `code-${targetTriple}`;
    const archiveName = isWin
      ? `${binaryName}.zip`
      : (() => {
          try {
            execSync("zstd --version", { stdio: "ignore", shell: true });
            return `${binaryName}.zst`;
          } catch {
            return `${binaryName}.tar.gz`;
          }
        })();
    const url = `https://github.com/hanzoai/dev/releases/download/v${version}/${archiveName}`;

    if (!invokedByRuntime) {
      console.log(`hanzo-node: downloading ${archiveName}...`);
    }

    const tmp = path.join(binDir, `.${archiveName}.part`);
    await httpsDownload(url, tmp);

    if (isWin) {
      const sysRoot =
        process.env.SystemRoot || process.env.windir || "C:\\Windows";
      const psFull = path.join(
        sysRoot,
        "System32",
        "WindowsPowerShell",
        "v1.0",
        "powershell.exe",
      );
      const psCmd = `Expand-Archive -Path '${tmp}' -DestinationPath '${binDir}' -Force`;
      let ok = false;
      try {
        execSync(`"${psFull}" -NoProfile -NonInteractive -Command "${psCmd}"`, {
          stdio: "ignore",
        });
        ok = true;
      } catch {}
      if (!ok) {
        try {
          execSync(
            `powershell -NoProfile -NonInteractive -Command "${psCmd}"`,
            { stdio: "ignore" },
          );
          ok = true;
        } catch {}
      }
      if (!ok) {
        try {
          execSync(`pwsh -NoProfile -NonInteractive -Command "${psCmd}"`, {
            stdio: "ignore",
          });
          ok = true;
        } catch {}
      }
      if (!ok) {
        execSync(`tar -xf "${tmp}" -C "${binDir}"`, {
          stdio: "ignore",
          shell: true,
        });
      }
      try {
        unlinkSync(tmp);
      } catch {}
    } else {
      if (archiveName.endsWith(".zst")) {
        execSync(`zstd -d '${tmp}' -o '${binaryPath}'`, {
          stdio: "ignore",
          shell: true,
        });
        try {
          unlinkSync(tmp);
        } catch {}
      } else {
        execSync(`tar -xzf '${tmp}' -C '${binDir}'`, {
          stdio: "ignore",
          shell: true,
        });
        try {
          unlinkSync(tmp);
        } catch {}
      }
      try {
        chmodSync(binaryPath, 0o755);
      } catch {}
    }

    // Cache for future use
    const cacheDir = getCacheDir(version);
    const cachePath = path.join(cacheDir, `hanzo-${targetTriple}`);
    try {
      copyFileSync(binaryPath, cachePath);
    } catch {}

    if (!invokedByRuntime) {
      console.log("hanzo-node: installation complete");
    }
  } catch (e) {
    if (!invokedByRuntime) {
      console.error(`hanzo-node: postinstall failed: ${e.message}`);
      console.error(
        "You can try running the CLI directly, which will attempt to bootstrap the binary.",
      );
    }
    throw e;
  }
}

// Run if invoked directly
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  runPostinstall().catch(() => process.exit(0)); // Don't fail npm install
}
