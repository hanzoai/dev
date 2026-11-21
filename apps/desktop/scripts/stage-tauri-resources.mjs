import * as NodeChildProcess from "node:child_process";
import * as NodeFS from "node:fs";
import * as NodeModule from "node:module";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";

const desktopDir = NodePath.resolve(import.meta.dirname, "..");
const repoRoot = NodePath.resolve(desktopDir, "..", "..");
const serverDir = NodePath.join(repoRoot, "apps", "server");
const webDistDir = NodePath.join(repoRoot, "apps", "web", "dist");
const serverDistDir = NodePath.join(serverDir, "dist");
const resourcesRoot = NodePath.join(desktopDir, "src-tauri", "resources");
// oxlint-disable-next-line hanzo/no-global-process-runtime -- standalone Node build script outside Effect runtime
const hostPlatform = NodeOS.platform();
// oxlint-disable-next-line hanzo/no-global-process-runtime -- standalone Node build script outside Effect runtime
const hostArch = NodeOS.arch();
const isWindows = hostPlatform === "win32";

const EXTERNAL_RUNTIME_PREFIXES = [
  "@cursor/sdk",
  "playwright-core",
  "node-pty",
  "ffi-rs",
  "@yuuang/",
  "@ff-labs/",
  "transcribe-cpp",
  "@transcribe-cpp/",
  "koffi",
  "@koromix/",
  "@napi-rs/keyring",
  "node-gyp-build",
  "node-addon-api",
  "bufferutil",
  "utf-8-validate",
  "@bufbuild/protobuf",
  "@connectrpc/connect",
  "@connectrpc/connect-node",
  "@connectrpc/connect-web",
  "@statsig/js-client",
  "@statsig/client-core",
  "zod",
  "undici",
  "@fastify/busboy",
];

function isExternalDep(name) {
  return EXTERNAL_RUNTIME_PREFIXES.some(
    (prefix) => name === prefix || name.startsWith(prefix.endsWith("/") ? prefix : `${prefix}/`),
  );
}

function runVp(args) {
  const vpBin = NodePath.join(repoRoot, "node_modules", ".bin", isWindows ? "vp.cmd" : "vp");
  NodeChildProcess.execFileSync(vpBin, args, {
    cwd: repoRoot,
    stdio: "inherit",
    shell: isWindows,
  });
}

function resolveInstalledPackageRoot(fromFile, depName) {
  const req = NodeModule.createRequire(fromFile);
  try {
    const pkgJsonPath = req.resolve(`${depName}/package.json`);
    return NodeFS.realpathSync(NodePath.dirname(pkgJsonPath));
  } catch {
    try {
      let cursor = NodePath.dirname(NodeFS.realpathSync(req.resolve(depName)));
      while (cursor !== NodePath.dirname(cursor)) {
        const candidate = NodePath.join(cursor, "package.json");
        if (NodeFS.existsSync(candidate)) {
          const parsed = JSON.parse(NodeFS.readFileSync(candidate, "utf8"));
          if (parsed.name === depName) {
            return cursor;
          }
        }
        cursor = NodePath.dirname(cursor);
      }
    } catch {
      return null;
    }
  }
  return null;
}

// 1. Ensure web frontend dist exists
if (
  process.env.HANZO_FORCE_WEB_BUILD === "1" ||
  !NodeFS.existsSync(NodePath.join(webDistDir, "index.html"))
) {
  console.log("[stage-tauri-resources] Building @hanzodev/web frontend...");
  runVp(["run", "--filter", "@hanzodev/web", "build"]);
}

// 2. Ensure server bundle dist/bin.mjs exists
if (
  process.env.HANZO_FORCE_SERVER_BUILD === "1" ||
  !NodeFS.existsSync(NodePath.join(serverDistDir, "bin.mjs"))
) {
  console.log("[stage-tauri-resources] Building hanzo server bundle...");
  runVp(["run", "--filter", "hanzo", "build:bundle"]);
}

// 3. Ensure native resource-monitor binary exists
const exeSuffix = isWindows ? ".exe" : "";
const monitorBinaryName = `hanzo-resource-monitor${exeSuffix}`;
const monitorManifest = NodePath.join(repoRoot, "native", "resource-monitor", "Cargo.toml");
const monitorReleaseBin = NodePath.join(
  repoRoot,
  "native",
  "resource-monitor",
  "target",
  "release",
  monitorBinaryName,
);

if (!NodeFS.existsSync(monitorReleaseBin) && NodeFS.existsSync(monitorManifest)) {
  console.log("[stage-tauri-resources] Building hanzo-resource-monitor...");
  NodeChildProcess.execFileSync(
    "cargo",
    ["build", "--locked", "--release", "--manifest-path", monitorManifest],
    { cwd: repoRoot, stdio: "inherit" },
  );
}

// 4. Clean and stage src-tauri/resources
NodeFS.rmSync(resourcesRoot, { recursive: true, force: true });
const stagedServerDir = NodePath.join(resourcesRoot, "server");
const stagedDistDir = NodePath.join(stagedServerDir, "dist");
const stagedNodeModulesDir = NodePath.join(stagedServerDir, "node_modules");
NodeFS.mkdirSync(stagedDistDir, { recursive: true });
NodeFS.mkdirSync(stagedNodeModulesDir, { recursive: true });
NodeFS.writeFileSync(NodePath.join(resourcesRoot, ".keep"), "");

const serverPkgJsonPath = NodePath.join(serverDir, "package.json");
const serverPkg = JSON.parse(NodeFS.readFileSync(serverPkgJsonPath, "utf8"));

NodeFS.writeFileSync(
  NodePath.join(stagedServerDir, "package.json"),
  JSON.stringify(
    {
      name: "hanzo-desktop-server",
      version: serverPkg.version ?? "0.1.10",
      private: true,
      type: "module",
    },
    null,
    2,
  ) + "\n",
);

// Copy server/dist without source maps or duplicate web client
NodeFS.cpSync(serverDistDir, stagedDistDir, {
  recursive: true,
  dereference: true,
  filter: (src) => {
    const rel = NodePath.relative(serverDistDir, src);
    if (rel === "client" || rel.startsWith(`client${NodePath.sep}`)) return false;
    if (src.endsWith(".map")) return false;
    return true;
  },
});

// Copy standalone server executable if built
const standaloneExe = NodePath.join(serverDir, "dist-exe", `hanzo${exeSuffix}`);
if (NodeFS.existsSync(standaloneExe)) {
  const destExe = NodePath.join(stagedServerDir, `hanzo${exeSuffix}`);
  NodeFS.copyFileSync(standaloneExe, destExe);
  if (!isWindows) {
    NodeFS.chmodSync(destExe, 0o755);
  }
}

// Copy hanzo-resource-monitor
if (NodeFS.existsSync(monitorReleaseBin)) {
  const destMonitor = NodePath.join(resourcesRoot, monitorBinaryName);
  NodeFS.copyFileSync(monitorReleaseBin, destMonitor);
  if (!isWindows) {
    NodeFS.chmodSync(destMonitor, 0o755);
  }
}

// Stage dereferenced runtime external node_modules via BFS from apps/server/package.json
const hostPlatformArch = `${hostPlatform}-${hostArch}`;
const visited = new Set();
const queue = [];

for (const depName of Object.keys(serverPkg.dependencies ?? {})) {
  if (isExternalDep(depName)) {
    queue.push([depName, serverPkgJsonPath]);
  }
}

while (queue.length > 0) {
  const [depName, fromFile] = queue.shift();
  if (visited.has(depName)) continue;

  const pkgRoot = resolveInstalledPackageRoot(fromFile, depName);
  if (!pkgRoot) continue;
  visited.add(depName);

  const destPkgDir = NodePath.join(stagedNodeModulesDir, depName);
  NodeFS.mkdirSync(NodePath.dirname(destPkgDir), { recursive: true });
  NodeFS.cpSync(pkgRoot, destPkgDir, {
    recursive: true,
    dereference: true,
    filter: (src) => {
      // Strip foreign node-pty prebuilds so macOS codesign never encounters Linux/Windows binaries.
      const rel = NodePath.relative(pkgRoot, src);
      const parts = rel.split(NodePath.sep);
      if (
        depName === "node-pty" &&
        parts[0] === "prebuilds" &&
        parts[1] &&
        parts[1] !== hostPlatformArch
      ) {
        return false;
      }
      if (parts.includes(".bin")) return false;
      return true;
    },
  });

  const childPkgJsonPath = NodePath.join(pkgRoot, "package.json");
  if (NodeFS.existsSync(childPkgJsonPath)) {
    const childPkg = JSON.parse(NodeFS.readFileSync(childPkgJsonPath, "utf8"));
    for (const nextDep of [
      ...Object.keys(childPkg.dependencies ?? {}),
      ...Object.keys(childPkg.optionalDependencies ?? {}),
    ]) {
      if (!visited.has(nextDep)) {
        queue.push([nextDep, childPkgJsonPath]);
      }
    }
  }
}

console.log(
  `[stage-tauri-resources] Staged server + ${visited.size} runtime external packages into ${resourcesRoot}`,
);

// The macOS menu-bar helper ships inside the app; it is the only menu-bar item.
if (hostPlatform === "darwin") {
  const menubarOut = NodePath.join(resourcesRoot, "menubar");
  NodeFS.rmSync(menubarOut, { recursive: true, force: true });
  NodeFS.mkdirSync(menubarOut, { recursive: true });
  NodeChildProcess.execFileSync(NodePath.join(desktopDir, "menubar", "build.sh"), [menubarOut], {
    stdio: "inherit",
  });
}
