import * as NodeChildProcess from "node:child_process";
import * as NodeFS from "node:fs";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";

const desktopDir = NodePath.resolve(import.meta.dirname, "..");
const repoRoot = NodePath.resolve(desktopDir, "..", "..");
const manifestPath = NodePath.join(desktopDir, "src-tauri", "Cargo.toml");
// oxlint-disable-next-line hanzo/no-global-process-runtime -- standalone Node launcher script outside Effect runtime
const exeSuffix = NodeOS.platform() === "win32" ? ".exe" : "";
const releaseBin = NodePath.join(
  desktopDir,
  "src-tauri",
  "target",
  "release",
  `hanzo-dev-desktop${exeSuffix}`,
);

if (!NodeFS.existsSync(releaseBin)) {
  NodeChildProcess.execFileSync(
    "cargo",
    ["build", "--release", "--features", "custom-protocol", "--manifest-path", manifestPath],
    {
      stdio: "inherit",
      cwd: desktopDir,
    },
  );
}

const childEnv = {
  ...process.env,
  HANZO_DEV_REPO_ROOT: repoRoot,
};
delete childEnv.VITE_DEV_SERVER_URL;
delete childEnv.ELECTRON_RUN_AS_NODE;

const child = NodeChildProcess.spawn(releaseBin, [], {
  stdio: "inherit",
  cwd: desktopDir,
  env: childEnv,
});

child.on("exit", (code, signal) => {
  if (signal) {
    process.kill(process.pid, signal);
    return;
  }
  process.exit(code ?? 0);
});
