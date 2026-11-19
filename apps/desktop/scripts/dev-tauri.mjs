import * as NodeChildProcess from "node:child_process";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";

import { waitForResources } from "./wait-for-resources.mjs";

const desktopDir = NodePath.resolve(import.meta.dirname, "..");
const repoRoot = NodePath.resolve(desktopDir, "..", "..");
const manifestPath = NodePath.join(desktopDir, "src-tauri", "Cargo.toml");
// oxlint-disable-next-line hanzo/no-global-process-runtime -- standalone Node launcher script outside Effect runtime
const exeSuffix = NodeOS.platform() === "win32" ? ".exe" : "";
const debugBin = NodePath.join(
  desktopDir,
  "src-tauri",
  "target",
  "debug",
  `hanzo-dev-desktop${exeSuffix}`,
);

const devServerUrl = process.env.VITE_DEV_SERVER_URL?.trim();
if (!devServerUrl) {
  throw new Error("VITE_DEV_SERVER_URL is required for Tauri desktop development.");
}
const devServer = new URL(devServerUrl);
const port = Number.parseInt(devServer.port, 10);

NodeChildProcess.execFileSync("cargo", ["build", "--manifest-path", manifestPath], {
  stdio: "inherit",
  cwd: desktopDir,
});

await waitForResources({
  baseDir: desktopDir,
  files: ["../server/dist/bin.mjs"],
  tcpHost: devServer.hostname,
  tcpPort: port,
});

const child = NodeChildProcess.spawn(debugBin, [], {
  stdio: "inherit",
  cwd: desktopDir,
  env: {
    ...process.env,
    HANZO_DEV_REPO_ROOT: repoRoot,
    VITE_DEV_SERVER_URL: devServerUrl,
  },
});

for (const sig of ["SIGINT", "SIGTERM"]) {
  process.on(sig, () => {
    child.kill("SIGTERM");
  });
}

child.on("exit", (code) => {
  process.exit(code ?? 0);
});
