#!/usr/bin/env node
// Windows-friendly preinstall: proactively free file locks from prior installs
// so npm/yarn/pnpm can stage the new package. No-ops on non-Windows.

import { platform } from 'os';
import { execSync } from 'child_process';
import { existsSync, readdirSync, rmSync, readFileSync, statSync } from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

function isWSL() {
  if (platform() !== 'linux') return false;
  try {
    const rel = readFileSync('/proc/version', 'utf8').toLowerCase();
    return rel.includes('microsoft') || !!process.env.WSL_DISTRO_NAME;
  } catch { return false; }
}

const isWin = platform() === 'win32';
const wsl = isWSL();
const isWinLike = isWin || wsl;

// Scope: only run for global installs, unless explicitly forced. Allow opt-out.
const isGlobal = process.env.npm_config_global === 'true';
const force = process.env.DEV_FORCE_PREINSTALL === '1';
const skip = process.env.DEV_SKIP_PREINSTALL === '1';
if (!isWinLike || skip || (!isGlobal && !force)) process.exit(0);

function tryExec(cmd, opts = {}) {
  try { execSync(cmd, { stdio: ['ignore', 'ignore', 'ignore'], shell: true, ...opts }); } catch { /* ignore */ }
}

// 1) Stop our native binary if it is holding locks. Avoid killing unrelated tools.
// Only available on native Windows; skip entirely on WSL to avoid noise.
if (isWin) {
  tryExec('taskkill /IM dev-x86_64-pc-windows-msvc.exe /F');
}

// 2) Remove stale staging dirs from previous failed installs under the global
//    @hanzo scope, which npm will reuse (e.g., .dev-XXXXX). Remove only
//    old entries and never the current staging or live package.
try {
  let scopeDir = '';
  try {
    const root = execSync('npm root -g', { stdio: ['ignore', 'pipe', 'ignore'], shell: true }).toString().trim();
    scopeDir = path.join(root, '@hanzo');
  } catch {
    // Fall back to guessing from this script location: <staging>\..\..\
    const here = path.resolve(path.dirname(fileURLToPath(import.meta.url)));
    scopeDir = path.resolve(here, '..');
  }
  if (existsSync(scopeDir)) {
    const now = Date.now();
    const maxAgeMs = 2 * 60 * 60 * 1000; // 2 hours
    const currentDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
    for (const name of readdirSync(scopeDir)) {
      if (!name.startsWith('.dev-')) continue;
      const p = path.join(scopeDir, name);
      if (path.resolve(p) === currentDir) continue; // never remove our current dir
      try {
        const st = statSync(p);
        const age = now - st.mtimeMs;
        if (age > maxAgeMs) rmSync(p, { recursive: true, force: true });
      } catch { /* ignore */ }
    }
  }
} catch { /* ignore */ }

process.exit(0);
