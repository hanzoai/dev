#!/usr/bin/env node
// Non-functional change to trigger release workflow

import { existsSync, mkdirSync, createWriteStream, chmodSync, readFileSync, readSync, writeFileSync, unlinkSync, statSync, openSync, closeSync, copyFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { get } from 'https';
import { platform, arch } from 'os';
import { execSync } from 'child_process';
import { createRequire } from 'module';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Map Node.js platform/arch to Rust target triples
function getTargetTriple() {
  const platformMap = {
    'darwin': 'apple-darwin',
    'linux': 'unknown-linux-musl',  // Default to musl for better compatibility
    'win32': 'pc-windows-msvc'
  };
  
  const archMap = {
    'x64': 'x86_64',
    'arm64': 'aarch64'
  };
  
  const rustArch = archMap[arch()] || arch();
  const rustPlatform = platformMap[platform()] || platform();
  
  return `${rustArch}-${rustPlatform}`;
}

// Resolve a persistent user cache directory for binaries so that repeated
// npx installs can reuse a previously downloaded artifact and skip work.
function getCacheDir(version) {
  const plt = platform();
  const home = process.env.HOME || process.env.USERPROFILE || '';
  let base = '';
  if (plt === 'win32') {
    base = process.env.LOCALAPPDATA || join(home, 'AppData', 'Local');
  } else if (plt === 'darwin') {
    base = join(home, 'Library', 'Caches');
  } else {
    base = process.env.XDG_CACHE_HOME || join(home, '.cache');
  }
  const dir = join(base, 'just-every', 'code', version);
  if (!existsSync(dir)) mkdirSync(dir, { recursive: true });
  return dir;
}

function getCachedBinaryPath(version, targetTriple, isWindows) {
  const ext = isWindows ? '.exe' : '';
  const cacheDir = getCacheDir(version);
  return join(cacheDir, `code-${targetTriple}${ext}`);
}

async function downloadBinary(url, dest, maxRedirects = 5, maxRetries = 3) {
  const sleep = (ms) => new Promise(r => setTimeout(r, ms));

  const doAttempt = () => new Promise((resolve, reject) => {
    const attempt = (currentUrl, redirectsLeft) => {
      const req = get(currentUrl, (response) => {
        const status = response.statusCode || 0;
        const location = response.headers.location;

        if ((status === 301 || status === 302 || status === 303 || status === 307 || status === 308) && location) {
          if (redirectsLeft <= 0) {
            reject(new Error(`Too many redirects while downloading ${currentUrl}`));
            return;
          }
          attempt(location, redirectsLeft - 1);
          return;
        }

        if (status === 200) {
          const expected = parseInt(response.headers['content-length'] || '0', 10) || 0;
          let bytes = 0;
          let timer;
          const timeoutMs = 30000; // 30s inactivity timeout

          const resetTimer = () => {
            if (timer) clearTimeout(timer);
            timer = setTimeout(() => {
              req.destroy(new Error('download stalled'));
            }, timeoutMs);
          };

          resetTimer();
          response.on('data', (chunk) => {
            bytes += chunk.length;
            resetTimer();
          });

          const file = createWriteStream(dest);
          response.pipe(file);
          file.on('finish', () => {
            if (timer) clearTimeout(timer);
            file.close();
            if (expected && bytes !== expected) {
              try { unlinkSync(dest); } catch {}
              reject(new Error(`incomplete download: got ${bytes} of ${expected} bytes`));
            } else if (bytes === 0) {
              try { unlinkSync(dest); } catch {}
              reject(new Error('empty download'));
            } else {
              resolve();
            }
          });
          file.on('error', (err) => {
            if (timer) clearTimeout(timer);
            try { unlinkSync(dest); } catch {}
            reject(err);
          });
        } else {
          reject(new Error(`Failed to download: HTTP ${status}`));
        }
      });

      req.on('error', (err) => {
        try { unlinkSync(dest); } catch {}
        reject(err);
      });

      // Absolute request timeout to avoid hanging forever
      req.setTimeout(120000, () => {
        req.destroy(new Error('download timed out'));
      });
    };

    attempt(url, maxRedirects);
  });

  let attemptNum = 0;
  while (true) {
    try {
      return await doAttempt();
    } catch (e) {
      attemptNum += 1;
      if (attemptNum > maxRetries) throw e;
      const backoff = Math.min(2000, 200 * attemptNum);
      await sleep(backoff);
    }
  }
}

function validateDownloadedBinary(p) {
  try {
    const st = statSync(p);
    if (!st.isFile() || st.size === 0) {
      return { ok: false, reason: 'empty or not a regular file' };
    }
    const fd = openSync(p, 'r');
    try {
      const buf = Buffer.alloc(4);
      const n = readSync(fd, buf, 0, 4, 0);
      if (n < 2) return { ok: false, reason: 'too short' };
      const plt = platform();
      if (plt === 'win32') {
        if (!(buf[0] === 0x4d && buf[1] === 0x5a)) return { ok: false, reason: 'invalid PE header (missing MZ)' };
      } else if (plt === 'linux' || plt === 'android') {
        if (!(buf[0] === 0x7f && buf[1] === 0x45 && buf[2] === 0x4c && buf[3] === 0x46)) return { ok: false, reason: 'invalid ELF header' };
      } else if (plt === 'darwin') {
        const isMachO = (buf[0] === 0xcf && buf[1] === 0xfa && buf[2] === 0xed && buf[3] === 0xfe) ||
                        (buf[0] === 0xca && buf[1] === 0xfe && buf[2] === 0xba && buf[3] === 0xbe);
        if (!isMachO) return { ok: false, reason: 'invalid Mach-O header' };
      }
      return { ok: true };
    } finally {
      closeSync(fd);
    }
  } catch (e) {
    return { ok: false, reason: e.message };
  }
}

async function main() {
  // Detect potential PATH conflict with an existing `code` command (e.g., VS Code)
  // Only relevant for global installs; skip for npx/local installs to keep postinstall fast.
  const ua = process.env.npm_config_user_agent || '';
  const isNpx = ua.includes('npx');
  const isGlobal = process.env.npm_config_global === 'true';
  if (isGlobal && !isNpx) {
    try {
      const whichCmd = process.platform === 'win32' ? 'where code' : 'command -v code || which code || true';
      const resolved = execSync(whichCmd, { stdio: ['ignore', 'pipe', 'ignore'], shell: process.platform !== 'win32' }).toString().split(/\r?\n/).filter(Boolean)[0];
      if (resolved) {
        let contents = '';
        try {
          contents = readFileSync(resolved, 'utf8');
        } catch {
          contents = '';
        }
        const looksLikeOurs = contents.includes('@just-every/code') || contents.includes('bin/coder.js');
        if (!looksLikeOurs) {
          console.warn('[notice] Found an existing `code` on PATH at:');
          console.warn(`         ${resolved}`);
          console.warn('[notice] We will still install our CLI, also available as `coder`.');
          console.warn('         If `code` runs another tool, prefer using: coder');
          console.warn('         Or run our CLI explicitly via: npx -y @just-every/code');
        }
      }
    } catch {
      // Ignore detection failures; proceed with install.
    }
  }

  const targetTriple = getTargetTriple();
  const isWindows = platform() === 'win32';
  const binaryExt = isWindows ? '.exe' : '';
  
  const binDir = join(__dirname, 'bin');
  if (!existsSync(binDir)) {
    mkdirSync(binDir, { recursive: true });
  }
  
  // Get package version - use readFileSync for compatibility
  const packageJson = JSON.parse(readFileSync(join(__dirname, 'package.json'), 'utf8'));
  const version = packageJson.version;
  
  // Download only the primary binary; we'll create wrappers for legacy names.
  const binaries = ['code'];
  
  console.log(`Installing @just-every/code v${version} for ${targetTriple}...`);
  
  for (const binary of binaries) {
    const binaryName = `${binary}-${targetTriple}${binaryExt}`;
    const localPath = join(binDir, binaryName);
    const cachePath = getCachedBinaryPath(version, targetTriple, isWindows);
    
    // Skip if already exists and has correct permissions
    if (existsSync(localPath)) {
      // Always try to fix permissions on Unix-like systems
      if (!isWindows) {
        try {
          chmodSync(localPath, 0o755);
          console.log(`✓ ${binaryName} already exists (permissions fixed)`);
        } catch (e) {
          console.log(`✓ ${binaryName} already exists`);
        }
      } else {
        console.log(`✓ ${binaryName} already exists`);
      }
      continue;
    }

    // Fast path: if a valid cached binary exists for this version+triple, reuse it.
    try {
      if (existsSync(cachePath)) {
        const valid = validateDownloadedBinary(cachePath);
        if (valid.ok) {
          copyFileSync(cachePath, localPath);
          if (!isWindows) chmodSync(localPath, 0o755);
          console.log(`✓ Installed ${binaryName} from user cache`);
          continue; // next binary
        }
      }
    } catch {
      // Ignore cache errors and fall through to normal paths
    }
    
    // First try platform package via npm optionalDependencies (fast path on npm CDN).
    const require = createRequire(import.meta.url);
    const platformPkg = (() => {
      const name = (() => {
        if (isWindows) return '@just-every/code-win32-x64';
        const plt = platform();
        const cpu = arch();
        if (plt === 'darwin' && cpu === 'arm64') return '@just-every/code-darwin-arm64';
        if (plt === 'darwin' && cpu === 'x64') return '@just-every/code-darwin-x64';
        if (plt === 'linux' && cpu === 'x64') return '@just-every/code-linux-x64-musl';
        if (plt === 'linux' && cpu === 'arm64') return '@just-every/code-linux-arm64-musl';
        return null;
      })();
      if (!name) return null;
      try {
        const pkgJsonPath = require.resolve(`${name}/package.json`);
        const pkgDir = dirname(pkgJsonPath);
        return { name, dir: pkgDir };
      } catch {
        return null;
      }
    })();

    if (platformPkg) {
      try {
        // Expect binary inside platform package bin directory
        const src = join(platformPkg.dir, 'bin', binaryName);
        if (!existsSync(src)) {
          throw new Error(`platform package missing binary: ${platformPkg.name}`);
        }
        copyFileSync(src, localPath);
        if (!isWindows) chmodSync(localPath, 0o755);
        console.log(`✓ Installed ${binaryName} from ${platformPkg.name}`);
        // Populate cache for future npx runs
        try {
          if (!existsSync(cachePath)) {
            copyFileSync(localPath, cachePath);
          }
        } catch {}
        continue; // next binary
      } catch (e) {
        console.warn(`⚠ Failed platform package install (${e.message}), falling back to GitHub download`);
      }
    }

    // Decide archive format per OS with fallback on macOS/Linux:
    // - Windows: .zip
    // - macOS/Linux: prefer .zst if `zstd` CLI is available; otherwise use .tar.gz
    const isWin = isWindows;
    let useZst = false;
    if (!isWin) {
      try {
        execSync('zstd --version', { stdio: 'ignore', shell: true });
        useZst = true;
      } catch {
        useZst = false;
      }
    }
    const archiveName = isWin ? `${binaryName}.zip` : (useZst ? `${binaryName}.zst` : `${binaryName}.tar.gz`);
    const downloadUrl = `https://github.com/just-every/code/releases/download/v${version}/${archiveName}`;

    console.log(`Downloading ${archiveName}...`);
    try {
      const tmpPath = join(binDir, `.${archiveName}.part`);
      await downloadBinary(downloadUrl, tmpPath);

      if (isWin) {
        // Unzip the single-file archive using PowerShell (built-in)
        try {
          const psCmd = `powershell -NoProfile -NonInteractive -Command "Expand-Archive -Path '${tmpPath}' -DestinationPath '${binDir}' -Force"`;
          execSync(psCmd, { stdio: 'ignore' });
        } catch (e) {
          throw new Error(`failed to unzip archive: ${e.message}`);
        } finally {
          try { unlinkSync(tmpPath); } catch {}
        }
      } else {
        if (useZst) {
          // Decompress .zst via system zstd
          try {
            execSync(`zstd -d '${tmpPath}' -o '${localPath}'`, { stdio: 'ignore', shell: true });
          } catch (e) {
            try { unlinkSync(tmpPath); } catch {}
            throw new Error(`failed to decompress .zst (need zstd CLI): ${e.message}`);
          }
          try { unlinkSync(tmpPath); } catch {}
        } else {
          // Extract .tar.gz using system tar
          try {
            execSync(`tar -xzf '${tmpPath}' -C '${binDir}'`, { stdio: 'ignore', shell: true });
          } catch (e) {
            try { unlinkSync(tmpPath); } catch {}
            throw new Error(`failed to extract .tar.gz: ${e.message}`);
          }
          try { unlinkSync(tmpPath); } catch {}
        }
      }

      // Validate header to avoid corrupt binaries causing spawn EFTYPE/ENOEXEC
      const valid = validateDownloadedBinary(localPath);
      if (!valid.ok) {
        try { unlinkSync(localPath); } catch {}
        throw new Error(`invalid binary (${valid.reason})`);
      }

      // Make executable on Unix-like systems
      if (!isWindows) {
        chmodSync(localPath, 0o755);
      }
      
      console.log(`✓ Installed ${binaryName}`);
      // Save into persistent cache for future fast installs
      try {
        copyFileSync(localPath, cachePath);
      } catch {}
    } catch (error) {
      console.error(`✗ Failed to install ${binaryName}: ${error.message}`);
      console.error(`  Downloaded from: ${downloadUrl}`);
      // Continue with other binaries even if one fails
    }
  }

  // Create platform-specific symlink/copy for main binary
  const mainBinary = `code-${targetTriple}${binaryExt}`;
  const mainBinaryPath = join(binDir, mainBinary);
  
  if (existsSync(mainBinaryPath)) {
    try {
      const stats = statSync(mainBinaryPath);
      if (!stats.size) {
        throw new Error('binary is empty (download likely failed)');
      }
      const valid = validateDownloadedBinary(mainBinaryPath);
      if (!valid.ok) {
        console.warn(`⚠ Main dev binary appears invalid: ${valid.reason}`);
        console.warn('  Try reinstalling or check your network/proxy settings.');
      }
    } catch (e) {
      console.warn(`⚠ Main dev binary appears invalid: ${e.message}`);
      console.warn('  Try reinstalling or check your network/proxy settings.');
    }
    console.log('Setting up main dev binary...');
    
    // On Windows, we can't use symlinks easily, so update the JS wrapper
    // On Unix, the JS wrapper will find the correct binary
    console.log('✓ Installation complete!');
  } else {
    console.warn('⚠ Main code binary not found. You may need to build from source.');
  }

  // Handle collisions (e.g., VS Code) and add wrappers. We no longer publish a
  // `code` bin in package.json. Instead, for global installs we create a `code`
  // wrapper only when there is no conflicting `code` earlier on PATH. This avoids
  // hijacking the VS Code CLI while still giving users a friendly name when safe.
  // For upgrades from older versions that published a `code` bin, we also remove
  // our old shim if a conflict is detected.
  if (isGlobal && !isNpx) try {
    const isTTY = process.stdout && process.stdout.isTTY;
    const isWindows = platform() === 'win32';
    const ua = process.env.npm_config_user_agent || '';
    const isBun = ua.includes('bun') || !!process.env.BUN_INSTALL;

    const installedCmds = new Set(['coder']); // global install always exposes coder via package manager
    const skippedCmds = [];

    // Helper to resolve all 'code' on PATH
    const resolveAllOnPath = () => {
      try {
        if (isWindows) {
          const out = execSync('where code', { stdio: ['ignore', 'pipe', 'ignore'] }).toString();
          return out.split(/\r?\n/).map(s => s.trim()).filter(Boolean);
        }
        let out = '';
        try {
          out = execSync('bash -lc "which -a code 2>/dev/null"', { stdio: ['ignore', 'pipe', 'ignore'] }).toString();
        } catch {
          try {
            out = execSync('command -v code || true', { stdio: ['ignore', 'pipe', 'ignore'] }).toString();
          } catch { out = ''; }
        }
        return out.split(/\r?\n/).map(s => s.trim()).filter(Boolean);
      } catch {
        return [];
      }
    };

    if (isBun) {
      // Bun creates shims for every bin; if another 'code' exists elsewhere on PATH, remove Bun's shim
      let bunBin = '';
      try {
        const home = process.env.HOME || process.env.USERPROFILE || '';
        const bunBase = process.env.BUN_INSTALL || join(home, '.bun');
        bunBin = join(bunBase, 'bin');
      } catch {}

      const bunShim = join(bunBin || '', isWindows ? 'code.cmd' : 'code');
      const candidates = resolveAllOnPath();
      const other = candidates.find(p => p && (!bunBin || !p.startsWith(bunBin)));
      if (other && existsSync(bunShim)) {
        try {
          unlinkSync(bunShim);
          console.log(`✓ Skipped global 'code' shim under Bun (existing: ${other})`);
          skippedCmds.push({ name: 'code', reason: `existing: ${other}` });
        } catch (e) {
          console.log(`⚠ Could not remove Bun shim '${bunShim}': ${e.message}`);
        }
      } else if (!other) {
        // No conflict: create a wrapper that forwards to `coder`
        try {
          const wrapperPath = bunShim;
          if (isWindows) {
            const content = `@echo off\r\n"%~dp0coder" %*\r\n`;
            writeFileSync(wrapperPath, content);
          } else {
            const content = `#!/bin/sh\nexec "$(dirname \"$0\")/coder" "$@"\n`;
            writeFileSync(wrapperPath, content);
            chmodSync(wrapperPath, 0o755);
          }
          console.log("✓ Created 'code' wrapper -> coder (bun)");
          installedCmds.add('code');
        } catch (e) {
          console.log(`⚠ Failed to create 'code' wrapper (bun): ${e.message}`);
        }
      }

      // Print summary for Bun
      const list = Array.from(installedCmds).sort().join(', ');
      console.log(`Commands installed (bun): ${list}`);
      if (skippedCmds.length) {
        for (const s of skippedCmds) console.error(`Commands skipped: ${s.name} (${s.reason})`);
        console.error('→ Use `coder` to run this tool.');
      }
      // Final friendly usage hint
      if (installedCmds.has('code')) {
        console.log("Use 'dev' to launch Hanzo Dev.");
      } else {
        console.log("Use 'dev' to launch Hanzo Dev.");
      }
    } else {
      // npm/pnpm/yarn path
      let globalBin = '';
      try {
        globalBin = execSync('npm bin -g', { stdio: ['ignore', 'pipe', 'ignore'] }).toString().trim();
      } catch {}

      const ourShim = join(globalBin || '', isWindows ? 'code.cmd' : 'code');
      const candidates = resolveAllOnPath();
      const others = candidates.filter(p => p && (!ourShim || p !== ourShim));
      const collision = others.length > 0;

      const ensureWrapper = (name, args) => {
        if (!globalBin) return;
        try {
          const wrapperPath = join(globalBin, isWindows ? `${name}.cmd` : name);
          if (isWindows) {
            const content = `@echo off\r\n"%~dp0${collision ? 'coder' : 'code'}" ${args} %*\r\n`;
            writeFileSync(wrapperPath, content);
          } else {
            const content = `#!/bin/sh\nexec "$(dirname \"$0\")/${collision ? 'coder' : 'code'}" ${args} "$@"\n`;
            writeFileSync(wrapperPath, content);
            chmodSync(wrapperPath, 0o755);
          }
          console.log(`✓ Created wrapper '${name}' -> ${collision ? 'coder' : 'code'} ${args}`);
          installedCmds.add(name);
        } catch (e) {
          console.log(`⚠ Failed to create '${name}' wrapper: ${e.message}`);
        }
      };

      // Always create legacy wrappers so existing scripts keep working
      ensureWrapper('code-tui', '');
      ensureWrapper('code-exec', 'exec');

      if (collision) {
        console.error('⚠ Detected existing `code` on PATH:');
        for (const p of others) console.error(`   - ${p}`);
        if (globalBin) {
          try {
            if (existsSync(ourShim)) {
              unlinkSync(ourShim);
              console.error(`✓ Skipped global 'code' shim (removed ${ourShim})`);
              skippedCmds.push({ name: 'code', reason: `existing: ${others[0]}` });
            }
          } catch (e) {
            console.error(`⚠ Could not remove npm shim '${ourShim}': ${e.message}`);
          }
          console.error('→ Use `coder` to run this tool.');
        } else {
          console.log('Note: could not determine npm global bin; skipping alias creation.');
        }
      } else {
        // No collision; ensure a 'code' wrapper exists forwarding to 'coder'
        if (globalBin) {
          try {
            const content = isWindows
              ? `@echo off\r\n"%~dp0coder" %*\r\n`
              : `#!/bin/sh\nexec "$(dirname \"$0\")/coder" "$@"\n`;
            writeFileSync(ourShim, content);
            if (!isWindows) chmodSync(ourShim, 0o755);
            console.log("✓ Created 'code' wrapper -> coder");
            installedCmds.add('code');
          } catch (e) {
            console.log(`⚠ Failed to create 'code' wrapper: ${e.message}`);
          }
        }
      }

      // Print summary for npm/pnpm/yarn
      const list = Array.from(installedCmds).sort().join(', ');
      console.log(`Commands installed: ${list}`);
      if (skippedCmds.length) {
        for (const s of skippedCmds) console.log(`Commands skipped: ${s.name} (${s.reason})`);
      }
      // Final friendly usage hint
      if (installedCmds.has('code')) {
        console.log("Use 'dev' to launch Hanzo Dev.");
      } else {
        console.log("Use 'dev' to launch Hanzo Dev.");
      }
    }
  } catch {
    // non-fatal
  }
}

main().catch(error => {
  console.error('Installation failed:', error);
  process.exit(1);
});
