#!/usr/bin/env node
// Postinstall script for @hanzo/dev

import { existsSync, mkdirSync, createWriteStream, chmodSync, readFileSync, readSync, writeFileSync, unlinkSync, statSync, openSync, closeSync, copyFileSync, fsyncSync, renameSync, realpathSync } from 'fs';
import { join, dirname, resolve } from 'path';
import { fileURLToPath } from 'url';
import { get } from 'https';
import { platform, arch, tmpdir } from 'os';
import { execSync } from 'child_process';
import { createRequire } from 'module';

const __dirname = dirname(fileURLToPath(import.meta.url));

function getTargetTriple() {
  const platformMap = {
    'darwin': 'apple-darwin',
    'linux': 'unknown-linux-musl',
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
  const dir = join(base, 'hanzo', 'dev', version);
  if (!existsSync(dir)) mkdirSync(dir, { recursive: true });
  return dir;
}

function getCachedBinaryPath(version, targetTriple, isWindows) {
  const ext = isWindows ? '.exe' : '';
  const cacheDir = getCacheDir(version);
  return join(cacheDir, `dev-${targetTriple}${ext}`);
}

function isWSL() {
  if (platform() !== 'linux') return false;
  try {
    const ver = readFileSync('/proc/version', 'utf8').toLowerCase();
    return ver.includes('microsoft') || !!process.env.WSL_DISTRO_NAME;
  } catch { return false; }
}

function isPathOnWindowsFs(p) {
  try {
    const mounts = readFileSync('/proc/mounts', 'utf8').split(/\n/).filter(Boolean);
    let best = { mount: '/', type: 'unknown', len: 1 };
    for (const line of mounts) {
      const parts = line.split(' ');
      if (parts.length < 3) continue;
      const mnt = parts[1];
      const typ = parts[2];
      if (p.startsWith(mnt) && mnt.length > best.len) best = { mount: mnt, type: typ, len: mnt.length };
    }
    return best.type === 'drvfs' || best.type === 'cifs';
  } catch { return false; }
}

async function writeCacheAtomic(srcPath, cachePath) {
  try {
    if (existsSync(cachePath)) {
      const ok = validateDownloadedBinary(cachePath).ok;
      if (ok) return;
    }
  } catch {}
  const dir = dirname(cachePath);
  if (!existsSync(dir)) { try { mkdirSync(dir, { recursive: true }); } catch {} }
  const tmp = cachePath + '.tmp-' + Math.random().toString(36).slice(2, 8);
  copyFileSync(srcPath, tmp);
  try { const fd = openSync(tmp, 'r'); try { fsyncSync(fd); } finally { closeSync(fd); } } catch {}
  const delays = [100, 200, 400, 800, 1200, 1600];
  for (let i = 0; i < delays.length; i++) {
    try {
      if (existsSync(cachePath)) { try { unlinkSync(cachePath); } catch {} }
      renameSync(tmp, cachePath);
      return;
    } catch {
      await new Promise(r => setTimeout(r, delays[i]));
    }
  }
  if (existsSync(cachePath)) { try { unlinkSync(cachePath); } catch {} }
  renameSync(tmp, cachePath);
}

function resolveGlobalBinDir() {
  const plt = platform();
  const userAgent = process.env.npm_config_user_agent || '';

  const fromPrefix = (prefixPath) => {
    if (!prefixPath) return '';
    return plt === 'win32' ? prefixPath : join(prefixPath, 'bin');
  };

  const prefixEnv = process.env.npm_config_prefix || process.env.PREFIX || '';
  const direct = fromPrefix(prefixEnv);
  if (direct) return direct;

  const tryExec = (command) => {
    try {
      return execSync(command, {
        stdio: ['ignore', 'pipe', 'ignore'],
        shell: true,
      }).toString().trim();
    } catch {
      return '';
    }
  };

  const prefixFromNpm = fromPrefix(tryExec('npm prefix -g'));
  if (prefixFromNpm) return prefixFromNpm;

  const binFromNpm = tryExec('npm bin -g');
  if (binFromNpm) return binFromNpm;

  if (userAgent.includes('pnpm')) {
    const pnpmBin = tryExec('pnpm bin --global');
    if (pnpmBin) return pnpmBin;
  }

  if (userAgent.includes('yarn')) {
    const yarnBin = tryExec('yarn global bin');
    if (yarnBin) return yarnBin;
  }

  return '';
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
          const timeoutMs = 30000;

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

export async function runPostinstall(options = {}) {
  const { skipGlobalAlias = false, invokedByRuntime = false } = options;
  if (process.env.DEV_POSTINSTALL_DRY_RUN === '1') {
    return { skipped: true };
  }

  if (invokedByRuntime) {
    process.env.DEV_RUNTIME_POSTINSTALL = process.env.DEV_RUNTIME_POSTINSTALL || '1';
  }

  const ua = process.env.npm_config_user_agent || '';
  const isNpx = ua.includes('npx');
  const isGlobal = process.env.npm_config_global === 'true';

  const targetTriple = getTargetTriple();
  const isWindows = platform() === 'win32';
  const binaryExt = isWindows ? '.exe' : '';

  const binDir = join(__dirname, 'bin');
  if (!existsSync(binDir)) {
    mkdirSync(binDir, { recursive: true });
  }

  const packageJson = JSON.parse(readFileSync(join(__dirname, 'package.json'), 'utf8'));
  const version = packageJson.version;

  // The release produces 'code-*' binaries; we'll download and rename to 'dev-*'
  const binaries = ['code'];

  console.log(`Installing @hanzo/dev v${version} for ${targetTriple}...`);

  for (const binary of binaries) {
    const binaryName = `${binary}-${targetTriple}${binaryExt}`;
    const devBinaryName = `dev-${targetTriple}${binaryExt}`;
    const localPath = join(binDir, devBinaryName);
    const cachePath = getCachedBinaryPath(version, targetTriple, isWindows);

    // Fast path: if a valid cached binary exists, reuse it
    try {
      if (existsSync(cachePath)) {
        const valid = validateDownloadedBinary(cachePath);
        if (valid.ok) {
          const wsl = isWSL();
          const binDirReal = (() => { try { return realpathSync(binDir); } catch { return binDir; } })();
          const mirrorToLocal = !(isWindows || (wsl && isPathOnWindowsFs(binDirReal)));
          if (mirrorToLocal) {
            copyFileSync(cachePath, localPath);
            try { chmodSync(localPath, 0o755); } catch {}
          }
          console.log(`✓ ${devBinaryName} ready from user cache`);
          continue;
        }
      }
    } catch {
      // Ignore cache errors
    }

    // Try platform package via npm optionalDependencies
    const require = createRequire(import.meta.url);
    const platformPkg = (() => {
      const name = (() => {
        if (isWindows) return '@hanzo/dev-win32-x64';
        const plt = platform();
        const cpu = arch();
        if (plt === 'darwin' && cpu === 'arm64') return '@hanzo/dev-darwin-arm64';
        if (plt === 'darwin' && cpu === 'x64') return '@hanzo/dev-darwin-x64';
        if (plt === 'linux' && cpu === 'x64') return '@hanzo/dev-linux-x64-musl';
        if (plt === 'linux' && cpu === 'arm64') return '@hanzo/dev-linux-arm64-musl';
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
        const src = join(platformPkg.dir, 'bin', devBinaryName);
        if (!existsSync(src)) {
          throw new Error(`platform package missing binary: ${platformPkg.name}`);
        }
        await writeCacheAtomic(src, cachePath);
        const wsl = isWSL();
        const binDirReal = (() => { try { return realpathSync(binDir); } catch { return binDir; } })();
        const mirrorToLocal = !(isWindows || (wsl && isPathOnWindowsFs(binDirReal)));
        if (mirrorToLocal) {
          copyFileSync(cachePath, localPath);
          try { chmodSync(localPath, 0o755); } catch {}
        }
        console.log(`✓ Installed ${devBinaryName} from ${platformPkg.name} (cached)`);
        continue;
      } catch (e) {
        console.warn(`⚠ Failed platform package install (${e.message}), falling back to GitHub download`);
      }
    }

    // Download from GitHub release
    const isWin = isWindows;
    const detectedWSL = isWSL();
    const binDirReal = (() => { try { return realpathSync(binDir); } catch { return binDir; } })();
    const mirrorToLocal = !(isWin || (detectedWSL && isPathOnWindowsFs(binDirReal)));
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
    const downloadUrl = `https://github.com/hanzoai/dev/releases/download/v${version}/${archiveName}`;

    console.log(`Downloading ${archiveName}...`);
    try {
      const needsIsolation = isWin || (!isWin && !mirrorToLocal);
      let safeTempDir = needsIsolation ? join(tmpdir(), 'hanzo', 'dev', version) : binDir;
      if (needsIsolation) {
        try {
          if (!existsSync(safeTempDir)) mkdirSync(safeTempDir, { recursive: true });
        } catch {
          try {
            safeTempDir = getCacheDir(version);
            if (!existsSync(safeTempDir)) mkdirSync(safeTempDir, { recursive: true });
          } catch {}
        }
      }
      const tmpPath = join(needsIsolation ? safeTempDir : binDir, `.${archiveName}.part`);
      await downloadBinary(downloadUrl, tmpPath);

      if (isWin) {
        const unzipDest = safeTempDir;
        try {
          const sysRoot = process.env.SystemRoot || process.env.windir || 'C:\\Windows';
          const psFull = join(sysRoot, 'System32', 'WindowsPowerShell', 'v1.0', 'powershell.exe');
          const psCmd = `Expand-Archive -Path '${tmpPath}' -DestinationPath '${unzipDest}' -Force`;
          let ok = false;
          try { execSync(`"${psFull}" -NoProfile -NonInteractive -Command "${psCmd}"`, { stdio: 'ignore' }); ok = true; } catch {}
          if (!ok) { try { execSync(`powershell -NoProfile -NonInteractive -Command "${psCmd}"`, { stdio: 'ignore' }); ok = true; } catch {} }
          if (!ok) { try { execSync(`pwsh -NoProfile -NonInteractive -Command "${psCmd}"`, { stdio: 'ignore' }); ok = true; } catch {} }
          if (!ok) { execSync(`tar -xf "${tmpPath}" -C "${unzipDest}"`, { stdio: 'ignore', shell: true }); }
        } catch (e) {
          throw new Error(`failed to unzip archive: ${e.message}`);
        } finally {
          try { unlinkSync(tmpPath); } catch {}
        }
        try {
          const extractedPath = join(unzipDest, binaryName);
          // Rename code-* to dev-* in cache
          const devCachePath = cachePath;
          await writeCacheAtomic(extractedPath, devCachePath);
          try { unlinkSync(extractedPath); } catch {}
        } catch (e) {
          throw new Error(`failed to move binary to cache: ${e.message}`);
        }
      } else {
        const downloadedPath = join(binDir, binaryName);
        if (useZst) {
          try {
            execSync(`zstd -d '${tmpPath}' -o '${downloadedPath}'`, { stdio: 'ignore', shell: true });
          } catch (e) {
            try { unlinkSync(tmpPath); } catch {}
            throw new Error(`failed to decompress .zst (need zstd CLI): ${e.message}`);
          }
          try { unlinkSync(tmpPath); } catch {}
        } else {
          try {
            execSync(`tar -xzf '${tmpPath}' -C '${binDir}'`, { stdio: 'ignore', shell: true });
          } catch (e) {
            try { unlinkSync(tmpPath); } catch {}
            throw new Error(`failed to extract .tar.gz: ${e.message}`);
          }
          try { unlinkSync(tmpPath); } catch {}
        }
        // Rename code-* to dev-*
        if (existsSync(downloadedPath)) {
          if (mirrorToLocal) {
            renameSync(downloadedPath, localPath);
          } else {
            const extractedPath = downloadedPath;
            await writeCacheAtomic(extractedPath, cachePath);
            try { unlinkSync(extractedPath); } catch {}
          }
        }
      }

      const valid = validateDownloadedBinary(isWin ? cachePath : (mirrorToLocal ? localPath : cachePath));
      if (!valid.ok) {
        try { (isWin || !mirrorToLocal) ? unlinkSync(cachePath) : unlinkSync(localPath); } catch {}
        throw new Error(`invalid binary (${valid.reason})`);
      }

      if (!isWin && mirrorToLocal) {
        chmodSync(localPath, 0o755);
      }

      console.log(`✓ Installed ${devBinaryName}${(isWin || !mirrorToLocal) ? ' (cached)' : ''}`);
      if (!isWin && mirrorToLocal) {
        try { await writeCacheAtomic(localPath, cachePath); } catch {}
      }
    } catch (error) {
      console.error(`✗ Failed to install ${devBinaryName}: ${error.message}`);
      console.error(`  Downloaded from: ${downloadUrl}`);
    }
  }

  const mainBinary = `dev-${targetTriple}${binaryExt}`;
  const mainBinaryPath = join(binDir, mainBinary);

  if (existsSync(mainBinaryPath) || existsSync(getCachedBinaryPath(version, targetTriple, platform() === 'win32'))) {
    try {
      const probePath = existsSync(mainBinaryPath) ? mainBinaryPath : getCachedBinaryPath(version, targetTriple, platform() === 'win32');
      const stats = statSync(probePath);
      if (!stats.size) throw new Error('binary is empty (download likely failed)');
      const valid = validateDownloadedBinary(probePath);
      if (!valid.ok) {
        console.warn(`⚠ Main dev binary appears invalid: ${valid.reason}`);
        console.warn('  Try reinstalling or check your network/proxy settings.');
      }
    } catch (e) {
      console.warn(`⚠ Main dev binary appears invalid: ${e.message}`);
      console.warn('  Try reinstalling or check your network/proxy settings.');
    }
    console.log('✓ Installation complete!');
  } else {
    console.warn('⚠ Main dev binary not found. You may need to build from source.');
  }
}

function isExecutedDirectly() {
  const entry = process.argv[1];
  if (!entry) return false;
  try {
    return resolve(entry) === fileURLToPath(import.meta.url);
  } catch {
    return false;
  }
}

if (isExecutedDirectly()) {
  runPostinstall().catch(error => {
    console.error('Installation failed:', error);
    process.exit(1);
  });
}
