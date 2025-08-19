#!/usr/bin/env node
// Non-functional change to trigger release workflow

import { existsSync, mkdirSync, createWriteStream, chmodSync, readFileSync, readSync, writeFileSync, unlinkSync, statSync, openSync, closeSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { get } from 'https';
import { platform, arch } from 'os';
import { execSync } from 'child_process';

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

async function downloadBinary(url, dest, maxRedirects = 5) {
  return new Promise((resolve, reject) => {
    const attempt = (currentUrl, redirectsLeft) => {
      get(currentUrl, (response) => {
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
          // Only create the file stream after we know it's a successful response
          const file = createWriteStream(dest);
          response.pipe(file);
          file.on('finish', () => {
            file.close();
            resolve();
          });
          file.on('error', (err) => {
            try { unlinkSync(dest); } catch {}
            reject(err);
          });
        } else {
          reject(new Error(`Failed to download: HTTP ${status}`));
        }
      }).on('error', (err) => {
        try { unlinkSync(dest); } catch {}
        reject(err);
      });
    };

    attempt(url, maxRedirects);
  });
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
  
  // Binary names to download (new naming is 'code*')
  const binaries = ['code', 'code-tui', 'code-exec'];
  
  console.log(`Installing @just-every/code v${version} for ${targetTriple}...`);
  
  for (const binary of binaries) {
    const binaryName = `${binary}-${targetTriple}${binaryExt}`;
    const localPath = join(binDir, binaryName);
    
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
    
    const downloadUrl = `https://github.com/just-every/code/releases/download/v${version}/${binaryName}`;
    
    console.log(`Downloading ${binaryName}...`);
    try {
      await downloadBinary(downloadUrl, localPath);

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
      
      console.log(`✓ Downloaded ${binaryName}`);
    } catch (error) {
      console.error(`✗ Failed to download ${binaryName}: ${error.message}`);
      console.error(`  URL: ${downloadUrl}`);
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
        console.warn(`⚠ Main code binary appears invalid: ${valid.reason}`);
        console.warn('  Try reinstalling or check your network/proxy settings.');
      }
    } catch (e) {
      console.warn(`⚠ Main code binary appears invalid: ${e.message}`);
      console.warn('  Try reinstalling or check your network/proxy settings.');
    }
    console.log('Setting up main code binary...');
    
    // On Windows, we can't use symlinks easily, so update the JS wrapper
    // On Unix, the JS wrapper will find the correct binary
    console.log('✓ Installation complete!');
  } else {
    console.warn('⚠ Main code binary not found. You may need to build from source.');
  }

  // With bin name = 'code', handle collisions with existing 'code' (e.g., VS Code)
  try {
    const isTTY = process.stdout && process.stdout.isTTY;
    const isWindows = platform() === 'win32';

    let globalBin = '';
    try {
      globalBin = execSync('npm bin -g', { stdio: ['ignore', 'pipe', 'ignore'] }).toString().trim();
    } catch {}

    const ourShim = join(globalBin || '', isWindows ? 'code.cmd' : 'code');

    // Resolve which 'code' is currently on PATH
    const resolveOnPath = (cmd) => {
      try {
        if (isWindows) {
          const out = execSync(`where ${cmd}`, { stdio: ['ignore', 'pipe', 'ignore'] }).toString().split(/\r?\n/)[0]?.trim();
          return out || '';
        } else {
          return execSync(`command -v ${cmd}`, { stdio: ['ignore', 'pipe', 'ignore'] }).toString().trim();
        }
      } catch { return ''; }
    };

    const codeResolved = resolveOnPath('code');
    const collision = codeResolved && ourShim && codeResolved !== ourShim;

    if (collision) {
      console.log('⚠ Detected an existing `code` command on your PATH (likely VS Code).');
      if (globalBin) {
        // Create a 'coder' shim that forwards to our installed 'code' in the same dir
        try {
          const coderShim = join(globalBin, isWindows ? 'coder.cmd' : 'coder');
          if (isWindows) {
            const content = `@echo off\r\n"%~dp0code" %*\r\n`;
            writeFileSync(coderShim, content);
          } else {
            const content = `#!/bin/sh\nexec "$(dirname \"$0\")/code" "$@"\n`;
            writeFileSync(coderShim, content);
            chmodSync(coderShim, 0o755);
          }
          console.log(`✓ Created fallback command \`coder\` -> our \`code\``);
        } catch (e) {
          console.log(`⚠ Failed to create 'coder' fallback: ${e.message}`);
        }

        // Offer to create a 'vscode' alias that points to the existing system VS Code
        if (isTTY && codeResolved) {
          const prompt = (msg) => {
            process.stdout.write(msg);
            try {
              const buf = Buffer.alloc(1024);
              const bytes = readSync(0, buf, 0, 1024, null);
              const ans = buf.slice(0, bytes).toString('utf8').trim().toLowerCase();
              return ans;
            } catch { return 'n'; }
          };
          const ans = prompt('Create a `vscode` alias for your existing editor? [y/N] ');
          if (ans === 'y' || ans === 'yes') {
            try {
              const vscodeShim = join(globalBin, isWindows ? 'vscode.cmd' : 'vscode');
              if (isWindows) {
                const content = `@echo off\r\n"${codeResolved}" %*\r\n`;
                writeFileSync(vscodeShim, content);
              } else {
                const content = `#!/bin/sh\nexec "${codeResolved}" "$@"\n`;
                writeFileSync(vscodeShim, content);
                chmodSync(vscodeShim, 0o755);
              }
              console.log('✓ Created `vscode` alias for your editor');
            } catch (e) {
              console.log(`⚠ Failed to create 'vscode' alias: ${e.message}`);
            }
          } else {
            console.log('Skipping creation of `vscode` alias.');
          }
        }

        console.log('→ Use `coder` to run this tool, and `vscode` (if created) for your editor.');
      } else {
        console.log('Note: could not determine npm global bin; skipping alias creation.');
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
