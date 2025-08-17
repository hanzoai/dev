#!/usr/bin/env node

import { existsSync, mkdirSync, createWriteStream, chmodSync, readFileSync, readSync, writeFileSync } from 'fs';
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

async function downloadBinary(url, dest) {
  return new Promise((resolve, reject) => {
    const file = createWriteStream(dest);
    
    get(url, (response) => {
      if (response.statusCode === 302 || response.statusCode === 301) {
        // Follow redirect
        get(response.headers.location, (redirectResponse) => {
          redirectResponse.pipe(file);
          file.on('finish', () => {
            file.close();
            resolve();
          });
        }).on('error', reject);
      } else if (response.statusCode === 200) {
        response.pipe(file);
        file.on('finish', () => {
          file.close();
          resolve();
        });
      } else {
        reject(new Error(`Failed to download: ${response.statusCode}`));
      }
    }).on('error', reject);
  });
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
  
  // Binary names to download (Rust artifacts remain named 'coder*')
  const binaries = ['coder', 'coder-tui', 'coder-exec'];
  
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
  const mainBinary = `coder-${targetTriple}${binaryExt}`;
  const mainBinaryPath = join(binDir, mainBinary);
  
  if (existsSync(mainBinaryPath)) {
    console.log('Setting up main coder binary...');
    
    // On Windows, we can't use symlinks easily, so update the JS wrapper
    // On Unix, the JS wrapper will find the correct binary
    console.log('✓ Installation complete!');
  } else {
    console.warn('⚠ Main coder binary not found. You may need to build from source.');
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
