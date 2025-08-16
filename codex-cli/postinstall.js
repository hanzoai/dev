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
  
  // Binary names to download
  const binaries = ['coder', 'coder-tui', 'coder-exec'];
  
  console.log(`Installing @just-every/coder v${version} for ${targetTriple}...`);
  
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

  // Optional: offer a 'code' alias if safe. Do not override existing 'code'.
  try {
    const isTTY = process.stdout && process.stdout.isTTY;
    // Best-effort detection of global install
    const isGlobal = process.env.npm_config_global === 'true' || (() => {
      try {
        const p = execSync('npm bin -g', { stdio: ['ignore', 'pipe', 'ignore'] }).toString().trim();
        return p.length > 0;
      } catch { return false; }
    })();

    // Resolve global bin dir
    let globalBin = '';
    try {
      globalBin = execSync('npm bin -g', { stdio: ['ignore', 'pipe', 'ignore'] }).toString().trim();
    } catch {}

    // Helper: does a command exist on PATH
    const commandExists = (cmd) => {
      try {
        if (platform() === 'win32') {
          execSync(`where ${cmd}`, { stdio: 'ignore' });
        } else {
          execSync(`command -v ${cmd}`, { stdio: 'ignore' });
        }
        return true;
      } catch { return false; }
    };

    const codeOnPath = commandExists('code');

    if (isGlobal && isTTY) {
      if (codeOnPath) {
        console.log('⚠ Detected an existing `code` command on your PATH (e.g., VS Code).');
        console.log('   We will keep the CLI command name as `coder` to avoid collisions.');
        // Optionally, offer to create a shell alias suggestion
        console.log('   Tip: add `alias code=coder` to your shell rc if you prefer.');
      } else if (globalBin) {
        // Offer to create a non-conflicting `code` shim that invokes coder.js
        const prompt = (msg) => {
          process.stdout.write(msg);
          try {
            const buf = Buffer.alloc(1024);
            const bytes = readSync(0, buf, 0, 1024, null);
            const ans = buf.slice(0, bytes).toString('utf8').trim().toLowerCase();
            return ans;
          } catch { return 'n'; }
        };
        const answer = prompt('No existing `code` found. Create a `code` alias for this tool? [y/N] ');
        if (answer === 'y' || answer === 'yes') {
          try {
            const shimPath = join(globalBin, platform() === 'win32' ? 'code.cmd' : 'code');
            if (platform() === 'win32') {
              // cmd shim that forwards to existing `coder`
              const content = `@echo off\r\n"%~dp0coder" %*\r\n`;
              writeFileSync(shimPath, content);
            } else {
              const content = `#!/bin/sh\nexec "$(dirname \"$0\")/coder" "$@"\n`;
              writeFileSync(shimPath, content);
              chmodSync(shimPath, 0o755);
            }
            console.log(`✓ Created \`${shimPath}\` that forwards to \`coder\``);
          } catch (e) {
            console.log(`⚠ Failed to create 'code' alias automatically: ${e.message}`);
            console.log('   You can add `alias code=coder` to your shell instead.');
          }
        } else {
          console.log('Skipping `code` alias creation. Use `coder` to run the CLI.');
        }
      }
    } else {
      // Non-interactive or local install; just print guidance
      if (!codeOnPath) {
        console.log('Note: You can add an alias `code=coder` in your shell if desired.');
      }
    }
  } catch {
    // Non-fatal; continue silently
  }
}

main().catch(error => {
  console.error('Installation failed:', error);
  process.exit(1);
});
