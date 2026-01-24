<#
Helper to recover from EBUSY/EPERM during global npm upgrades on Windows.
Closes running processes and removes stale package folders.

Usage (PowerShell):
  Set-ExecutionPolicy -Scope Process Bypass -Force
  ./dev-cli/scripts/windows-cleanup.ps1
#>

$ErrorActionPreference = 'SilentlyContinue'

Write-Host "Stopping running Hanzo Dev processes..."
taskkill /IM dev-x86_64-pc-windows-msvc.exe /F 2>$null | Out-Null
taskkill /IM dev.exe /F 2>$null | Out-Null
taskkill /IM hanzo.exe /F 2>$null | Out-Null

Write-Host "Removing old global package (if present)..."
$npmRoot = (& npm root -g).Trim()
$pkgPath = Join-Path $npmRoot "@hanzo\dev"
if (Test-Path $pkgPath) {
  try { Remove-Item -LiteralPath $pkgPath -Recurse -Force -ErrorAction Stop } catch {}
}

Write-Host "Removing temp staging directories (if present)..."
Get-ChildItem -LiteralPath (Join-Path $npmRoot "@hanzo") -Force -ErrorAction SilentlyContinue |
  Where-Object { $_.Name -like '.dev-*' } |
  ForEach-Object {
    try { Remove-Item -LiteralPath $_.FullName -Recurse -Force -ErrorAction Stop } catch {}
  }

Write-Host "Cleanup complete. You can now run: npm install -g @hanzo/dev@latest"
