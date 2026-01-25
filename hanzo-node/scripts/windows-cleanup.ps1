# Windows cleanup script for hanzo-node
# Removes any leftover temporary files from failed installations

$binDir = Join-Path $PSScriptRoot ".." "bin"

if (Test-Path $binDir) {
    Get-ChildItem -Path $binDir -Filter "*.part" | Remove-Item -Force -ErrorAction SilentlyContinue
    Get-ChildItem -Path $binDir -Filter "*.tmp" | Remove-Item -Force -ErrorAction SilentlyContinue
}

Write-Host "hanzo-node: cleanup complete"
