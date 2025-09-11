param(
  [string]$Version = "latest",
  [switch]$FixCache
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Step($msg) { Write-Host ("---- " + $msg) -ForegroundColor Cyan }
function Info($msg) { Write-Host $msg }
function Warn($msg) { Write-Host $msg -ForegroundColor Yellow }
function Err ($msg) { Write-Host $msg -ForegroundColor Red }

# 1) Basics
Step "Environment"
$envs = [ordered]@{
  "Node" = (node -v 2>$null)
  "npm"  = (npm -v 2>$null)
  "PowerShell" = $PSVersionTable.PSVersion.ToString()
  "OSArch" = $env:PROCESSOR_ARCHITECTURE
  "APPDATA" = $env:APPDATA
  "LOCALAPPDATA" = $env:LOCALAPPDATA
}
$envs.GetEnumerator() | ForEach-Object { Info ("{0} = {1}" -f $_.Key, $_.Value) }

Step "npm config (registry/proxy/flags)"
try {
  Info ("registry = " + (npm config get registry))
  Info ("ignore-scripts = " + (npm config get ignore-scripts))
  Info ("strict-ssl = " + (npm config get strict-ssl))
  $hp = npm config get https-proxy; if ($hp -ne "null") { Info ("https-proxy = " + $hp) }
  $p  = npm config get proxy;       if ($p  -ne "null") { Info ("proxy = " + $p) }
} catch { Warn ("npm config read failed: " + $_) }

# 2) Resolve versions and tarballs from NPM
Step "Resolve @just-every/code@$Version (npm)"
$mainVersion = $null
try { $mainVersion = (npm view "@just-every/code@$Version" version) } catch {}
if (-not $mainVersion) { Err "Could not resolve @just-every/code@$Version"; exit 1 }
$mainTar = $null
try { $mainTar = (npm view "@just-every/code@$mainVersion" dist.tarball) } catch {}
Info ("main version = " + $mainVersion)
Info ("main tarball = " + ($mainTar ?? "<none>"))

Step "Resolve @just-every/code-win32-x64@$mainVersion (npm)"
$winVersion = $null; $winTar = $null
try { $winVersion = (npm view "@just-every/code-win32-x64@$mainVersion" version) } catch {}
if ($winVersion) {
  try { $winTar = (npm view "@just-every/code-win32-x64@$mainVersion" dist.tarball) } catch {}
  Info ("win version  = " + $winVersion)
  Info ("win tarball  = " + ($winTar ?? "<none>"))
} else {
  Warn ("@just-every/code-win32-x64@$mainVersion not found on npm")
}

# 3) Download + inspect npm tarballs
$tmp = New-Item -ItemType Directory -Path (Join-Path $env:TEMP ("code-diag-" + [guid]::NewGuid().ToString("N"))) -Force
Step ("Work dir: " + $tmp.FullName)
Set-Location $tmp.FullName

function Fetch($url, $dst) {
  if (-not $url) { return $false }
  try {
    Invoke-WebRequest -UseBasicParsing -Uri $url -OutFile $dst -TimeoutSec 120
    return $true
  } catch {
    Err ("Download failed: " + $url)
    Err ($_.Exception.Message)
    return $false
  }
}

$t1 = Join-Path $pwd "code.tgz"
$t2 = Join-Path $pwd "win.tgz"
$gotMain = Fetch $mainTar $t1
$gotWin = $false
if ($winTar) { $gotWin = Fetch $winTar $t2 }

if ($gotMain) {
  try {
    mkdir p1 | Out-Null
    tar -xzf $t1 -C p1
    Step ("@just-every/code@" + $mainVersion + " contents")
    dir p1/package
    Info "\nHead of bin/coder.js:"
    Get-Content p1/package/bin/coder.js -TotalCount 30 | ForEach-Object { "  " + $_ }
    $isESM = Select-String -Path p1/package/package.json -Pattern '"type"\s*:\s*"module"' -Quiet
    Info ("package.json type=module? " + [string]$isESM)
  } catch {
    Warn ("Could not extract or inspect main tarball: " + $_)
  }
} else {
  Err "Could not fetch main tarball from npm"
}

if ($gotWin) {
  try {
    mkdir p2 | Out-Null
    tar -xzf $t2 -C p2
    Step ("@just-every/code-win32-x64@" + $mainVersion + " contents")
    dir p2/package/bin
  } catch {
    Warn ("Could not extract or inspect win tarball: " + $_)
  }
}

# 4) Compute target triple and cache paths
$triple = "x86_64-pc-windows-msvc.exe"  # Windows x64
$cacheDir = Join-Path $env:LOCALAPPDATA ("just-every\code\" + $mainVersion)
$expectOK  = Join-Path $cacheDir ("code-" + $triple)   # correct
$expectBAD = $expectOK + ".exe"                          # incorrect (double .exe)
Step "Cache expectations"
Info ("cache dir  = " + $cacheDir)
Info ("expect OK  = " + $expectOK)
Info ("expect BAD = " + $expectBAD)
Info ("exists OK?  " + ([string](Test-Path $expectOK)))
Info ("exists BAD? " + ([string](Test-Path $expectBAD)))

# 5) GitHub release asset (fallback path)
$ghZip = "https://github.com/just-every/code/releases/download/v$mainVersion/code-$triple.zip"
Step "GitHub asset (fallback) HEAD check"
try {
  $resp = Invoke-WebRequest -UseBasicParsing -Uri $ghZip -Method Head
  Info ("GitHub asset reachable? " + [string]$resp.StatusCode)
} catch {
  Warn ("GitHub HEAD failed: " + $_)
}

# Optional: download and test unzip to cache
$zipTmp = Join-Path $pwd ("code-" + $triple + ".zip")
$unzipOk = $false
$existedBefore = Test-Path $expectOK
try {
  if (Fetch $ghZip $zipTmp) {
    Step "Test Expand-Archive to cache dir"
    if (-not (Test-Path $cacheDir)) { New-Item -ItemType Directory -Path $cacheDir | Out-Null }
    $sysRoot = $env:SystemRoot
    $psFull  = Join-Path $sysRoot "System32\WindowsPowerShell\v1.0\powershell.exe"
    $psCmd   = "Expand-Archive -Path `"$zipTmp`" -DestinationPath `"$cacheDir`" -Force"
    $ok = $false
    try { & "$psFull" -NoProfile -NonInteractive -Command $psCmd 2>$null; $ok = $true } catch {}
    if (-not $ok) { try { powershell -NoProfile -NonInteractive -Command $psCmd 2>$null; $ok = $true } catch {} }
    if (-not $ok) { try { pwsh -NoProfile -NonInteractive -Command $psCmd 2>$null; $ok = $true } catch {} }
    if (-not $ok) { try { tar -xf $zipTmp -C $cacheDir; $ok = $true } catch {} }
    $unzipOk = $ok
    Info ("unzip ok? " + [string]$unzipOk)
    Info ("post-unzip OK exists?  " + ([string](Test-Path $expectOK)))
    Info ("post-unzip BAD exists? " + ([string](Test-Path $expectBAD)))
  }
} catch {
  Warn ("Unzip test failed: " + $_)
} finally {
  try { Remove-Item -Force $zipTmp } catch {}
  if (-not $FixCache -and -not $existedBefore) {
    try { if (Test-Path $expectOK) { Remove-Item -Force $expectOK } } catch {}
  }
}

# 6) Global locations that the error messages referenced
Step "Global npm prefix + expected node_modules bin path"
$prefix = $null
try { $prefix = (npm prefix -g 2>$null) } catch {}
$nmBin  = if ($prefix) { Join-Path $prefix "node_modules\@just-every\code\bin" } else { "<unknown>" }
Info ("prefix = " + ($prefix ?? "<none>"))
Info ("bin dir= " + $nmBin)
if ($prefix -and (Test-Path $nmBin)) { dir $nmBin } else { Info "(bin dir not present - expected for npx, present for global installs)" }

# 7) Summary
Step "RESULTS SUMMARY"
$lines = @()
$lines += ("main version: " + $mainVersion)
$lines += ("platform pkg present on npm: " + ([string]([bool]$winVersion)))
$lines += ("cache ok path exists: " + ([string](Test-Path $expectOK)))
$lines += ("cache bad path exists: " + ([string](Test-Path $expectBAD)))
$lines += ("github asset unzip ok: " + ([string]$unzipOk))
$esm = $false; try { $esm = Select-String -Path p1/package/package.json -Pattern '"type"\s*:\s*"module"' -Quiet } catch {}
$lines += ("esm bin (type:module) in npm tarball: " + ([string]$esm))
$lines | ForEach-Object { Info $_ }

Info ""
Info "Done. Paste the RESULTS SUMMARY back if anything looks off."

