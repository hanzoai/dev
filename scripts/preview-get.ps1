param(
  [Parameter(Mandatory = $true)] [string] $RunId,
  [string] $Repo
)

$ErrorActionPreference = 'Stop'
if (-not $Repo) {
  $Repo = if ($env:GITHUB_REPOSITORY) { $env:GITHUB_REPOSITORY } else { 'just-every/code' }
}

$parts = $Repo.Split('/')
$Owner = $parts[0]
$Name  = $parts[1]

# Detect target
$arch = (Get-CimInstance Win32_Processor).Architecture
if ($arch -ne 9 -and $arch -ne 0) { # 9 = x64, 0 = x86
  Write-Error "Unsupported Windows architecture: $arch"; exit 1
}
$Target = 'x86_64-pc-windows-msvc'

$Work = "code-preview-$Target"
if (Test-Path $Work) { Remove-Item -Recurse -Force $Work }
New-Item -ItemType Directory -Path $Work | Out-Null
Set-Location $Work

function Download-WithGh {
  gh run download $RunId -R "$Owner/$Name" -n "preview-$Target" -D . | Out-Null
}

function Download-WithApi {
  if (-not $env:GH_TOKEN) { Write-Error 'Set GH_TOKEN to a GitHub token with actions:read'; exit 2 }
  $base = "https://api.github.com/repos/$Owner/$Name"
  $hdrs = @{ Authorization = "Bearer $env:GH_TOKEN"; Accept = 'application/vnd.github+json' }
  $arts = Invoke-RestMethod -Uri "$base/actions/runs/$RunId/artifacts?per_page=100" -Headers $hdrs
  $art  = $arts.artifacts | Where-Object { $_.name -eq "preview-$Target" } | Select-Object -First 1
  if (-not $art) { Write-Error "Could not find artifact preview-$Target for run $RunId"; exit 3 }
  Invoke-WebRequest -Uri "$base/actions/artifacts/$($art.id)/zip" -Headers $hdrs -OutFile artifact.zip
  Expand-Archive -Path artifact.zip -DestinationPath . -Force
}

try {
  if (Get-Command gh -ErrorAction SilentlyContinue) { Download-WithGh } else { Download-WithApi }
} catch { Download-WithApi }

# Extract and run
$zip = Get-ChildItem -Filter 'code-*.zip' | Select-Object -First 1
if ($zip) {
  Expand-Archive -Path $zip.FullName -DestinationPath . -Force
}
$exe = Get-ChildItem -Filter 'code-*.exe' | Select-Object -First 1
if (-not $exe) { Write-Error 'No code executable found in artifact.'; exit 4 }
Write-Host "Ready: $($exe.FullName)" 
Write-Host "Launching with --help"
Start-Process -FilePath $exe.FullName -ArgumentList '--help'

