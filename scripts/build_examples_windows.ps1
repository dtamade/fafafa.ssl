Param(
  [string]$Target = "",
  [switch]$Clean = $false
)

$ErrorActionPreference = "Continue"
Write-Host "=== Build Examples (Windows) ===" -ForegroundColor Cyan

if (-not (Get-Command fpc -ErrorAction SilentlyContinue)) {
  Write-Host "fpc not found in PATH" -ForegroundColor Yellow
}

$Root = (Get-Location).Path
$ExDir = Join-Path $Root "examples"
$Out = Join-Path $ExDir "bin-win"
New-Item -ItemType Directory -Force -Path $Out | Out-Null

if ($Clean) {
  Get-ChildItem $Out -Recurse -Force -ErrorAction SilentlyContinue | Remove-Item -Force -Recurse -ErrorAction SilentlyContinue
}

$units = @("-Fusrc","-Fusrc\openssl","-Fusrc\winssl")

function Build-One($path) {
  $name = [IO.Path]::GetFileNameWithoutExtension($path)
  $exe = Join-Path $Out ($name + ".exe")
  Write-Host "[BUILD] $name" -ForegroundColor Yellow
  & fpc @units -FE:$Out $path | Out-Null
  if ($LASTEXITCODE -ne 0 -or -not (Test-Path $exe)) {
    Write-Host "  -> FAIL" -ForegroundColor Red
    return $false
  }
  Write-Host "  -> OK" -ForegroundColor Green
  return $true
}

$ok = $true
$files = if ($Target -ne "") { Get-ChildItem $ExDir -Recurse -Include $Target } else { Get-ChildItem $ExDir -Recurse -Include *.pas,*.lpr }
foreach ($f in $files) {
  if ($f.Extension -in (".pas",".lpr")) {
    $ok = (Build-One $f.FullName) -and $ok
  }
}

if (-not $ok) { exit 1 } else { exit 0 }


