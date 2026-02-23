param(
    [string]$Compiler = "gcc"
)

$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$src = Join-Path $scriptDir "src\\bern_http.c"
$outDir = Join-Path $scriptDir "windows"
$outDll = Join-Path $outDir "bern_http.dll"

New-Item -ItemType Directory -Force -Path $outDir | Out-Null

& $Compiler "-shared" "-O2" $src "-o" $outDll "-lws2_32"
if ($LASTEXITCODE -ne 0) {
    throw "Build failed: $outDll"
}

Write-Host "Built $outDll"
