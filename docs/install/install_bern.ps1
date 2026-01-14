<#
.SYNOPSIS
    Installs the Bern programming language from GitHub to C:\Bern and adds it to the user PATH.
.DESCRIPTION
    Downloads the latest Bern.exe release from GitHub, places it in C:\Bern, and updates the user PATH.
#>

$ErrorActionPreference = 'Stop'

$installDir = "C:\Bern"
$repo = "mirvoxtm/Bern"
 $zipName = "Bern_win_1.0.1.zip"
 $exeName = "Bern.exe"
 $zipPath = Join-Path $installDir $zipName
 $exePath = Join-Path $installDir $exeName




# Bern REPL-style banner
$banner = @(
    "______                  ",
    "| ___ \                       The Bern Language Installer ",
    "| |_/ / ___ _ __ _ __       ",
    "| ___ \/ _ \ '__| '_ \      This script will install Bern on",
    "| |_/ /  __/ |  | | | |                 your machine",
    "\____/ \___|_|  |_| |_|         [ v.1.0.1 12.01.2025 ]"
)
foreach ($line in $banner) {
    Write-Host $line -ForegroundColor Magenta
}
Write-Host ""
Write-Host "--------------------------------------------------------------" -ForegroundColor DarkGray
Write-Host "" 
Write-Host "[bern] Installing to $installDir..." -ForegroundColor Cyan

# Create install directory if it doesn't exist
if (!(Test-Path $installDir)) {
    New-Item -ItemType Directory -Path $installDir | Out-Null
}

# Get latest release info from GitHub
$releaseUrl = "https://api.github.com/repos/$repo/releases/latest"
Write-Host "[bern] Fetching latest release info from GitHub..." -ForegroundColor Cyan
$release = Invoke-RestMethod -Uri $releaseUrl

Write-Host ""

# Find Bern.zip asset download URL
$asset = $release.assets | Where-Object { $_.name -eq $zipName }
if (-not $asset) {
    Write-Error "Bern.zip not found in the latest release assets."
    exit 1
}
$downloadUrl = $asset.browser_download_url

Write-Host "[bern] Downloading $zipName from $downloadUrl..." -ForegroundColor Cyan
Invoke-WebRequest -Uri $downloadUrl -OutFile $zipPath

Write-Host ""

# Extract to a temporary folder to avoid overwrite errors
$tempDir = Join-Path $env:TEMP "bern_install_tmp"
if (Test-Path $tempDir) { Remove-Item $tempDir -Recurse -Force }
New-Item -ItemType Directory -Path $tempDir | Out-Null

Write-Host "[bern] Extracting Bern.exe from $zipName..." -ForegroundColor Cyan
Add-Type -AssemblyName System.IO.Compression.FileSystem
[System.IO.Compression.ZipFile]::ExtractToDirectory($zipPath, $tempDir)


# Copy Bern.exe to installDir
$tempExePath = Join-Path $tempDir $exeName
if (!(Test-Path $tempExePath)) {
    Write-Error "Bern.exe not found in the extracted zip."
    Remove-Item $tempDir -Recurse -Force
    exit 1
}
Copy-Item $tempExePath -Destination $exePath -Force

# Copy lib folder to installDir
$tempLibPath = Join-Path $tempDir 'lib'
$destLibPath = Join-Path $installDir 'lib'
if (Test-Path $tempLibPath) {
    if (Test-Path $destLibPath) {
        Remove-Item $destLibPath -Recurse -Force
    }
    Copy-Item $tempLibPath -Destination $installDir -Recurse
    Write-Host "[bern] lib folder copied to $installDir." -ForegroundColor Green
} else {
    Write-Host "[bern] lib folder not found in the extracted zip." -ForegroundColor Yellow
}

# Clean up temp and zip
Remove-Item $zipPath
Remove-Item $tempDir -Recurse -Force

Write-Host "[bern] Bern.exe extracted to $installDir." -ForegroundColor Green

# Add installDir to user PATH if not already present
$envPath = [Environment]::GetEnvironmentVariable("Path", "User")
if ($envPath -notlike "*$installDir*") {
        Write-Host "[bern] Adding $installDir to user PATH..." -ForegroundColor Cyan
        [Environment]::SetEnvironmentVariable("Path", "$envPath;$installDir", "User")
        Write-Host "[bern] PATH updated. You may need to restart your terminal." -ForegroundColor Yellow
} else {
    Write-Host "[bern] $installDir is already in your PATH." -ForegroundColor DarkGray
}

Write-Host "[bern] Installation complete!" -ForegroundColor Green
