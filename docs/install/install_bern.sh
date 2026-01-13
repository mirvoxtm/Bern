#!/usr/bin/env bash
# Bern Language Installer (Bash)
# Installs Bern from GitHub to /opt/bern and adds it to PATH

set -e

INSTALL_DIR="/opt/bern"
REPO="mirvoxtm/Bern"
ZIP_NAME="Bern_win_1.0.1.zip"
EXE_NAME="Bern.exe"
ZIP_PATH="$INSTALL_DIR/$ZIP_NAME"
EXE_PATH="$INSTALL_DIR/$EXE_NAME"

# Bern REPL-style banner
PURPLE='\033[35m'
GRAY='\033[90m'
GREEN='\033[32m'
CYAN='\033[36m'
YELLOW='\033[33m'
RESET='\033[0m'

banner=(
  "______                  "
  "| ___ \\                      The Bern Language Installer "
  "| |_/ / ___ _ __ _ __       "
  "| ___ \/ _ \\ '__| '_ \\     This script will install Bern on"
  "| |_/ /  __/ |  | | | |                 your machine"
  "\\____/ \\___|_|  |_| |_|         [ v.1.0.1 12.01.2025 ]"
)
for line in "${banner[@]}"; do
  echo -e "${PURPLE}${line}${RESET}"
done

echo -e "${GRAY}--------------------------------------------------------------${RESET}\n"
echo -e "${CYAN}[bern] Installing to $INSTALL_DIR...${RESET}"

# Create install directory if it doesn't exist
if [ ! -d "$INSTALL_DIR" ]; then
  sudo mkdir -p "$INSTALL_DIR"
fi

# Get latest release info from GitHub
RELEASE_URL="https://api.github.com/repos/$REPO/releases/latest"
echo -e "${CYAN}[bern] Fetching latest release info from GitHub...${RESET}"
RELEASE_JSON=$(curl -s "$RELEASE_URL")

echo

# Find Bern.zip asset download URL
DOWNLOAD_URL=$(echo "$RELEASE_JSON" | grep "browser_download_url" | grep "$ZIP_NAME" | cut -d '"' -f4)
if [ -z "$DOWNLOAD_URL" ]; then
  echo "Bern.zip not found in the latest release assets." >&2
  exit 1
fi

echo -e "${CYAN}[bern] Downloading $ZIP_NAME from $DOWNLOAD_URL...${RESET}"
curl -L "$DOWNLOAD_URL" -o "$ZIP_PATH"

echo

# Extract to a temporary folder to avoid overwrite errors
TEMP_DIR="/tmp/bern_install_tmp"
rm -rf "$TEMP_DIR"
mkdir -p "$TEMP_DIR"

echo -e "${CYAN}[bern] Extracting Bern.exe and lib from $ZIP_NAME...${RESET}"
unzip -o "$ZIP_PATH" -d "$TEMP_DIR"

# Copy Bern.exe to installDir
if [ ! -f "$TEMP_DIR/$EXE_NAME" ]; then
  echo "Bern.exe not found in the extracted zip." >&2
  rm -rf "$TEMP_DIR"
  exit 1
fi
sudo cp "$TEMP_DIR/$EXE_NAME" "$EXE_PATH"

# Copy lib folder to installDir
if [ -d "$TEMP_DIR/lib" ]; then
  if [ -d "$INSTALL_DIR/lib" ]; then
    sudo rm -rf "$INSTALL_DIR/lib"
  fi
  sudo cp -r "$TEMP_DIR/lib" "$INSTALL_DIR/"
  echo -e "${GREEN}[bern] lib folder copied to $INSTALL_DIR.${RESET}"
else
  echo -e "${YELLOW}[bern] lib folder not found in the extracted zip.${RESET}"
fi

# Clean up temp and zip
rm -rf "$ZIP_PATH" "$TEMP_DIR"

echo -e "${GREEN}[bern] Bern.exe extracted to $INSTALL_DIR.${RESET}"

# Add installDir to user PATH if not already present
if ! echo "$PATH" | grep -q "$INSTALL_DIR"; then
  echo -e "${CYAN}[bern] Adding $INSTALL_DIR to user PATH...${RESET}"
  echo "export PATH=\"$INSTALL_DIR:$PATH\"" >> ~/.bashrc
  echo -e "${YELLOW}[bern] PATH updated. You may need to restart your terminal.${RESET}"
else
  echo -e "${GRAY}[bern] $INSTALL_DIR is already in your PATH.${RESET}"
fi

echo -e "${GREEN}[bern] Installation complete!${RESET}"
