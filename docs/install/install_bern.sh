#!/usr/bin/env bash
# Bern Language Installer
# Installs Bern from GitHub to /opt/bern and adds it to PATH

set -euo pipefail

# Configuration
readonly INSTALL_DIR="/opt/bern"
readonly REPO="mirvoxtm/Bern"
readonly ZIP_NAME="Bern_linux_1.1.3.zip"
readonly VERSION="1.1.3"

# Colors
readonly PURPLE='\033[35m'
readonly GRAY='\033[90m'
readonly GREEN='\033[32m'
readonly CYAN='\033[36m'
readonly YELLOW='\033[33m'
readonly RESET='\033[0m'

# Print colored message
log() {
    local color=$1
    shift
    echo -e "${color}$*${RESET}"
}

# Print banner
print_banner() {
    log "$PURPLE" "______                  "
    log "$PURPLE" "| ___ \\                      The Bern Language Installer "
    log "$PURPLE" "| |_/ / ___ _ __ _ __       "
    log "$PURPLE" "| ___ \/ _ \\ '__| '_ \\     This script will install Bern on"
    log "$PURPLE" "| |_/ /  __/ |  | | | |                 your machine"
    log "$PURPLE" "\\____/ \\___|_|  |_| |_|         [ v.1.1.3 24.02.2026 ]"
    log "$GRAY" "--------------------------------------------------------------\n"
}

# Download and extract Bern
install_bern() {
    log "$CYAN" "[bern] Installing to $INSTALL_DIR..."
    
    sudo mkdir -p "$INSTALL_DIR"
    
    # Fetch latest release
    log "$CYAN" "[bern] Fetching latest release from GitHub..."
    local release_url="https://api.github.com/repos/$REPO/releases/latest"
    local download_url
    download_url=$(curl -sL "$release_url" | grep -o "https://.*/$ZIP_NAME" | head -1)
    
    if [[ -z "$download_url" ]]; then
        log "$YELLOW" "[bern] Error: Could not find $ZIP_NAME in latest release" >&2
        exit 1
    fi
    
    # Download to temp directory
    local temp_dir
    temp_dir=$(mktemp -d)
    trap 'rm -rf "$temp_dir"' EXIT
    
    log "$CYAN" "[bern] Downloading $ZIP_NAME..."
    curl -sL "$download_url" -o "$temp_dir/$ZIP_NAME"
    
    # Extract
    log "$CYAN" "[bern] Extracting..."
    unzip -q "$temp_dir/$ZIP_NAME" -d "$temp_dir"
    
    # Install binary
    if [[ ! -f "$temp_dir/Bern" ]]; then
        log "$YELLOW" "[bern] Error: Bern binary not found in archive" >&2
        exit 1
    fi
    
    sudo cp "$temp_dir/Bern" "$INSTALL_DIR/Bern"
    sudo chmod +x "$INSTALL_DIR/Bern"
    
    # Create lowercase wrapper
    sudo tee "$INSTALL_DIR/bern" > /dev/null << 'EOF'
#!/usr/bin/env bash
exec "$(dirname "$0")/Bern" "$@"
EOF
    sudo chmod +x "$INSTALL_DIR/bern"
    
    # Install lib folder if present
    if [[ -d "$temp_dir/lib" ]]; then
        sudo rm -rf "$INSTALL_DIR/lib"
        sudo cp -r "$temp_dir/lib" "$INSTALL_DIR/"
        log "$GREEN" "[bern] lib folder installed"
    fi
    
    log "$GREEN" "[bern] Bern binary installed successfully"
}

# Configure PATH
setup_path() {
    # Try system-wide installation first
    if [[ -w /etc/profile.d ]] || [[ $EUID -eq 0 ]]; then
        log "$CYAN" "[bern] Configuring system-wide PATH..."
        
        # Bash/sh/zsh
        sudo tee /etc/profile.d/bern.sh > /dev/null << 'EOF'
export PATH="/opt/bern:$PATH"
EOF
        sudo chmod 644 /etc/profile.d/bern.sh
        
        # Fish
        sudo mkdir -p /etc/fish/conf.d
        sudo tee /etc/fish/conf.d/bern.fish > /dev/null << 'EOF'
set -gx PATH /opt/bern $PATH
EOF
        sudo chmod 644 /etc/fish/conf.d/bern.fish
        
        log "$YELLOW" "[bern] PATH configured system-wide. Restart your shell or run:"
        log "$YELLOW" "       source /etc/profile.d/bern.sh"
    else
        # User-level installation
        local user_home="${SUDO_USER:+$(eval echo "~$SUDO_USER")}"
        user_home="${user_home:-$HOME}"
        local shell_name
        shell_name=$(basename "${SHELL:-bash}")
        
        case "$shell_name" in
            fish)
                local rc="$user_home/.config/fish/config.fish"
                local line="set -gx PATH /opt/bern \$PATH"
                ;;
            zsh)
                local rc="$user_home/.zshrc"
                local line='export PATH="/opt/bern:$PATH"'
                ;;
            *)
                local rc="$user_home/.bashrc"
                local line='export PATH="/opt/bern:$PATH"'
                ;;
        esac
        
        if ! grep -qF "/opt/bern" "$rc" 2>/dev/null; then
            log "$CYAN" "[bern] Adding /opt/bern to $rc..."
            echo "$line" >> "$rc"
            log "$YELLOW" "[bern] PATH updated. Restart your terminal or run:"
            log "$YELLOW" "       source $rc"
        else
            log "$GRAY" "[bern] /opt/bern already in PATH"
        fi
    fi
}

# Main installation
main() {
    print_banner
    install_bern
    setup_path
    echo
    log "$GREEN" "[bern] Installation complete!"
    log "$CYAN" "[bern] Run 'bern' or 'Bern' to get started"
}

main "$@"