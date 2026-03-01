#!/bin/bash
set -euo pipefail

# Emacs Configuration Setup for Ubuntu 24.04
# This script sets up a complete Emacs IDE environment with:
# - Emacs installation
# - Configuration deployment via rsync
# - Language servers (clangd, bash-language-server, typescript-language-server)
# - Code formatters (black, shfmt, prettier, clang-format)
# - OpenAI API integration setup

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# User feedback functions
info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
}

section() {
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# Check if running on Ubuntu 24.04
check_ubuntu_version() {
    section "Checking Ubuntu Version"
    
    if [ ! -f /etc/os-release ]; then
        error "Cannot detect OS version"
        exit 1
    fi
    
    . /etc/os-release
    
    if [ "$ID" != "ubuntu" ]; then
        warning "This script is designed for Ubuntu 24.04"
        warning "Detected OS: $NAME"
        read -p "Continue anyway? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
    
    info "Detected: $NAME $VERSION"
    success "OS check completed"
}

# Update package lists
update_packages() {
    section "Updating Package Lists"
    
    info "Running: sudo apt update"
    if sudo apt update; then
        success "Package lists updated"
    else
        error "Failed to update package lists"
        exit 1
    fi
}

# Install Emacs
install_emacs() {
    section "Installing Emacs"
    
    if command -v emacs &> /dev/null; then
        EMACS_VERSION=$(emacs --version | head -n1)
        info "Emacs is already installed: $EMACS_VERSION"
        read -p "Reinstall/upgrade Emacs? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            return 0
        fi
    fi
    
    info "Installing Emacs..."
    if sudo apt install -y emacs; then
        EMACS_VERSION=$(emacs --version | head -n1)
        success "Emacs installed: $EMACS_VERSION"
    else
        error "Failed to install Emacs"
        exit 1
    fi
}

# Install Node.js and npm (required for language servers)
install_nodejs() {
    section "Installing Node.js and npm"
    
    if command -v node &> /dev/null && command -v npm &> /dev/null; then
        NODE_VERSION=$(node --version)
        NPM_VERSION=$(npm --version)
        info "Node.js is already installed: $NODE_VERSION"
        info "npm is already installed: $NPM_VERSION"
        return 0
    fi
    
    info "Installing Node.js and npm..."
    if sudo apt install -y nodejs npm; then
        NODE_VERSION=$(node --version)
        NPM_VERSION=$(npm --version)
        success "Node.js installed: $NODE_VERSION"
        success "npm installed: $NPM_VERSION"
    else
        error "Failed to install Node.js and npm"
        exit 1
    fi
}

# Install language servers
install_language_servers() {
    section "Installing Language Servers"
    
    # Install clangd (C/C++ language server)
    info "Installing clangd (C/C++ language server)..."
    if sudo apt install -y clangd; then
        success "clangd installed"
    else
        warning "Failed to install clangd (C/C++ features may not work)"
    fi
    
    # Install bash-language-server
    info "Installing bash-language-server..."
    if sudo npm install -g bash-language-server; then
        success "bash-language-server installed"
    else
        warning "Failed to install bash-language-server (Bash features may not work)"
    fi
    
    # Install typescript-language-server
    info "Installing typescript-language-server..."
    if sudo npm install -g typescript-language-server typescript; then
        success "typescript-language-server installed"
    else
        warning "Failed to install typescript-language-server (TypeScript features may not work)"
    fi
    
    # Install pyright (Python language server)
    info "Installing pyright (Python language server)..."
    if sudo npm install -g pyright; then
        success "pyright installed"
    else
        warning "Failed to install pyright (Python LSP features may not work)"
    fi
}

# Install code formatters
install_formatters() {
    section "Installing Code Formatters"
    
    # Install Python (for black formatter)
    if ! command -v python3 &> /dev/null; then
        info "Installing Python3..."
        sudo apt install -y python3 python3-pip
    fi
    
    # Install black (Python formatter)
    info "Installing black (Python formatter)..."
    if python3 -m pip install --user black; then
        success "black installed"
    else
        warning "Failed to install black (Python formatting may not work)"
    fi
    
    # Install prettier (TypeScript/JavaScript formatter)
    info "Installing prettier..."
    if sudo npm install -g prettier; then
        success "prettier installed"
    else
        warning "Failed to install prettier (TypeScript/JavaScript formatting may not work)"
    fi
    
    # Install Go (for shfmt)
    if ! command -v go &> /dev/null; then
        info "Installing Go (required for shfmt)..."
        sudo apt install -y golang-go
    fi
    
    # Install shfmt (Bash formatter)
    info "Installing shfmt (Bash formatter)..."
    if go install mvdan.cc/sh/v3/cmd/shfmt@latest; then
        # Add Go bin to PATH if not already there
        if [[ ":$PATH:" != *":$HOME/go/bin:"* ]]; then
            echo 'export PATH="$HOME/go/bin:$PATH"' >> ~/.bashrc
            export PATH="$HOME/go/bin:$PATH"
        fi
        success "shfmt installed"
    else
        warning "Failed to install shfmt (Bash formatting may not work)"
    fi
    
    # clang-format usually comes with clangd
    if command -v clang-format &> /dev/null; then
        success "clang-format already available"
    else
        warning "clang-format not found (C/C++ formatting may not work)"
    fi
}

# Deploy Emacs configuration
deploy_config() {
    section "Deploying Emacs Configuration"
    
    EMACS_DIR="$HOME/.emacs.d"
    SOURCE_DIR="$SCRIPT_DIR/dot.emacs.d"
    
    # Backup existing configuration
    if [ -d "$EMACS_DIR" ]; then
        BACKUP_DIR="$HOME/.emacs.d.backup.$(date +%Y%m%d_%H%M%S)"
        warning "Existing Emacs configuration found at $EMACS_DIR"
        info "Creating backup: $BACKUP_DIR"
        
        if mv "$EMACS_DIR" "$BACKUP_DIR"; then
            success "Backup created: $BACKUP_DIR"
        else
            error "Failed to create backup"
            exit 1
        fi
    fi
    
    # Deploy configuration using rsync
    info "Deploying configuration from $SOURCE_DIR to $EMACS_DIR"
    info "Using rsync for intelligent file synchronization..."
    
    if rsync -av --exclude='.git*' --exclude='*.backup' --exclude='straight/' \
             --exclude='.cache/' --exclude='auto-save-list/' --exclude='eln-cache/' \
             "$SOURCE_DIR/" "$EMACS_DIR/"; then
        success "Configuration deployed successfully"
        info "Location: $EMACS_DIR"
    else
        error "Failed to deploy configuration"
        exit 1
    fi
    
    # Create necessary directories
    mkdir -p "$HOME/.cache/jedi"
    success "Created cache directories"
}

# Setup OpenAI API integration
setup_openai() {
    section "Setting Up OpenAI API Integration"
    
    ENV_FILE="$HOME/.emacs.d/.env"
    ENV_EXAMPLE="$SCRIPT_DIR/.env.example"
    
    info "OpenAI API integration allows you to ask questions inline in Emacs"
    info "using GPTel (ChatGPT integration)"
    echo ""
    
    # Check if .env already exists
    if [ -f "$ENV_FILE" ]; then
        warning ".env file already exists at $ENV_FILE"
        read -p "Do you want to reconfigure OpenAI? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            info "Keeping existing .env configuration"
            return 0
        fi
    fi
    
    # Ask user if they want to configure OpenAI
    read -p "Do you want to configure OpenAI API now? (y/N) " -n 1 -r
    echo
    
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        # Copy example file
        cp "$ENV_EXAMPLE" "$ENV_FILE"
        
        echo ""
        info "Please enter your OpenAI API key"
        info "Get your key from: https://platform.openai.com/api-keys"
        echo ""
        read -p "OpenAI API Key (or press Enter to skip): " OPENAI_KEY
        
        if [ -n "$OPENAI_KEY" ]; then
            # Update .env file with the API key
            sed -i "s/OPENAI_API_KEY=your_openai_api_key_here/OPENAI_API_KEY=$OPENAI_KEY/" "$ENV_FILE"
            success "OpenAI API key configured"
            
            # Add to .bashrc if not already there
            if ! grep -q "source.*\.emacs\.d/\.env" ~/.bashrc; then
                echo "" >> ~/.bashrc
                echo "# Load Emacs environment variables" >> ~/.bashrc
                echo "if [ -f \"$ENV_FILE\" ]; then" >> ~/.bashrc
                echo "    export \$(grep -v '^#' \"$ENV_FILE\" | xargs)" >> ~/.bashrc
                echo "fi" >> ~/.bashrc
                success "Added .env to ~/.bashrc"
                
                # Source for current session
                export $(grep -v '^#' "$ENV_FILE" | xargs) 2>/dev/null || true
            fi
            
            info "Use C-c C-g in Emacs to open GPTel chat"
        else
            info "Skipped OpenAI configuration"
            info "You can configure it later by editing: $ENV_FILE"
        fi
    else
        info "Skipped OpenAI configuration"
        info "To configure later:"
        info "  1. Copy $ENV_EXAMPLE to $ENV_FILE"
        info "  2. Edit $ENV_FILE and add your API key"
        info "  3. Source it: export \$(grep -v '^#' $ENV_FILE | xargs)"
    fi
}

# Display completion message
display_completion() {
    section "Installation Complete!"
    
    echo ""
    success "Emacs configuration has been successfully installed!"
    echo ""
    info "What was installed:"
    echo "  ✓ Emacs (latest version from apt)"
    echo "  ✓ Configuration deployed to ~/.emacs.d"
    echo "  ✓ Language servers (clangd, bash-language-server, typescript-language-server, pyright)"
    echo "  ✓ Code formatters (black, prettier, shfmt, clang-format)"
    echo "  ✓ OpenAI API integration setup (if configured)"
    echo ""
    info "Configuration location: $HOME/.emacs.d"
    if [ -n "$(ls -A $HOME/.emacs.d.backup* 2>/dev/null)" ]; then
        info "Backup of old config: $(ls -dt $HOME/.emacs.d.backup* 2>/dev/null | head -1)"
    fi
    echo ""
    info "Next steps:"
    echo "  1. Start Emacs: emacs"
    echo "     (First launch will install packages - takes a few minutes)"
    echo "  2. Press F8 to toggle the file explorer (Treemacs)"
    echo "  3. Press C-k to see the keybinding cheat sheet"
    echo "  4. Use C-c C-g to chat with AI (if OpenAI configured)"
    echo ""
    info "Documentation:"
    echo "  • README: $SCRIPT_DIR/README.md"
    echo "  • Autocomplete setup: $SCRIPT_DIR/AUTOCOMPLETE_SETUP.md"
    echo "  • Cheat sheet: $HOME/.emacs.d/README.cheatsheet.md"
    echo ""
    info "For Python autocomplete, see the Jedi container setup:"
    echo "  • Build: ./scripts/build-jedi.sh"
    echo "  • Deploy: ./scripts/deploy-jedi.sh"
    echo ""
    
    if command -v emacs &> /dev/null; then
        echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo -e "${GREEN}Ready to start! Run: emacs${NC}"
        echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    fi
}

# Main installation flow
main() {
    echo ""
    echo -e "${BLUE}╔══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║  Emacs Configuration Setup for Ubuntu 24.04             ║${NC}"
    echo -e "${BLUE}║  Modern IDE with LSP, Autocomplete, and AI Integration  ║${NC}"
    echo -e "${BLUE}╚══════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    info "This script will:"
    echo "  • Check Ubuntu version"
    echo "  • Install Emacs and dependencies"
    echo "  • Install language servers (C/C++, Bash, TypeScript, Python)"
    echo "  • Install code formatters"
    echo "  • Deploy configuration to ~/.emacs.d using rsync"
    echo "  • Setup OpenAI API integration (optional)"
    echo ""
    warning "This will backup any existing ~/.emacs.d directory"
    echo ""
    
    read -p "Continue with installation? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        info "Installation cancelled"
        exit 0
    fi
    
    check_ubuntu_version
    update_packages
    install_emacs
    install_nodejs
    install_language_servers
    install_formatters
    deploy_config
    setup_openai
    display_completion
}

# Run main function
main "$@"
