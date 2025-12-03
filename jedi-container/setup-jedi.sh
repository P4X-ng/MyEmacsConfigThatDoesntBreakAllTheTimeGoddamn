#!/bin/bash
#
# setup-jedi.sh - Install jedi-language-server from Docker container to host venv
#
# This script builds the jedi container and copies the complete jedi venv
# to ~/.venv/jedi for use with Emacs LSP.
#
# Usage:
#   ./setup-jedi.sh           # Install jedi to ~/.venv/jedi
#   VENV_HOME=/path ./setup-jedi.sh  # Install to custom venv home
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_HOME="${VENV_HOME:-$HOME/.venv}"
JEDI_VENV="$VENV_HOME/jedi"

echo "=============================================="
echo "  Jedi Language Server - Containerized Setup"
echo "=============================================="
echo ""
echo "Target: $JEDI_VENV"
echo ""

# Ensure venv home exists
mkdir -p "$VENV_HOME"

# Check for docker/podman
if command -v docker &> /dev/null; then
    CONTAINER_CMD="docker"
elif command -v podman &> /dev/null; then
    CONTAINER_CMD="podman"
else
    echo "❌ Error: Docker or Podman is required"
    echo "   Please install Docker: https://docs.docker.com/get-docker/"
    exit 1
fi

echo "Using: $CONTAINER_CMD"
echo ""

# Check for docker-compose or podman-compose
if command -v docker-compose &> /dev/null; then
    COMPOSE_CMD="docker-compose"
elif command -v docker &> /dev/null && docker compose version &> /dev/null; then
    COMPOSE_CMD="docker compose"
elif command -v podman-compose &> /dev/null; then
    COMPOSE_CMD="podman-compose"
else
    echo "⚠️  No compose command found, using direct container build..."
    USE_DIRECT_BUILD=1
fi

cd "$SCRIPT_DIR"

if [ -n "$USE_DIRECT_BUILD" ]; then
    echo "📦 Building jedi container..."
    $CONTAINER_CMD build -t jedi-language-server:latest .
    
    echo ""
    echo "📋 Copying jedi venv to $JEDI_VENV..."
    
    # Remove existing jedi venv if present
    rm -rf "$JEDI_VENV"
    
    # Create a temporary container to extract the venv
    TEMP_CONTAINER=$($CONTAINER_CMD create jedi-language-server:latest)
    $CONTAINER_CMD cp "$TEMP_CONTAINER:/jedi-venv" "$JEDI_VENV"
    $CONTAINER_CMD rm "$TEMP_CONTAINER" > /dev/null
else
    echo "📦 Building jedi container with compose..."
    VENV_HOME="$VENV_HOME" $COMPOSE_CMD build
    
    echo ""
    echo "📋 Installing jedi venv to $JEDI_VENV..."
    VENV_HOME="$VENV_HOME" $COMPOSE_CMD run --rm install-jedi
fi

# Verify installation
if [ -x "$JEDI_VENV/bin/jedi-language-server" ]; then
    echo ""
    echo "=============================================="
    echo "  ✅ Installation Complete!"
    echo "=============================================="
    echo ""
    echo "Jedi Language Server installed to:"
    echo "  $JEDI_VENV/bin/jedi-language-server"
    echo ""
    echo "Version:"
    if ! "$JEDI_VENV/bin/jedi-language-server" --version 2>&1; then
        echo "  (version check failed - server may still work)"
    fi
    echo ""
    echo "To use with Emacs, the init.el has been configured to"
    echo "automatically detect and use this jedi installation."
    echo ""
else
    echo ""
    echo "❌ Installation may have failed."
    echo "   Expected: $JEDI_VENV/bin/jedi-language-server"
    exit 1
fi
