#!/bin/bash
set -euo pipefail

# Clean Jedi Installation from Virtual Environment
# Usage: ./clean-jedi.sh [venv_path]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Determine target venv path
if [ $# -eq 0 ]; then
    # Try to detect active venv
    if [ -n "${VIRTUAL_ENV:-}" ]; then
        VENV_PATH="$VIRTUAL_ENV"
        echo "🐍 Using active virtual environment: $VENV_PATH"
    elif [ -d ".venv" ]; then
        VENV_PATH="$(pwd)/.venv"
        echo "🐍 Using local .venv: $VENV_PATH"
    elif [ -d "venv" ]; then
        VENV_PATH="$(pwd)/venv"
        echo "🐍 Using local venv: $VENV_PATH"
    else
        echo "❌ No virtual environment found"
        echo "   Please specify a path or activate a virtual environment"
        echo "   Usage: $0 [venv_path]"
        exit 1
    fi
else
    VENV_PATH="$1"
    echo "🐍 Using specified virtual environment: $VENV_PATH"
fi

# Validate venv path
if [ ! -d "$VENV_PATH" ]; then
    echo "❌ Virtual environment not found: $VENV_PATH"
    exit 1
fi

JEDI_DIR="$VENV_PATH/jedi"
VENV_BIN_DIR="$VENV_PATH/bin"
if [ ! -d "$VENV_BIN_DIR" ]; then
    VENV_BIN_DIR="$VENV_PATH/Scripts"  # Windows
fi

echo "🧹 Cleaning Jedi Installation"
echo "============================="
echo "   Target: $VENV_PATH"
echo ""

# Remove Jedi directory
if [ -d "$JEDI_DIR" ]; then
    echo "🗑️  Removing Jedi directory: $JEDI_DIR"
    rm -rf "$JEDI_DIR"
else
    echo "ℹ️  Jedi directory not found: $JEDI_DIR"
fi

# Remove symlinks
echo "🔗 Removing symlinks..."
SYMLINKS=(
    "$VENV_BIN_DIR/jedi-language-server"
    "$VENV_BIN_DIR/pylsp"
    "$VENV_BIN_DIR/jedi-health-check"
)

for symlink in "${SYMLINKS[@]}"; do
    if [ -L "$symlink" ]; then
        echo "   Removing: $symlink"
        rm "$symlink"
    fi
done

# Clean activation script
VENV_ACTIVATE="$VENV_PATH/bin/activate"
if [ ! -f "$VENV_ACTIVATE" ]; then
    VENV_ACTIVATE="$VENV_PATH/Scripts/activate"  # Windows
fi

if [ -f "$VENV_ACTIVATE" ]; then
    echo "🔧 Cleaning venv activation script..."
    # Remove Jedi activation lines
    sed -i.bak '/# Activate Jedi/,+1d' "$VENV_ACTIVATE" 2>/dev/null || true
    rm -f "$VENV_ACTIVATE.bak" 2>/dev/null || true
fi

# Clean cache
CACHE_DIR="${HOME}/.cache/jedi"
if [ -d "$CACHE_DIR" ]; then
    echo "🗑️  Cleaning Jedi cache: $CACHE_DIR"
    rm -rf "$CACHE_DIR"
fi

PYLSP_CACHE_DIR="${HOME}/.cache/pylsp"
if [ -d "$PYLSP_CACHE_DIR" ]; then
    echo "🗑️  Cleaning PyLSP cache: $PYLSP_CACHE_DIR"
    rm -rf "$PYLSP_CACHE_DIR"
fi

echo ""
echo "✅ Jedi cleanup completed"
echo "   All Jedi files and configurations have been removed"