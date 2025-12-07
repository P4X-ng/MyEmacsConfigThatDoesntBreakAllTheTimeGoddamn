#!/bin/bash
set -euo pipefail

# Deploy Reliable Jedi to Virtual Environment
# Usage: ./deploy-jedi.sh [venv_path]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DEPLOYMENT_ARCHIVE="$PROJECT_ROOT/dist/jedi-deployment.tar.gz"

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

if [ ! -f "$VENV_PATH/bin/python" ] && [ ! -f "$VENV_PATH/Scripts/python.exe" ]; then
    echo "❌ Invalid virtual environment: $VENV_PATH"
    echo "   No Python executable found"
    exit 1
fi

# Check if deployment archive exists
if [ ! -f "$DEPLOYMENT_ARCHIVE" ]; then
    echo "❌ Deployment archive not found: $DEPLOYMENT_ARCHIVE"
    echo "   Run ./scripts/build-jedi.sh first"
    exit 1
fi

echo "🚀 Deploying Reliable Jedi"
echo "=========================="
echo "   Source: $DEPLOYMENT_ARCHIVE"
echo "   Target: $VENV_PATH"
echo ""

# Create jedi directory in venv
JEDI_DIR="$VENV_PATH/jedi"
echo "📁 Creating Jedi directory: $JEDI_DIR"
mkdir -p "$JEDI_DIR"

# Extract deployment archive
echo "📦 Extracting Jedi files..."
if tar -xzf "$DEPLOYMENT_ARCHIVE" -C "$VENV_PATH"; then
    echo "✅ Files extracted successfully"
else
    echo "❌ Failed to extract deployment archive"
    exit 1
fi

# Make scripts executable
echo "🔧 Setting up executables..."
chmod +x "$JEDI_DIR/bin"/*.py

# Create symlinks in venv bin directory
VENV_BIN_DIR="$VENV_PATH/bin"
if [ ! -d "$VENV_BIN_DIR" ]; then
    VENV_BIN_DIR="$VENV_PATH/Scripts"  # Windows
fi

echo "🔗 Creating symlinks..."
ln -sf "../jedi/bin/jedi-wrapper.py" "$VENV_BIN_DIR/jedi-language-server"
ln -sf "../jedi/bin/pylsp-wrapper.py" "$VENV_BIN_DIR/pylsp"
ln -sf "../jedi/bin/health-check.py" "$VENV_BIN_DIR/jedi-health-check"

# Create activation script
ACTIVATION_SCRIPT="$JEDI_DIR/activate-jedi.sh"
cat > "$ACTIVATION_SCRIPT" << 'EOF'
#!/bin/bash
# Jedi Environment Setup
export JEDI_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export PYTHONPATH="$JEDI_HOME/lib:${PYTHONPATH:-}"
export PATH="$JEDI_HOME/bin:$PATH"

# Jedi configuration
export JEDI_CACHE_DIRECTORY="${HOME}/.cache/jedi"
export JEDI_SETTINGS="$JEDI_HOME/config/jedi-config.json"
export PYLSP_SETTINGS="$JEDI_HOME/config/pylsp-config.json"

# Create cache directory
mkdir -p "$JEDI_CACHE_DIRECTORY"

echo "✅ Jedi environment activated"
echo "   Home: $JEDI_HOME"
echo "   Cache: $JEDI_CACHE_DIRECTORY"
EOF

chmod +x "$ACTIVATION_SCRIPT"

# Add to venv activation script
VENV_ACTIVATE="$VENV_PATH/bin/activate"
if [ ! -f "$VENV_ACTIVATE" ]; then
    VENV_ACTIVATE="$VENV_PATH/Scripts/activate"  # Windows
fi

if [ -f "$VENV_ACTIVATE" ]; then
    echo "🔧 Adding Jedi to venv activation..."
    if ! grep -q "activate-jedi.sh" "$VENV_ACTIVATE"; then
        echo "" >> "$VENV_ACTIVATE"
        echo "# Activate Jedi" >> "$VENV_ACTIVATE"
        echo "source \"$ACTIVATION_SCRIPT\"" >> "$VENV_ACTIVATE"
    fi
fi

# Run health check
echo "🧪 Running health check..."
if "$JEDI_DIR/bin/health-check.py"; then
    echo ""
    echo "🎉 Deployment completed successfully!"
    echo ""
    echo "Jedi is now available in your virtual environment:"
    echo "   jedi-language-server  - Jedi Language Server"
    echo "   pylsp                 - Python LSP Server with Jedi"
    echo "   jedi-health-check     - Health check utility"
    echo ""
    echo "To use with Emacs, make sure your venv is activated and"
    echo "the language servers will be automatically detected."
else
    echo ""
    echo "⚠️  Deployment completed but health check failed"
    echo "   Check the logs above for issues"
    exit 1
fi