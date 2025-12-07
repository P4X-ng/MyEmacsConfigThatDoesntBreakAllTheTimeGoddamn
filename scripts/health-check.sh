#!/bin/bash
set -euo pipefail

# Health Check for Deployed Jedi
# Usage: ./health-check.sh [venv_path]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Determine target venv path
if [ $# -eq 0 ]; then
    # Try to detect active venv
    if [ -n "${VIRTUAL_ENV:-}" ]; then
        VENV_PATH="$VIRTUAL_ENV"
    elif [ -d ".venv" ]; then
        VENV_PATH="$(pwd)/.venv"
    elif [ -d "venv" ]; then
        VENV_PATH="$(pwd)/venv"
    else
        echo "❌ No virtual environment found"
        echo "   Please specify a path or activate a virtual environment"
        echo "   Usage: $0 [venv_path]"
        exit 1
    fi
else
    VENV_PATH="$1"
fi

# Validate venv path
if [ ! -d "$VENV_PATH" ]; then
    echo "❌ Virtual environment not found: $VENV_PATH"
    exit 1
fi

JEDI_DIR="$VENV_PATH/jedi"
HEALTH_CHECK="$JEDI_DIR/bin/health-check.py"

echo "🔍 Running Jedi Health Check"
echo "============================"
echo "   Virtual Environment: $VENV_PATH"
echo "   Jedi Directory: $JEDI_DIR"
echo ""

# Check if Jedi is deployed
if [ ! -d "$JEDI_DIR" ]; then
    echo "❌ Jedi not found in virtual environment"
    echo "   Run ./scripts/deploy-jedi.sh first"
    exit 1
fi

# Check if health check script exists
if [ ! -f "$HEALTH_CHECK" ]; then
    echo "❌ Health check script not found: $HEALTH_CHECK"
    echo "   Jedi deployment may be incomplete"
    exit 1
fi

# Activate the virtual environment and run health check
if [ -f "$VENV_PATH/bin/activate" ]; then
    source "$VENV_PATH/bin/activate"
elif [ -f "$VENV_PATH/Scripts/activate" ]; then
    source "$VENV_PATH/Scripts/activate"
else
    echo "❌ Cannot activate virtual environment"
    exit 1
fi

# Run the health check
python "$HEALTH_CHECK"