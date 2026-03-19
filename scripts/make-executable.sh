#!/bin/bash
# Make all scripts executable

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

chmod +x "$SCRIPT_DIR"/*.sh
chmod +x "$SCRIPT_DIR/../docker"/*.py

echo "✅ All scripts are now executable"