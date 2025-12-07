#!/bin/bash
# Startup script for IDE Server

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SERVER_SCRIPT="$SCRIPT_DIR/server.py"

# Check if server.py exists
if [ ! -f "$SERVER_SCRIPT" ]; then
    echo "Error: server.py not found at $SERVER_SCRIPT"
    exit 1
fi

# Default values
PORT=9999
HOST="127.0.0.1"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --port)
            PORT="$2"
            shift 2
            ;;
        --host)
            HOST="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --port PORT    Port to bind to (default: 9999)"
            echo "  --host HOST    Host to bind to (default: 127.0.0.1)"
            echo "  --help         Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  OPENAI_API_KEY    OpenAI API key for LLM chat"
            echo "  GPTEL_BACKEND     Backend to use (openai, vllm, tgi)"
            echo "  OPENAI_BASE_URL   Base URL for API"
            echo "  GPTEL_MODEL       Model to use (default: gpt-4o-mini)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Start the server
echo "Starting IDE Server on http://$HOST:$PORT"
python3 -u "$SERVER_SCRIPT" --host "$HOST" --port "$PORT"
