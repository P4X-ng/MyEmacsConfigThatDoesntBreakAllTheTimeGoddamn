# IDE Server

A Python-based service that provides IDE features for Emacs through a simple HTTP JSON API.

## Features

- **Chat Interface**: Integrate with OpenAI-compatible LLM endpoints for coding assistance
- **Context Management**: Manage and search context directories
- **Simple HTTP API**: Easy integration with Emacs Lisp

## Installation

No external dependencies are required. The server uses only the Python standard library, including
`urllib` for LLM requests.

```bash
chmod +x server.py
```

## Usage

Start the server:

```bash
python3 server.py
```

Or with custom host/port:

```bash
python3 server.py --host 0.0.0.0 --port 8888
```

## API Endpoints

### Health Check
- `GET /health` - Check if server is running

### Chat
- `POST /chat/send` - Send a message to the LLM
  - Body: `{"message": "your message", "conversation_id": "optional_id", "context": "optional context"}`
  - Behavior:
    - Uses OpenAI-compatible `chat/completions` APIs
    - Includes prior conversation messages automatically
    - Injects optional `context` as a system message for that request
- `GET /chat/conversations` - List all conversation IDs
- `GET /chat/history/{conv_id}` - Get conversation history
- `POST /chat/clear` - Clear a conversation
  - Body: `{"conversation_id": "id"}`

### Context
- `GET /context/dirs` - List context directories
- `POST /context/add` - Add a context directory
  - Body: `{"path": "/path/to/dir"}`
- `POST /context/remove` - Remove a context directory
  - Body: `{"path": "/path/to/dir"}`
- `POST /context/search` - Search context directories
  - Body: `{"query": "search term"}`

## Environment Variables

- `OPENAI_API_KEY` - Your OpenAI API key
- `GPTEL_BACKEND` - Backend to use (openai, vllm, tgi)
- `OPENAI_BASE_URL` - Base URL for API (for custom endpoints, defaults to `https://api.openai.com/v1`)
- `GPTEL_MODEL` - Model to use (default: gpt-4o-mini)
- `IDE_SERVER_LLM_TIMEOUT_SECONDS` - HTTP timeout for LLM requests (default: `30`)
- `IDE_SERVER_RATE_LIMIT_MAX_REQUESTS` - Max requests per client in a window (default: `100`)
- `IDE_SERVER_RATE_LIMIT_WINDOW_SECONDS` - Rate limit window in seconds (default: `60`)

## Running tests

From this directory:

```bash
python3 -m unittest discover -s tests -p "test_*.py"
```

## Integration with Emacs

The Emacs configuration automatically connects to this server when started. See `init.el` for the integration code.
