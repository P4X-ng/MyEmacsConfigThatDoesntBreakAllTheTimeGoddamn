# IDE Server

A Python-based service that provides IDE features for Emacs through a simple HTTP JSON API.

## Features

- **Chat Interface**: Integrate with LLMs for coding assistance
- **Context Management**: Manage and search context directories
- **Simple HTTP API**: Easy integration with Emacs Lisp

## Installation

No external dependencies required - uses only Python standard library.

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
- `OPENAI_BASE_URL` - Base URL for API (for custom endpoints)
- `GPTEL_MODEL` - Model to use (default: gpt-4o-mini)
- `IDE_RATE_LIMIT_MAX_REQUESTS` - Max requests per client in window (default: 100; legacy alias: `IDE_SERVER_MAX_REQUESTS`)
- `IDE_RATE_LIMIT_WINDOW_SECONDS` - Rate limit window size in seconds (default: 60; legacy alias: `IDE_SERVER_RATE_LIMIT_WINDOW`)
- `LLM_REQUEST_TIMEOUT_SECONDS` - HTTP timeout for LLM requests (default: 30)
- `LLM_MAX_TOKENS` - Optional max tokens per response (default: disabled)

## LLM Runtime Behavior

- Uses OpenAI-compatible `POST /chat/completions` via Python stdlib HTTP client.
- Sends full in-memory conversation history for each request.
- If `context` is provided in `/chat/send`, it is prepended as a `system` message.
- For local backends (`GPTEL_BACKEND=vllm|tgi`), a fallback API key value is used if `OPENAI_API_KEY` is unset.
- On provider/network errors, the API returns a descriptive assistant message instead of crashing.

## Integration with Emacs

The Emacs configuration automatically connects to this server when started. See `init.el` for the integration code.

## Testing

Run server tests with:

```bash
python3 -m unittest discover -s tests -p "test_*.py"
```
