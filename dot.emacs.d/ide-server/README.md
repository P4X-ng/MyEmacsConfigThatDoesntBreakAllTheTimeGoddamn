# IDE Server

A Python-based service that provides IDE features for Emacs through a simple HTTP JSON API.

## Features

- **Chat Interface**: Integrate with LLMs for coding assistance
- **Context Management**: Manage and search context directories
- **Simple HTTP API**: Easy integration with Emacs Lisp
- **OpenAI-Compatible Backends**: Works with OpenAI, vLLM, TGI, and compatible APIs

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

## LLM Backend Behavior

`POST /chat/send` now calls an OpenAI-compatible `chat/completions` endpoint.

- Uses conversation history (bounded by a configurable message limit)
- Supports optional `context` as a `system` message
- Uses `OPENAI_BASE_URL` for OpenAI-compatible local/remote backends
- Uses a placeholder token automatically for local backends (`vllm`, `tgi`) when no API key is set

Example local backend setup:

```bash
export GPTEL_BACKEND=vllm
export OPENAI_BASE_URL=http://127.0.0.1:8000/v1
export GPTEL_MODEL=Qwen/Qwen2.5-Coder-7B-Instruct
python3 server.py
```

Example OpenAI setup:

```bash
export OPENAI_API_KEY=sk-...
export GPTEL_BACKEND=openai
export OPENAI_BASE_URL=https://api.openai.com/v1
export GPTEL_MODEL=gpt-4o-mini
python3 server.py
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

- `OPENAI_API_KEY` - API key for OpenAI or compatible endpoint (required for OpenAI, optional for local backends)
- `GPTEL_BACKEND` - Backend to use (openai, vllm, tgi)
- `OPENAI_BASE_URL` - Base URL for API (for custom endpoints)
- `GPTEL_MODEL` - Model to use (default: gpt-4o-mini)
- `IDE_SERVER_LLM_TIMEOUT_SECONDS` - LLM request timeout in seconds (default: 30)
- `IDE_SERVER_MAX_LLM_HISTORY_MESSAGES` - Max conversation history messages sent to LLM (default: 20)
- `IDE_SERVER_RATE_LIMIT_MAX_REQUESTS` - Rate limit max requests per window (default: 100)
- `IDE_SERVER_RATE_LIMIT_WINDOW_SECONDS` - Rate limit window length in seconds (default: 60)

## Troubleshooting

- If `/chat/send` returns connection errors, verify `OPENAI_BASE_URL` and backend server availability.
- If OpenAI requests fail, make sure `OPENAI_API_KEY` is set and valid.
- If responses are slow, tune `IDE_SERVER_LLM_TIMEOUT_SECONDS`.
- Check the Emacs `*IDE Server*` buffer (or server stdout) for detailed server logs.

## Integration with Emacs

The Emacs configuration automatically connects to this server when started. See `init.el` for the integration code.
