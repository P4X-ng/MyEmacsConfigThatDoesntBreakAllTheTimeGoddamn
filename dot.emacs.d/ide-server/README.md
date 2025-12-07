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

## Integration with Emacs

The Emacs configuration automatically connects to this server when started. See `init.el` for the integration code.
