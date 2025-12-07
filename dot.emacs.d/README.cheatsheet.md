# Emacs IDE Cheat Sheet

Quick reference for the key bindings defined in `init.el`. Press `C-k` inside Emacs at any time to show this list from within the editor.

## Navigation & UI
- `F8` — Toggle Treemacs sidebar
- `M-←` / `M-→` — Previous / next tab
- `M-t` / `M-w` — New / close tab
- `C-c l` — Reset IDE layout (Treemacs left, shell bottom, chat right)

## Git
- `C-x g` — Open Magit status for the current repo

## GPTel / LLM Chat
- `C-c g` — Launch a GPTel chat buffer
- `C-c RET` — Send the current prompt while inside GPTel

## IDE Server (Python-based)
- `C-c i c` — Send message to IDE server chat
- `C-c i a` — Add context directory to IDE server
- `C-c i h` — Check IDE server health
- `C-c i s` — Start IDE server manually
- `C-c i q` — Stop IDE server

## Context Helpers
- `C-c s` — Search configured context directories and insert the result
- `C-c r a` — Add a new context directory
- `C-c r r` — Remove an existing context directory

## Python Virtualenvs
- `C-c v a` — Activate a virtual environment (prompted)
- `C-c v d` — Deactivate the current environment
- `C-c v s` — Show the currently active environment

## IDE Features

This configuration provides an IDE-like experience with:
- **Treemacs** on the left for file navigation
- **Shell/Terminal** at the bottom for command execution
- **Chat/LLM** on the right for AI assistance
- **Python IDE Server** for enhanced features via simple HTTP API

The Python IDE server (`ide-server/server.py`) runs automatically on startup and provides chat, context management, and other IDE features through a simple JSON API.

Keep `init.el` handy for more advanced customization details.

