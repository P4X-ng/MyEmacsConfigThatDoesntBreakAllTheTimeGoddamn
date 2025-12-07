# Emacs IDE Cheat Sheet

Quick reference for the key bindings defined in `init.el`. Press `C-k` inside Emacs at any time to show this list from within the editor.

## Navigation & UI
- `F8` — Toggle Treemacs sidebar
- `M-←` / `M-→` — Previous / next tab
- `M-t` / `M-w` — New / close tab

## Git
- `C-x g` — Open Magit status for the current repo

## GPTel / LLM Chat
- `C-c g` — Launch a GPTel chat buffer
- `C-c RET` — Send the current prompt while inside GPTel

## Context Helpers
- `C-c s` — Search configured context directories and insert the result
- `C-c r a` — Add a new context directory
- `C-c r r` — Remove an existing context directory

## Python Virtualenvs
- `C-c v a` — Activate a virtual environment (prompted)
- `C-c v d` — Deactivate the current environment
- `C-c v s` — Show the currently active environment

## Python LSP (Jedi)
Jedi Language Server is containerized for reliable installation.

**Setup:**
```bash
cd jedi-container
./setup-jedi.sh
```

This installs jedi-language-server to `~/.venv/jedi/` which is automatically detected by Emacs.

Keep `init.el` handy for more advanced customization details.
