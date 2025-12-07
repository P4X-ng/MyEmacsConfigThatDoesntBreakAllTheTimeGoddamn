# Emacs IDE Cheat Sheet

Quick reference for the key bindings defined in `init.el`. Press `C-k` inside Emacs at any time to show this list from within the editor.

## Autocompletion
- Completions appear **automatically** while typing (after 2 characters)
- `TAB` — Accept completion or cycle forward
- `S-TAB` — Cycle backward through completions
- `RET` (Enter) — Insert selected completion
- `ESC` — Cancel completion popup

**Documentation popups** appear next to completions after 0.5 seconds showing function signatures, type info, and docstrings.

## Navigation & UI
- `F8` — Toggle Treemacs sidebar
- `M-←` / `M-→` — Previous / next tab
- `M-t` / `M-w` — New / close tab

## LSP (Language Server) Commands
- `C-c l g g` — Go to definition
- `C-c l g r` — Find references
- `C-c l r r` — Rename symbol
- `C-c l h h` — Show documentation (hover info)
- `C-c l =` — Format buffer/region

## Projects & Git
- `C-c p` — Projectile command map
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

## Setup Notes
See `AUTOCOMPLETE_SETUP.md` for full setup instructions including:
- Installing language servers (pyright for Python, clangd for C/C++, bash-language-server)
- Ubuntu 24.04 specific instructions
- Testing and troubleshooting tips

Keep `init.el` handy for more advanced customization details.
