# 🚀 Enhanced Emacs IDE Cheat Sheet

Quick reference for the key bindings defined in `init.el`. Press `C-k` inside Emacs at any time to show this list from within the editor.

## Autocompletion
- Completions appear **automatically** while typing (after 2 characters)
- `TAB` — Accept completion or cycle forward
- `S-TAB` — Cycle backward through completions
- `RET` (Enter) — Insert selected completion
- `ESC` — Cancel completion popup

**Documentation popups** appear next to completions after 0.5 seconds showing function signatures, type info, and docstrings.

## Navigation & UI
## 🖥️ Terminal & Shell
- `C-c t` — Open terminal (vterm/ansi-term) - **THE GOOD SHELL!**
- `C-c T` — Open terminal in current directory
- `C-c M-t` — Open terminal in project root

## 🗂️ Navigation & Files  
- `F8` — Toggle Treemacs sidebar
<<<<<<< HEAD
- `C-x C-f` — Find file (enhanced with counsel)
- `C-c f` — Recent files
- `C-s` — Search in buffer (swiper)
- `M-x` — Command palette (enhanced)
- `C-x C-b` — Buffer list (organized with ibuffer)

## 📑 Tabs & Windows
- `M-←/→/↑/↓` — Move between windows
- `C-|` — Split window vertically (side-by-side)
- `C--` — Split window horizontally (top/bottom)
- `M-PgUp` / `M-PgDn` — Previous / next tab
- `M-t` / `M-w` — New / close tab

## LSP (Language Server) Commands
- `C-c l g g` — Go to definition
- `C-c l g r` — Find references
- `C-c l r r` — Rename symbol
- `C-c l h h` — Show documentation (hover info)
- `C-c l =` — Format buffer/region

## Projects & Git
- `C-x p` — Project commands (find file, switch project, etc.)
- `C-c g` — Git grep (counsel)

## 🤖 GPTel / LLM Chat
- `C-c C-g` — Launch a GPTel chat buffer
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

## 🐍 Python Virtualenvs
- `C-c v a` — Activate a virtual environment (prompted)
- `C-c v d` — Deactivate the current environment
- `C-c v s` — Show the currently active environment

## Setup Notes
See `AUTOCOMPLETE_SETUP.md` for full setup instructions including:
- Installing language servers (pyright for Python, clangd for C/C++, bash-language-server)
- Ubuntu 24.04 specific instructions
- Testing and troubleshooting tips
## IDE Features

This configuration provides an IDE-like experience with:
- **Treemacs** on the left for file navigation
- **Shell/Terminal** at the bottom for command execution
- **Chat/LLM** on the right for AI assistance
- **Python IDE Server** for enhanced features via simple HTTP API

The Python IDE server (`ide-server/server.py`) runs automatically on startup and provides chat, context management, and other IDE features through a simple JSON API.

Keep `init.el` handy for more advanced customization details.

## 💡 Help & Discovery
- `C-k` — Show this cheat sheet
- `C-h k` — Describe key
- `C-h f` — Describe function
- **Wait 0.5s** — Which-key popup shows available keys

## ✨ New Features
- **Modern doom-one theme** with enhanced modeline
- **Git gutter** shows changes in fringe
- **Line highlighting** and bracket matching
- **Smart completion** with ivy/counsel/swiper
- **Real terminal experience** with vterm
- **Enhanced project management** with better indexing
- **Visual buffer organization** with grouped ibuffer

---

**No more shell frustration!** The terminal now behaves like your normal system shell instead of that "fucked up mix of emacs and bash" 🎉
## Python LSP (Jedi)
Jedi Language Server is containerized for reliable installation.

**Setup:**
```bash
cd jedi-container
./setup-jedi.sh
```

This installs jedi-language-server to `~/.venv/jedi/` which is automatically detected by Emacs.

Keep `init.el` handy for more advanced customization details.
