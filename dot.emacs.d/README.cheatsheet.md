# 🚀 Enhanced Emacs IDE Cheat Sheet

Quick reference for the key bindings defined in `init.el`. Press `C-k` inside Emacs at any time to show this list from within the editor.

## 🖥️ Terminal & Shell
- `C-c t` — Open terminal (vterm/ansi-term) - **THE GOOD SHELL!**
- `C-c T` — Open terminal in current directory
- `C-c M-t` — Open terminal in project root

## 🗂️ Navigation & Files  
- `F8` — Toggle Treemacs sidebar
- `C-x C-f` — Find file (enhanced with counsel)
- `C-c f` — Recent files
- `C-s` — Search in buffer (swiper)
- `M-x` — Command palette (enhanced)
- `C-x C-b` — Buffer list (organized with ibuffer)

## 📑 Tabs & Windows
- `M-←` / `M-→` — Previous / next tab
- `M-t` / `M-w` — New / close tab

## 📁 Projects & Git
- `C-c p` — Projectile command map (enhanced)
- `C-c p f` — Find file in project
- `C-c p s g` — Grep in project  
- `C-x g` — Open Magit status for the current repo
- `C-c g` — Git grep (counsel)

## 🤖 GPTel / LLM Chat
- `C-c C-g` — Launch a GPTel chat buffer
- `C-c RET` — Send the current prompt while inside GPTel

## 🔍 Context & Search
- `C-c s` — Search configured context directories and insert the result
- `C-c r a` — Add a new context directory
- `C-c r r` — Remove an existing context directory

## 🐍 Python Virtualenvs
- `C-c v a` — Activate a virtual environment (prompted)
- `C-c v d` — Deactivate the current environment
- `C-c v s` — Show the currently active environment

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

Keep `init.el` handy for more advanced customization details.
