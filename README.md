# My Emacs Config That Doesn't Break All The Time Goddamn

A modern, terminal-friendly Emacs IDE configuration with comprehensive autocompletion support for C, C++, Python, and Bash.

## Features

### 🚀 Intelligent Autocompletion
- **Corfu**: Modern in-buffer completion with auto-show
- **LSP Integration**: Full Language Server Protocol support
- **Documentation Popups**: Function signatures and docstrings appear next to completions
- **Visual Icons**: Completion type indicators (function, variable, class, etc.)
- **Multiple Sources**: Code, files, keywords, and dynamic abbreviations

### 💻 Language Support
- **Python**: Pyright language server (NOT Jedi - faster and more accurate)
- **C/C++**: clangd language server with full IDE features
- **Bash**: bash-language-server for shell scripts
- **Auto-formatting**: Supports black (Python), clang-format (C/C++), shfmt (Bash)

### 🛠️ Development Tools
- **Syntax Checking**: Real-time with Flycheck
- **Git Integration**: Magit for version control
- **Project Management**: Built-in project.el support
- **File Explorer**: Treemacs sidebar (toggle with F8)
- **Virtual Environments**: Auto-detection for Python venvs
- **LLM Chat**: GPTel integration for AI assistance

### 🎨 UI Features
- Terminal-friendly (works in terminal and GUI)
- Tab-bar mode for multiple tabs (M-t for new, M-w to close)
- Clean, minimal UI with no distractions
- Customizable keybindings
- Built-in cheat sheet (press C-k)

## Quick Start

### 1. Install Emacs
```bash
sudo apt update
sudo apt install -y emacs
```

### 2. Clone This Repository
```bash
git clone https://github.com/P4X-ng/MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn.git
cd MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn
```

### 3. Link Configuration
```bash
# Backup existing config if you have one
mv ~/.emacs.d ~/.emacs.d.backup

# Create symlink to this config
ln -s $(pwd)/dot.emacs.d ~/.emacs.d
```

### 4. Install Language Servers (for autocompletion)
```bash
# Prerequisites
sudo apt install -y nodejs npm

# Python language server
sudo npm install -g pyright

# C/C++ language server
sudo apt install -y clangd

# Bash language server (optional)
sudo npm install -g bash-language-server
```

### 5. Launch Emacs
```bash
emacs
```

First launch will install all packages automatically (takes a few minutes).

## Documentation

- **[AUTOCOMPLETE_SETUP.md](AUTOCOMPLETE_SETUP.md)** - Comprehensive setup guide for Ubuntu 24.04
- **[AUTOCOMPLETE_SUMMARY.md](AUTOCOMPLETE_SUMMARY.md)** - Feature overview and quick reference
- **[dot.emacs.d/README.cheatsheet.md](dot.emacs.d/README.cheatsheet.md)** - Keybinding reference
- **[examples/](examples/)** - Demo files to test autocompletion

## Testing Autocompletion

Try the demo files to see autocompletion in action:

```bash
emacs examples/demo_python.py  # Python examples
emacs examples/demo_c.c        # C examples
emacs examples/demo_bash.sh    # Bash examples
```

Follow the comments in each file for what to type to trigger completions.

## Key Bindings

### Autocompletion
- Completions appear **automatically** after typing 2 characters
- **TAB** - Accept or cycle forward
- **S-TAB** - Cycle backward
- **RET** - Insert completion
- **ESC** - Cancel

### LSP Commands (C-c l prefix)
- **C-c l g g** - Go to definition
- **C-c l g r** - Find references
- **C-c l r r** - Rename symbol
- **C-c l h h** - Show documentation
- **C-c l =** - Format code

### Navigation
- **F8** - Toggle file explorer (Treemacs)
- **M-←/M-→** - Switch tabs
- **M-t** - New tab
- **M-w** - Close tab

### Git
- **C-x g** - Open Magit status

### Help
- **C-k** - Show keybinding cheat sheet

## Configuration Structure

```
dot.emacs.d/
├── init.el              # Main configuration file
├── README.cheatsheet.md # Keybinding reference
├── straight/            # Package management (auto-generated)
└── .cache/             # Cache files (auto-generated)

examples/
├── demo_python.py       # Python demo
├── demo_c.c            # C demo
├── demo_bash.sh        # Bash demo
└── README.md           # Examples guide
```

## Customization

The main configuration is in `dot.emacs.d/init.el`. It's organized into clear sections:

- **Completion + Orderless** - Autocompletion settings
- **Syntax checking + LSP** - Language server configuration
- **Projects & Git** - Project and version control
- **GPTel** - LLM chat integration
- **Python venv** - Virtual environment handling
- **Cheat Sheet** - Built-in help

Edit `init.el` to customize behavior, add packages, or modify keybindings.

## Why This Config?

- ✅ **Modern**: Uses latest Emacs best practices (LSP, Corfu, straight.el)
- ✅ **Fast**: Optimized for quick startup and responsive completions
- ✅ **Stable**: Tested and working configuration
- ✅ **Well-documented**: Extensive comments and guides
- ✅ **Terminal-friendly**: Works in both GUI and terminal
- ✅ **No Jedi**: Uses modern pyright instead of slow Jedi
- ✅ **Comprehensive**: Full IDE features out of the box

## Troubleshooting

### No completions appearing?
1. Check language server is installed: `which pyright` / `which clangd`
2. Check LSP started: Look for LSP indicators in mode line
3. Try manually starting: `M-x lsp`
4. See [AUTOCOMPLETE_SETUP.md](AUTOCOMPLETE_SETUP.md) for detailed troubleshooting

### Packages not installing?
1. Check internet connection
2. Delete `~/.emacs.d/straight/` and restart Emacs
3. Check `*Messages*` buffer for errors

### Emacs won't start?
1. Try with minimal config: `emacs -q`
2. Check for syntax errors in `init.el`
3. Restore backup: `mv ~/.emacs.d.backup ~/.emacs.d`

## Requirements

- **Emacs**: Version 27.1 or later (29+ recommended)
- **Node.js**: For pyright and bash-language-server
- **clangd**: For C/C++ support
- **Git**: For package management and Magit

## Contributing

This is a personal configuration, but suggestions are welcome! Open an issue if you find bugs or have improvement ideas.

## License

This configuration is provided as-is. Feel free to use, modify, and share it.

## Additional Resources

- [Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [LSP-mode Documentation](https://emacs-lsp.github.io/lsp-mode/)
- [Corfu GitHub](https://github.com/minad/corfu)
- [Magit Manual](https://magit.vc/manual/)

---

**Note**: This config is designed to "not break all the time" - it uses stable, well-maintained packages and follows best practices. If something breaks, check the troubleshooting sections in the documentation.
