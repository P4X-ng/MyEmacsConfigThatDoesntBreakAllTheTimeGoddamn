# VSCode-like Emacs Configuration

This Emacs configuration provides a VSCode-like experience with all the power of Emacs, but without the memory-heavy project management features.

## 🎨 VSCode-like Features

### Visual Experience
- **Doom Themes**: Modern, beautiful color schemes similar to VSCode themes
- **Doom Modeline**: A sleek status bar that looks like VSCode's status bar
- **All-the-icons**: Beautiful icons in the modeline and file explorer
- **Rainbow Delimiters**: Color-coded brackets for better code readability
- **Highlight Indent Guides**: Visual indent guides like VSCode

### File Navigation & Exploration
- **Treemacs**: A sidebar file explorer (press `F8` to toggle) - just like VSCode's Explorer
- **Vertico + Marginalia**: Enhanced minibuffer completion for better file/buffer selection
- **Quick File Open**: `C-c C-p` to quickly open files (VSCode-inspired)
- **Buffer Switching**: `C-x C-b` to switch between open files (enhanced standard binding)

### Code Intelligence
- **LSP Mode**: Language Server Protocol support for:
  - Python (with Pyright)
  - Bash/Shell
  - C/C++ (with clangd)
- **Corfu**: Auto-completion as you type (like IntelliSense)
- **Cape**: Additional completion backends for files and words
- **Flycheck**: Real-time syntax checking and linting
- **LSP UI**: Inline errors, documentation, and code actions

### Editing Features
- **Multiple Cursors**: Edit multiple locations at once (like VSCode)
  - `C->` / `C-<`: Mark next/previous occurrence
  - `C-S-c C-S-c`: Edit multiple lines
- **Smartparens**: Auto-pairing brackets and quotes
- **Comment Toggle**: `C-/` to comment/uncomment lines
- **Line Movement**: `M-↑` / `M-↓` to move lines up/down
- **Undo Tree**: Better undo/redo system

### Navigation & Discovery
- **Avy**: Jump to any character or line quickly
  - `C-'`: Jump to character
  - `C-;`: Jump to line
- **Which-key**: Shows available keybindings as you type (like VSCode's command palette hints)

### Tab Management
- **Tab Bar**: VSCode-like tabs for managing multiple files
  - `M-←` / `M-→`: Switch between tabs
  - `M-t`: New tab
  - `M-w`: Close tab

### Git Integration
- **Magit**: Powerful Git interface (`C-x g`)

### AI/LLM Integration
- **GPTel**: ChatGPT integration for AI assistance
  - `C-c C-g`: Open GPTel chat

### Auto-formatting
Automatic code formatting on save (when formatters are installed):
- Python: `black`
- Bash/Shell: `shfmt`
- C/C++: `clang-format`

## 🚀 Installation

1. Back up your existing Emacs configuration:
   ```bash
   mv ~/.emacs.d ~/.emacs.d.backup
   ```

2. Clone this repository:
   ```bash
   git clone <repository-url> ~/.emacs.d
   # Or if using the dot.emacs.d structure:
   ln -s /path/to/this/repo/dot.emacs.d ~/.emacs.d
   ```

3. Install Emacs (version 27.1 or higher recommended)

4. Start Emacs - packages will be installed automatically via straight.el

5. (Optional) Install language servers for better code intelligence:
   ```bash
   # Python
   pip install pyright
   
   # Bash
   npm install -g bash-language-server
   
   # C/C++
   # Install clangd from your package manager
   
   # Formatters
   pip install black
   go install mvdan.cc/sh/v3/cmd/shfmt@latest
   # clang-format usually comes with clang
   ```

6. (Optional) If using GUI Emacs, install fonts for icons:
   - Run `M-x all-the-icons-install-fonts`

## ⌨️ Key Keybindings

**Note**: This configuration preserves standard Emacs keybindings (like `C-p` for previous-line, `C-b` for backward-char) and adds VSCode-like alternatives that don't conflict with core Emacs functionality.

### File & Navigation
| Keybinding | Action |
|------------|--------|
| `F8` | Toggle Treemacs sidebar |
| `C-c C-p` | Quick open file (VSCode-like) |
| `C-x C-b` | Switch buffer (enhanced) |
| `C-S-f` | Search in files |

### Tabs
| Keybinding | Action |
|------------|--------|
| `M-←` / `M-→` | Switch tabs |
| `M-t` | New tab |
| `M-w` | Close tab |

### Editing
| Keybinding | Action |
|------------|--------|
| `C-/` | Comment/uncomment line |
| `M-↑` / `M-↓` | Move line up/down |
| `C->` / `C-<` | Mark next/previous like this |
| `C-S-c C-S-c` | Edit multiple lines |

### Navigation
| Keybinding | Action |
|------------|--------|
| `C-'` | Jump to character |
| `C-;` | Jump to line |

### Git
| Keybinding | Action |
|------------|--------|
| `C-x g` | Magit status |

### LSP
| Keybinding | Action |
|------------|--------|
| `C-c l` | LSP command prefix |

### Help
| Keybinding | Action |
|------------|--------|
| `C-k` | Show cheat sheet |
| `C-h k` | Describe key |
| `C-h f` | Describe function |

## 🎯 Why This Configuration?

- **No Projects**: Unlike the default setup, this configuration doesn't use Projectile or project.el, eliminating project-related overhead
- **Memory Efficient**: Uses straight.el for package management and carefully selected packages
- **VSCode Familiar**: Keybindings and UI inspired by VSCode for easier transition
- **Modern Look**: Doom themes and modeline for a contemporary appearance
- **Powerful**: All the extensibility and power of Emacs with a friendly interface

## 🔧 Customization

The main configuration file is `dot.emacs.d/init.el`. You can customize:
- Themes: Change `doom-one` to another doom-theme
- Keybindings: Add or modify in the VSCode-like keybindings section
- LSP servers: Add more language modes in the LSP configuration
- Completion behavior: Adjust corfu settings

## 📦 Included Packages

- `straight.el` - Package manager
- `doom-themes` - Modern color schemes
- `doom-modeline` - Modern status bar
- `all-the-icons` - Icon support
- `which-key` - Keybinding discovery
- `vertico` - Vertical completion UI
- `marginalia` - Rich completion annotations
- `corfu` - In-buffer completion
- `orderless` - Flexible completion style
- `cape` - Completion backends
- `multiple-cursors` - Multi-cursor editing
- `avy` - Quick navigation
- `smartparens` - Bracket pairing
- `rainbow-delimiters` - Colorful brackets
- `highlight-indent-guides` - Indent guides
- `undo-tree` - Better undo system
- `flycheck` - Syntax checking
- `lsp-mode` - Language Server Protocol
- `lsp-ui` - LSP UI enhancements
- `lsp-pyright` - Python LSP
- `treemacs` - File explorer
- `magit` - Git interface
- `gptel` - ChatGPT integration
- `pyvenv` - Python virtual environment support

## 📝 Notes

- First launch will take a few minutes as packages are downloaded and compiled
- Some features (like icons) require GUI Emacs for full effect
- LSP features require language servers to be installed separately
- Auto-formatting requires external formatters (black, shfmt, clang-format)

## 🐛 Troubleshooting

### Icons not showing
Run `M-x all-the-icons-install-fonts` in GUI Emacs

### LSP not working
Make sure language servers are installed and in your PATH

### Slow startup
This is normal on first launch. Subsequent launches should be fast.

### Packages not installing
Delete `~/.emacs.d/straight/` and restart Emacs

## 📄 License

This configuration is provided as-is for personal use.
# My Emacs Config That ~~Doesn't Break All The Time~~ Actually Works Now Goddamn

**STATUS: ✅ FIXED AND STABLE** - Configuration has been thoroughly debugged and stabilized!

A modern, terminal-friendly Emacs IDE configuration with comprehensive autocompletion support for C, C++, Python, and Bash.

## 🎉 What's New - Configuration Fixed!

**All critical issues have been resolved:**
- ✅ Fixed syntax errors (duplicate declarations, unbalanced parentheses)
- ✅ Added comprehensive error handling
- ✅ Graceful fallbacks for missing dependencies
- ✅ Suppressed unnecessary warnings
- ✅ Ensured reliable startup even without optional packages

See [FIXES.md](FIXES.md) for detailed information about what was fixed.

## Features

### 🚀 Intelligent Autocompletion
- **Corfu**: Modern in-buffer completion with auto-show
- **LSP Integration**: Full Language Server Protocol support
- **Documentation Popups**: Function signatures and docstrings appear next to completions
- **Visual Icons**: Completion type indicators (function, variable, class, etc.)
- **Multiple Sources**: Code, files, keywords, and dynamic abbreviations

### 💻 Language Support
- **C/C++**: clangd language server with full IDE features
- **Bash**: bash-language-server for shell scripts
- **Python**: Jedi (handled separately in containerized environment)
- **Auto-formatting**: Supports clang-format (C/C++), shfmt (Bash)

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

# C/C++ language server
sudo apt install -y clangd

# Bash language server (optional)
sudo npm install -g bash-language-server
```

**Note**: Python autocompletion is handled separately via Jedi in a containerized environment.

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
emacs examples/demo_c.c        # C examples
emacs examples/demo_bash.sh    # Bash examples
```

**Note**: Python demo is not included as Python autocompletion is handled separately via Jedi.

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
- ✅ **Stable**: **NOW ACTUALLY STABLE** - Thoroughly tested and debugged!
- ✅ **Resilient**: Handles missing dependencies gracefully
- ✅ **Well-documented**: Extensive comments, guides, and troubleshooting docs
- ✅ **Terminal-friendly**: Works in both GUI and terminal
- ✅ **Comprehensive**: Full IDE features out of the box
- ✅ **Python via Jedi**: Python handled separately in containerized environment
- ✅ **Error Recovery**: Clear error messages, won't crash on startup

## Troubleshooting

### Configuration now includes robust error handling!

The configuration has been fixed to handle common issues gracefully:

- **Syntax errors**: All fixed! Parentheses balanced, no duplicates
- **Missing dependencies**: Emacs will start and degrade gracefully
- **Terminal issues**: Automatic fallback (vterm → ansi-term → eshell)
- **LSP failures**: Clear messages, won't prevent startup

For detailed information about fixes, see **[FIXES.md](FIXES.md)**

### No completions appearing?
1. Check language server is installed: `which clangd` / `which bash-language-server`
2. Check LSP started: Look for LSP indicators in mode line
3. Try manually starting: `M-x lsp`
4. See [AUTOCOMPLETE_SETUP.md](AUTOCOMPLETE_SETUP.md) for detailed troubleshooting

**Note**: For Python autocompletion issues, check the Jedi containerized setup.

### Packages not installing?
1. Check internet connection
2. Delete `~/.emacs.d/straight/` and restart Emacs
3. Check `*Messages*` buffer for errors

### Emacs won't start?
**This should be fixed now!** But if issues persist:
1. Try with minimal config: `emacs -q`
2. Try with debug mode: `emacs --debug-init`
3. Check FIXES.md for what was fixed
4. Check for syntax errors in `init.el`

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

**Note**: This config is now **truly stable** and "doesn't break all the time" - it uses stable, well-maintained packages, follows best practices, and has comprehensive error handling. If something breaks, check [FIXES.md](FIXES.md) and the troubleshooting sections in the documentation.

## 🛡️ Stability Improvements

This configuration now includes:
- **Syntax validation** - All parentheses balanced, no duplicate declarations
- **Error recovery** - Graceful handling of missing dependencies
- **Clear error messages** - Know exactly what went wrong
- **Resilient startup** - Emacs starts even if optional features fail
- **Automated testing** - Syntax checks to prevent future breaks

For technical details, see [FIXES.md](FIXES.md).
# Reliable Jedi - Containerized Python Code Intelligence

This repository provides a bulletproof, containerized Jedi setup that can be reliably deployed to any Python virtual environment.

## Quick Start

```bash
# Build the Jedi container
./scripts/build-jedi.sh

# Deploy to your current venv
./scripts/deploy-jedi.sh

# Or deploy to a specific venv
./scripts/deploy-jedi.sh /path/to/your/venv
```

## What This Provides

- **Containerized Jedi**: Pre-built container with Jedi, language server, and all dependencies
- **Turnkey Deployment**: One-command deployment to any virtual environment
- **Emacs Integration**: Updated configuration for seamless Jedi integration
- **Zero Installation Issues**: No more pip install headaches

## Architecture

1. **Docker Container**: Contains Jedi language server and all Python analysis tools
2. **Deployment Scripts**: Copy/mount container contents into venv paths
3. **Emacs Configuration**: Updated to use the containerized Jedi
4. **Health Checks**: Automatic validation of Jedi functionality

## Files

- `docker/`: Dockerfile and container configuration
- `scripts/`: Build and deployment scripts
- `dot.emacs.d/`: Updated Emacs configuration with Jedi integration
- `config/`: Jedi and language server configurations

## Requirements

- Docker
- Python virtual environments
- Emacs (optional, but recommended)

## Troubleshooting

If Jedi isn't working:
1. Run `./scripts/health-check.sh` to validate the installation
2. Check `./logs/jedi.log` for error messages
3. Rebuild with `./scripts/build-jedi.sh --force`
