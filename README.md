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
