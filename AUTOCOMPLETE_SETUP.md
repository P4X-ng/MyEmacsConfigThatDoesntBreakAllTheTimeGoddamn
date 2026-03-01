# Autocomplete Setup Guide (Ubuntu 24.04)

This Emacs configuration provides intelligent autocompletions for C, C++, and Bash through the Language Server Protocol (LSP). 

**Note**: Python autocompletion is handled separately via Jedi in a containerized environment.

## Quick Start

The configuration is already set up! You just need to install the language servers for C/C++ and Bash.

### Prerequisites

```bash
# Update package lists
sudo apt update

# Install Node.js and npm (required for bash-language-server)
sudo apt install -y nodejs npm
```

## Supported Languages

This configuration provides intelligent autocompletions for:
- **C/C++** (via clangd)
- **Bash/Shell** (via bash-language-server)
- **TypeScript/JavaScript** (via typescript-language-server)
- **Python** (handled separately via Jedi in containerized environment)
- **Makefiles** (via built-in completion with file paths and dynamic abbreviations)

## Language Server Installation

### C/C++ (clangd)

```bash
# Install clangd from Ubuntu repositories
sudo apt install -y clangd

# Verify installation
clangd --version
```

For better C/C++ support, you may also want:
```bash
# Install build essentials and C/C++ development tools
sudo apt install -y build-essential cmake
```

### Bash (bash-language-server)

```bash
# Install bash language server
sudo npm install -g bash-language-server

# Verify installation
bash-language-server --version
```

### TypeScript/JavaScript (typescript-language-server)

```bash
# Install TypeScript language server
sudo npm install -g typescript-language-server typescript

# Verify installation
typescript-language-server --version
tsc --version
```

**Optional**: Install prettier for auto-formatting TypeScript/JavaScript files:
```bash
sudo npm install -g prettier

# Verify installation
prettier --version
```

## How It Works

### Completion System

The configuration uses several packages working together:

1. **Corfu**: Modern in-buffer completion popup that auto-shows while typing
   - Triggers automatically after typing 2 characters
   - Shows completions after 2.0 seconds
   - Preview of current candidate shown inline (ghost text)
   - Press TAB to accept or cycle through options

2. **Cape**: Provides additional completion sources
   - Dynamic abbreviations (words from open buffers)
   - File path completions
   - Programming language keywords

3. **Corfu Popupinfo**: Shows documentation next to completions
   - Documentation appears after 0.5 seconds
   - Shows function signatures, type info, docstrings

4. **Kind-icon**: Adds visual icons showing completion type
   - Function, variable, class, module icons
   - Makes it easier to identify what you're completing

5. **LSP-mode**: Integrates with language servers
   - Provides intelligent, context-aware completions
   - Powers go-to-definition, find-references, etc.

### What You Get

When you open a C or Bash file, you'll automatically get:

- **Smart autocompletions** as you type
- **Function signatures** and parameter hints
- **Documentation popups** for functions/methods
- **Type information** for variables and returns
- **Member access** completions (e.g., `struct.member`)
- **Code snippets** (if supported by the language server)

## Testing Your Setup

### Test C Completion

1. Create a test file: `test.c`
2. Type the following:

```c
#include <stdio.h>

int main() {
    // Type 'prin' and you should see printf completion
    prin
    
    return 0;
}
```

### Test Bash Completion

1. Create a test file: `test.sh`
2. Type shell commands and you'll get completions for:
   - Built-in commands
   - Variables
   - Functions

### Test TypeScript Completion

1. Create a test file: `test.ts`
2. Type the following:

```typescript
interface User {
  name: string;
  age: number;
}

const user: User = {
  // Type 'na' and you should see 'name' completion
  name: "Alice",
  age: 30
};

// Type 'user.' and you should see name and age completions
user.
```

## Troubleshooting

### No completions appearing

1. **Check if language server is installed:**
   ```bash
   # For C/C++
   which clangd
   
   # For Bash
   which bash-language-server
   ```

2. **Check if LSP started:**
   - Open a file in the language
   - Look at the bottom of Emacs - you should see LSP indicators
   - Press `C-c l` to access LSP commands

3. **Check Messages buffer:**
   - Press `M-x` then type `messages` and press Enter
   - Look for LSP startup messages or errors

### Completions are slow

- Adjust `corfu-auto-delay` in `init.el` (currently 2.0s, decrease to 1.0 or 0.5 for faster)
- Adjust `lsp-idle-delay` (increase from 0.3 to 0.5)

### LSP not starting

- Make sure the language server executable is in your PATH
- Check that your file has the correct extension (`.c`, `.h`, `.sh`)
- Try manually starting LSP with `M-x lsp`

## Key Bindings for Completion

While typing:
- **TAB** - Complete selected candidate or cycle forward
- **S-TAB** (Shift+Tab) - Cycle backward through completions
- **RET** (Enter) - Insert selected completion
- **ESC** - Cancel completion popup

LSP commands (prefix `C-c l`):
- `C-c l g g` - Go to definition
- `C-c l g r` - Find references
- `C-c l r r` - Rename symbol
- `C-c l h h` - Show documentation (hover info)
- `C-c l =` - Format buffer/region

## Advanced Configuration

All completion settings are in `init.el` in the "Completion + Orderless" section. You can customize:

- `corfu-auto-delay` - How quickly completions appear
- `corfu-auto-prefix` - Minimum characters before completion triggers
- `corfu-cycle` - Whether to cycle through completions
- `lsp-completion-provider` - Currently set to `:none` to use corfu

## Further Reading

- [Corfu documentation](https://github.com/minad/corfu)
- [LSP-mode documentation](https://emacs-lsp.github.io/lsp-mode/)
- [Clangd documentation](https://clangd.llvm.org/)
