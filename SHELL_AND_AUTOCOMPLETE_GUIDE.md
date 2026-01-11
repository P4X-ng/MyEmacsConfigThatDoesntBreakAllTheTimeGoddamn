# Shell and Autocomplete Guide

This guide addresses the two most common questions about this Emacs configuration:
1. **"Is there a better shell than eshell?"** - YES! We now have shell-pop
2. **"Why isn't autocomplete working?"** - Let's fix that!

## 🖥️ Better Shell Experience (NEW!)

### Quick Answer: Use F9 for Shell-Pop! ⭐

We've added **shell-pop**, which provides a much better shell experience than eshell:

#### What is Shell-Pop?
- A quick, toggleable terminal that slides in from the bottom of your screen
- Works like modern IDE terminals (VS Code, IntelliJ, etc.)
- Uses vterm (best terminal emulation) when available
- Falls back to ansi-term or eshell if vterm isn't available

#### How to Use Shell-Pop

**Quick Access:**
- Press **F9** to toggle the shell popup (recommended!)
- Or press **C-`** (Control + backtick) as an alternative

**Features:**
- **Automatic directory**: Opens in your current file's directory
- **Quick toggle**: Press F9 again to hide the shell
- **Full width**: Spans the entire bottom of your frame
- **Clean integration**: Preserves your window layout when closed

### Shell Comparison Table

| Shell Type | Quality | Access Method | Best For |
|------------|---------|---------------|----------|
| **shell-pop with vterm** | ⭐⭐⭐⭐⭐ | F9 or C-\` | Everything! Quick terminal access |
| vterm (standalone) | ⭐⭐⭐⭐ | C-c t | Full-screen terminal work |
| ansi-term | ⭐⭐⭐ | C-c t (fallback) | When vterm won't compile |
| eshell | ⭐⭐ | M-x eshell | Emacs-native shell scripting |

### Shell-Pop Tips

1. **First Time Setup**: Shell-pop automatically detects the best available shell:
   - First tries vterm (requires compilation)
   - Falls back to ansi-term
   - Last resort: eshell

2. **If vterm compilation fails**: Don't worry! Shell-pop will still work with ansi-term, which is much better than eshell.

3. **Directory handling**: Shell-pop automatically changes to your current file's directory. Perfect for quick commands!

4. **Multiple shells**: You can have both shell-pop (for quick access) AND regular terminals (C-c t) open at the same time.

### Traditional Shell Options Still Available

We kept all the original shell options too:

- **C-c t** - Open a terminal in a new buffer
- **C-c T** - Open terminal in current directory
- **C-c M-t** - Open terminal in project root

## 🎯 Autocomplete Troubleshooting

### Quick Diagnostic

Press **C-k** to see the keybindings cheat sheet - it includes autocomplete help.

### Why Autocomplete Might Not Work

#### 1. **Language Server Not Installed**

Autocomplete requires language servers to be installed:

**Check if installed:**
```bash
# For Python (Jedi)
ls ~/.venv/jedi/bin/jedi-language-server

# For C/C++
which clangd

# For Bash
which bash-language-server

# For TypeScript/JavaScript
which typescript-language-server
```

**Install missing servers:**
```bash
# Python (Jedi) - Use containerized approach
./scripts/build-jedi.sh
./scripts/deploy-jedi.sh

# C/C++
sudo apt install clangd

# Bash
sudo npm install -g bash-language-server

# TypeScript/JavaScript
sudo npm install -g typescript-language-server typescript
```

#### 2. **LSP Not Starting**

**Check if LSP started:**
- Open a file (e.g., `.py`, `.c`, `.sh`)
- Look at the mode line (bottom of screen) - you should see "LSP" indicators
- Press `C-c l` - if it shows LSP commands, LSP is running

**If LSP not starting:**
```elisp
M-x lsp-doctor  ; Run LSP diagnostics
M-x lsp         ; Manually start LSP
```

**Check Messages buffer:**
```elisp
M-x messages    ; See any error messages
```

#### 3. **Corfu or Company Not Active**

This configuration uses BOTH Corfu and Company for maximum reliability.

**Check if active:**
```elisp
M-x corfu-mode          ; Toggle corfu
M-x company-mode        ; Toggle company
M-x global-corfu-mode   ; Toggle corfu globally
M-x global-company-mode ; Toggle company globally
```

All should return "enabled" status.

#### 4. **Wrong File Type**

Autocomplete only works for supported languages:
- Python (`.py`)
- C/C++ (`.c`, `.cpp`, `.h`, `.hpp`)
- Bash (`.sh`)
- TypeScript/JavaScript (`.ts`, `.js`, `.tsx`, `.jsx`)

**Verify mode:**
- Check the mode line - it should show `Python`, `C`, `Bash`, etc.
- If it says "Fundamental", the file type wasn't detected

**Fix:**
```elisp
M-x python-mode    ; For Python files
M-x c-mode         ; For C files
M-x sh-mode        ; For Bash files
```

### Testing Autocomplete

#### Test Python Autocomplete

Create a file `test.py`:
```python
import os

# Type 'os.' and wait - you should see completions
os.
```

#### Test C Autocomplete

Create a file `test.c`:
```c
#include <stdio.h>

int main() {
    // Type 'prin' - you should see 'printf'
    prin
    return 0;
}
```

#### Test Bash Autocomplete

Create a file `test.sh`:
```bash
#!/bin/bash

# Type 'ec' - you should see 'echo'
ec
```

### Autocomplete Keybindings

**Automatic (default):**
- Completions appear automatically after typing 2 characters
- Wait 0.2 seconds for popup to appear

**Manual trigger:**
- **C-TAB** (Ctrl+Tab) - Force completion popup

**Navigation:**
- **TAB** - Accept selection or cycle forward
- **S-TAB** (Shift+Tab) - Cycle backward
- **C-n** / **C-p** - Next/Previous (in company popup)
- **M-1** through **M-9** - Quick select (company mode)
- **RET** (Enter) - Insert completion
- **ESC** - Cancel popup

### Advanced: Autocomplete Configuration

The configuration in `init.el` has two completion systems:

**Corfu (Primary):**
- Modern, minimal completion UI
- Shows inline in buffer
- Works with LSP via `completion-at-point`

**Company (Backup):**
- Robust, proven completion framework
- Shows popup with more information
- Works even if Corfu has issues

**Both are enabled** and work together. If one fails, the other provides completions.

### Still Not Working?

1. **Restart Emacs** after installing language servers

2. **Check LSP logs:**
   ```elisp
   M-x lsp-workspace-show-log
   ```

3. **Increase LSP delay** (if too aggressive):
   ```elisp
   M-x customize-variable RET lsp-idle-delay RET
   ; Set to 0.5 or 1.0
   ```

4. **Check if file is too large:**
   - LSP might skip very large files
   - Try with a smaller test file first

5. **Verify Emacs version:**
   ```bash
   emacs --version  # Should be 27.1 or newer
   ```

6. **Check for conflicting packages:**
   ```elisp
   M-x describe-mode  ; See what modes are active
   ```

## 📚 Additional Resources

- **Main README**: See [README.md](README.md) for installation instructions
- **Autocomplete Setup**: See [AUTOCOMPLETE_SETUP.md](AUTOCOMPLETE_SETUP.md) for detailed language server setup
- **Autocomplete Summary**: See [AUTOCOMPLETE_SUMMARY.md](AUTOCOMPLETE_SUMMARY.md) for feature overview
- **Cheat Sheet**: Press `C-k` in Emacs to see all keybindings

## 🆘 Getting Help

If you're still having issues:

1. Open an issue on GitHub with:
   - What you tried
   - Error messages from `*Messages*` buffer
   - Output of `M-x lsp-doctor` (if relevant)
   - Your Emacs version (`M-x emacs-version`)

2. Common solutions:
   - Delete `~/.emacs.d/straight/` and restart Emacs
   - Run `M-x package-refresh-contents`
   - Check your internet connection for package downloads

## 💡 Pro Tips

### Shell Pro Tips
- **Use F9 religiously** - it's the fastest way to run quick commands
- **C-c t for long sessions** - when you need a full terminal
- **Combine both** - Use F9 for quick commands, C-c t for interactive work

### Autocomplete Pro Tips
- **Wait a moment** - Completions appear after 0.2 seconds
- **Type more characters** - More specific = better completions
- **Use C-TAB** - Force completions if they don't appear
- **Check mode line** - Make sure LSP is active (look for "LSP" indicator)
- **Install ALL language servers** - Even if you only use one language now

### Quality of Life
- **Press C-k often** - The cheat sheet is always there to help
- **Use M-x which-key-show-top-level** - See all available prefixes
- **Explore LSP commands** - Press `C-c l` and explore what's available
