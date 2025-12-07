# Autocompletion Demo Examples

This directory contains example files to test and demonstrate the autocompletion features in the Emacs configuration.

## Files

- **demo_python.py** - Python example showing type hints, imports, and function completions
- **demo_c.c** - C example showing standard library, struct members, and custom function completions
- **demo_bash.sh** - Bash script example showing commands, variables, and function completions

## How to Use

1. Make sure you've installed the language servers as described in `AUTOCOMPLETE_SETUP.md`:
   - Python: `sudo npm install -g pyright`
   - C/C++: `sudo apt install -y clangd`
   - Bash: `sudo npm install -g bash-language-server`

2. Open any of these demo files in Emacs:
   ```bash
   emacs examples/demo_python.py
   # or
   emacs examples/demo_c.c
   # or
   emacs examples/demo_bash.sh
   ```

3. Try typing the suggestions in the comments:
   - Start typing a partial identifier
   - Completions should appear automatically after 2 characters (0.2s delay)
   - Press TAB to accept or cycle through completions
   - Documentation should appear next to completions after 0.5s

## What to Look For

### Python (demo_python.py)
- Type `os.` - see methods like `getcwd()`, `listdir()`, etc.
- Type `sys.` - see methods like `exit()`, `argv`, etc.
- Type `calculate_` - should complete to `calculate_sum`
- Hover over functions to see their docstrings

### C (demo_c.c)
- Type `prin` - should complete to `printf`
- Type `str` - see string.h functions
- Type `record.` - see struct member completions (id, name, value)
- Type `fact` - should complete to `factorial`

### Bash (demo_bash.sh)
- Type `ech` - should complete to `echo`
- Type variable names after `$` - should complete
- Type function names - should complete to `calculate_sum`
- Common commands should autocomplete

## Troubleshooting

If completions don't appear:

1. Check that LSP started:
   - Look at the bottom of Emacs for LSP indicators
   - Press `C-c l` to access LSP commands

2. Verify language server is installed:
   ```bash
   which pyright  # for Python
   which clangd   # for C
   which bash-language-server  # for Bash
   ```

3. Check the `*Messages*` buffer in Emacs (`M-x messages`) for errors

4. Try manually starting LSP: `M-x lsp`

See `AUTOCOMPLETE_SETUP.md` for more detailed troubleshooting.
