# Autocompletion Demo Examples

This directory contains example files to test and demonstrate the autocompletion features in the Emacs configuration.

**Note**: Python autocompletion is handled separately via Jedi in a containerized environment, so no Python demo is included here.

## Files

- **demo_c.c** - C example showing standard library, struct members, and custom function completions
- **demo_bash.sh** - Bash script example showing commands, variables, and function completions
- **demo_typescript.ts** - TypeScript example showing types, interfaces, classes, and async/await completions
- **Makefile** - Makefile example showing variable names, targets, and file path completions
- **CMakeLists.txt** - CMake example showing executable definitions and project settings

## How to Use

1. Make sure you've installed the language servers as described in `AUTOCOMPLETE_SETUP.md`:
  - C/C++: `sudo apt install -y clangd`
  - Bash: `sudo npm install -g bash-language-server`
  - TypeScript/JavaScript: `sudo npm install -g typescript-language-server typescript`
  - Build tools: `sudo apt install -y build-essential cmake`

2. Open any of these demo files in Emacs:
   ```bash
   emacs examples/demo_c.c
   # or
   emacs examples/demo_bash.sh
   # or
   emacs examples/demo_typescript.ts
   ```

3. Try typing the suggestions in the comments:
   - Start typing a partial identifier
   - Completions should appear automatically after 1 character (about 0.1s delay)
   - Preview text (ghost text) shows inline
   - Press TAB to accept the current completion
   - Documentation should appear next to completions after 0.5s

## What to Look For

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

### TypeScript (demo_typescript.ts)
- Type `user.` - should show object properties (id, name, email, isActive)
- Type `calc.` - should show class methods (add, subtract, multiply, divide)
- Type `Math.` - should show built-in Math methods
- Type partial function names - should complete to full function names

### Makefile (Makefile)
- Type `$(C` - should complete to variable names like `$(CC)`, `$(CFLAGS)`
- Type `$(O` - should complete to `$(OBJECTS)`
- Type target names - should complete to available targets (all, clean, install, test)
- Type file paths - should show file path completions

### CMake (CMakeLists.txt)
- Open the file and confirm Emacs switches to `cmake-mode`
- Type `add_ex` - you should be able to complete toward `add_executable`
- Type `set(` - CMake syntax highlighting and indentation should stay consistent
- Use `C-c c c` in a project to generate a fresh `CMakeLists.txt` with GPTel

## Build helper commands

For C and C++ projects, the config adds a dedicated helper prefix:

- `C-c c b` - build the current project
- `C-c c c` - generate `CMakeLists.txt`
- `C-c c m` - generate `Makefile`

## Troubleshooting

If completions don't appear:

1. Check that LSP started:
   - Look at the bottom of Emacs for LSP indicators
   - Press `C-c l` to access LSP commands

2. Verify language server is installed:
   ```bash
   which clangd                    # for C
   which bash-language-server      # for Bash
   which typescript-language-server # for TypeScript
   ```

3. Check the `*Messages*` buffer in Emacs (`M-x messages`) for errors

4. Try manually starting LSP: `M-x lsp`

See `AUTOCOMPLETE_SETUP.md` for more detailed troubleshooting.
