# Contributing to MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn

Thank you for your interest in contributing to this Emacs configuration! This project aims to provide a stable, VSCode-like Emacs experience that "doesn't break all the time."

## How to Contribute

### Reporting Issues

If you encounter any bugs or have suggestions:

1. Check if the issue already exists in the [issue tracker](https://github.com/P4X-ng/MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn/issues)
2. If not, create a new issue with:
   - A clear, descriptive title
   - Steps to reproduce (if it's a bug)
   - Expected vs. actual behavior
   - Your Emacs version and OS
   - Relevant portions of your `*Messages*` buffer

### Suggesting Enhancements

Enhancement suggestions are welcome! Please:

1. Check existing issues and pull requests first
2. Explain the use case and why it would benefit users
3. Keep suggestions focused on stability and usability
4. Consider if it aligns with the "doesn't break all the time" philosophy

### Pull Requests

We welcome pull requests! Here's how to submit one:

1. **Fork the repository** and create a branch from `main`
2. **Make your changes** with clear, focused commits
3. **Test thoroughly** - ensure your changes don't break existing functionality
4. **Update documentation** if you're adding new features or changing behavior
5. **Follow the existing code style** - look at existing configuration patterns
6. **Submit the PR** with a clear description of what and why

#### Guidelines for Code Changes

- **Minimal and Focused**: Make the smallest possible changes to achieve your goal
- **Backward Compatible**: Don't break existing workflows or keybindings unless absolutely necessary
- **Well-Tested**: Test your changes in both terminal and GUI Emacs
- **Documented**: Add comments for complex configurations
- **Stable Packages**: Prefer well-maintained, stable packages over experimental ones

#### Configuration Style

- Use `straight.el` for package management
- Group related configurations together
- Add comments explaining non-obvious settings
- Keep keybindings organized and documented
- Preserve standard Emacs keybindings when possible

### Testing Your Changes

Before submitting a PR:

1. Start Emacs with your changes in a clean environment:
   ```bash
   # Back up your config
   mv ~/.emacs.d ~/.emacs.d.backup
   
   # Test with your changes
   ln -s /path/to/your/fork/dot.emacs.d ~/.emacs.d
   emacs
   ```

2. Test in both GUI and terminal mode
3. Verify LSP functionality for supported languages
4. Check that all keybindings work as expected
5. Ensure no errors in `*Messages*` buffer

### What We're Looking For

Contributions that align with our goals:

- ✅ Bug fixes that improve stability
- ✅ Performance improvements
- ✅ Better documentation
- ✅ Additional language support (with LSP)
- ✅ UI/UX improvements that maintain the VSCode-like feel
- ✅ Better error handling and recovery

### What We're Not Looking For

- ❌ Breaking changes without strong justification
- ❌ Experimental or unstable packages
- ❌ Features that significantly increase startup time
- ❌ Heavy project management features (this config intentionally avoids them)
- ❌ Changes that break terminal compatibility

## Development Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/P4X-ng/MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn.git
   ```

2. Create a test environment:
   ```bash
   # Link your development version
   ln -s $(pwd)/dot.emacs.d ~/.emacs.d.test
   ```

3. Test with:
   ```bash
   emacs --init-directory=~/.emacs.d.test
   ```

## Code Review Process

1. A maintainer will review your PR
2. Feedback will be provided if changes are needed
3. Once approved, your PR will be merged
4. Your contribution will be recognized in the CHANGELOG

## Getting Help

- Open an issue for questions about contributing
- Tag your issue with `question` label
- Be patient - this is a personal project maintained in spare time

## Code of Conduct

Be respectful, constructive, and helpful. We're all here to make Emacs better!

## License

By contributing, you agree that your contributions will be licensed under the same terms as the project (see LICENSE.md).

---

Thank you for helping make this Emacs configuration more stable and user-friendly!
