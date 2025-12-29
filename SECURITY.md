# Security Policy

## Supported Versions

This Emacs configuration is a personal project that is actively maintained. Security updates will be applied to the latest version on the `main` branch.

| Version | Supported          |
| ------- | ------------------ |
| Latest (main branch) | :white_check_mark: |
| Older commits | :x: |

## Security Considerations

This repository contains an Emacs configuration with package management and language server integrations. While Emacs configurations themselves are typically low-risk, please be aware of the following:

### Package Sources
- This configuration uses `straight.el` to install packages from official repositories (GNU ELPA, MELPA, GitHub)
- Packages are pulled from trusted sources, but you should review any modifications to package sources in `init.el`

### Language Servers
- The configuration integrates with external language servers (clangd, bash-language-server, pyright, etc.)
- These are system-level installations, not managed by this repository
- Keep your language servers updated through your system package manager

### External Tools
- The configuration may interact with external tools (git, formatters, etc.)
- Ensure these tools are from trusted sources and kept up to date

## Reporting a Vulnerability

If you discover a security vulnerability in this Emacs configuration, please report it by:

1. **Do not** open a public issue for security vulnerabilities
2. Open a private security advisory on GitHub by going to the "Security" tab and clicking "Report a vulnerability"
3. Alternatively, create a private issue and mark it as a security concern

### What to Include

When reporting a security issue, please include:

- Description of the vulnerability
- Steps to reproduce the issue
- Potential impact
- Suggested fix (if you have one)
- Your Emacs version and operating system

### Response Time

As this is a personal project:
- I will acknowledge receipt of your report within 48 hours
- I will investigate and provide an initial assessment within 1 week
- I will work to address confirmed vulnerabilities as quickly as possible
- You will be notified when a fix is available

## Security Best Practices for Users

When using this Emacs configuration:

1. **Review before use**: Read through `init.el` to understand what it does
2. **Keep Emacs updated**: Use a recent version of Emacs (27.1+, 29+ recommended)
3. **Update packages regularly**: Run `M-x straight-pull-all` periodically
4. **Vet external tools**: Only install language servers and formatters from trusted sources
5. **Use virtual environments**: For Python development, use virtual environments to isolate dependencies
6. **Be cautious with eval**: Be careful when evaluating untrusted Emacs Lisp code

## Known Security Considerations

### GPTel Integration
- This configuration includes GPTel for ChatGPT integration
- API keys should be stored securely (not committed to version control)
- Be mindful of what code/data you send to external AI services

### File System Access
- Emacs has full access to your file system
- Be careful when opening files from untrusted sources
- Review any code before evaluating it

## Security Updates

Security-related updates will be:
- Documented in CHANGELOG.md with a `[SECURITY]` tag
- Mentioned in commit messages
- Highlighted in release notes (if applicable)

## Additional Resources

- [Emacs Security](https://www.gnu.org/software/emacs/manual/html_node/emacs/Security.html)
- [straight.el Security](https://github.com/raxod502/straight.el#security)
- [LSP Mode Security Considerations](https://emacs-lsp.github.io/lsp-mode/page/security/)

---

Thank you for helping keep this project secure!
