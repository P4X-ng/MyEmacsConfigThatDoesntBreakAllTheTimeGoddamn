# Security Policy

## Overview

This repository contains an Emacs configuration that includes various packages and integrations. While Emacs configurations themselves typically don't have traditional security vulnerabilities like web applications, there are still security considerations to be aware of.

## Supported Versions

We support the latest version of the configuration. We recommend always using the latest commit from the `main` branch for the most up-to-date and secure configuration.

| Version | Supported          |
| ------- | ------------------ |
| Latest (main branch) | ✅ |
| Older commits | ⚠️ Use at own risk |

## Security Considerations

### Package Security

- **Package Sources**: We use `straight.el` to manage packages directly from Git repositories
- **Package Updates**: Regularly update packages to get security fixes
- **Trusted Sources**: All packages are sourced from reputable repositories (GNU ELPA, MELPA, GitHub)

### Code Execution

- **Elisp Code**: Emacs configurations involve executing Elisp code
- **Third-party Packages**: We only include well-maintained, widely-used packages
- **Review Changes**: Always review configuration changes before applying them

### API Keys and Credentials

- **GPTel Integration**: If using the GPTel (AI/LLM) integration, API keys must be stored securely
- **Never commit API keys**: Do not commit API keys or credentials to this repository
- **Use Environment Variables**: Store sensitive credentials in environment variables or secure key stores
- **Example**: Set GPTel API keys via environment variables or Emacs' `auth-sources`

### Language Server Protocol (LSP)

- **LSP Servers**: This configuration uses LSP servers (Pyright, clangd, bash-language-server)
- **Network Communication**: LSP servers may communicate over network sockets
- **Trust LSP Servers**: Only install LSP servers from trusted sources
- **Workspace Trust**: Be cautious when opening untrusted projects with LSP enabled

## Reporting a Vulnerability

If you discover a security vulnerability in this configuration, please report it responsibly:

### How to Report

1. **Do NOT** open a public GitHub issue for security vulnerabilities
2. **Do** report security issues by:
   - Opening a private security advisory on GitHub (if available)
   - Creating a regular issue with minimal details and requesting private communication
   - Contacting the repository owner directly through GitHub

### What to Include

When reporting a security vulnerability, please include:

- **Description**: A clear description of the vulnerability
- **Impact**: What could an attacker do with this vulnerability?
- **Reproduction Steps**: Step-by-step instructions to reproduce the issue
- **Affected Components**: Which parts of the configuration are affected?
- **Suggested Fix**: If you have ideas for fixing the issue (optional)
- **Your Contact Info**: How we can reach you for follow-up questions

### What to Expect

- **Acknowledgment**: We will acknowledge your report within 48 hours
- **Investigation**: We will investigate and assess the severity of the issue
- **Fix**: We will work on a fix and test it thoroughly
- **Disclosure**: We will coordinate disclosure timing with you
- **Credit**: You will be credited for the discovery (unless you prefer anonymity)

## Security Best Practices

When using this Emacs configuration, follow these security best practices:

### For Users

1. **Keep Updated**: Regularly update the configuration and packages
   ```bash
   cd ~/.emacs.d
   git pull origin main
   # In Emacs: M-x straight-pull-all
   ```

2. **Review Before Running**: Always review code before adding it to your configuration

3. **Secure Your API Keys**: Never commit API keys or credentials
   - Use environment variables
   - Use Emacs' auth-sources (e.g., `~/.authinfo.gpg`)
   - Use a password manager

4. **Trust Your Workspace**: Be cautious when opening projects from untrusted sources

5. **Use Secure Connections**: Ensure package sources use HTTPS

### For Contributors

1. **No Secrets**: Never commit API keys, passwords, or credentials

2. **Secure Package Sources**: Only add packages from trusted, well-maintained sources

3. **Code Review**: Review third-party code before integrating

4. **Test Security**: Test configuration changes in a safe environment first

5. **Document Security**: Document any security implications of new features

## Known Security Considerations

### GPTel AI Integration

- Requires API key for OpenAI or other LLM providers
- API key must be stored securely (not in repository)
- LLM requests send code/text to external services
- Users should be aware of data privacy implications

### LSP Servers

- Execute external binaries (pyright, clangd, bash-language-server)
- Communicate over local network sockets
- Process workspace files
- Should only be installed from official sources

### External Dependencies

This configuration relies on external tools:
- **Emacs** itself (keep updated)
- **LSP servers** (pyright, clangd, bash-language-server)
- **Git** (for straight.el)
- **Various system tools** (grep, find, etc.)

Keep these dependencies updated for security patches.

## Security Updates

When security issues are discovered and fixed:

1. We will document them in the CHANGELOG.md
2. We will update this SECURITY.md if policies change
3. Critical issues will be highlighted in commit messages
4. Users will be notified through GitHub notifications

## Vulnerability Disclosure Policy

- **Coordinated Disclosure**: We practice coordinated vulnerability disclosure
- **Fix First**: We will fix the vulnerability before public disclosure
- **Responsible Timeline**: We aim to fix issues within 90 days of responsible disclosure
- **Public Acknowledgment**: We will publicly acknowledge security researchers who report issues responsibly

## Additional Resources

- [GNU Emacs Security](https://www.gnu.org/software/emacs/manual/html_node/emacs/Security.html)
- [Straight.el Security](https://github.com/radian-software/straight.el#security)
- [OWASP Secure Coding Practices](https://owasp.org/www-project-secure-coding-practices-quick-reference-guide/)

## Questions?

If you have questions about security that don't involve reporting a vulnerability:

- Open a GitHub issue with the `security` label
- Tag it as a `question`
- We'll respond publicly to help others with similar questions

---

**Remember**: Security is a shared responsibility. Stay vigilant, keep updated, and report issues responsibly. Thank you for helping keep this project secure! 🔒
