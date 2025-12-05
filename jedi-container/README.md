# Containerized Jedi Language Server

This directory contains a Docker-based setup for **jedi-language-server**, providing reliable Python autocompletion for Emacs without the usual installation headaches.

## Why Containerized?

Jedi (and jedi-language-server) can be tricky to install correctly due to:
- Python version conflicts
- Missing dependencies
- pip installation issues
- Virtualenv path problems

By containerizing the installation, we get a **100% reproducible** jedi environment that:
- Works on any system with Docker/Podman
- Doesn't interfere with your system Python
- Can be easily updated or reset
- Gets dropped directly into your venv path for seamless Emacs integration

## Quick Start

```bash
# From this directory:
./setup-jedi.sh

# Or with a custom venv location:
VENV_HOME=/path/to/venvs ./setup-jedi.sh
```

This will:
1. Build a Docker container with jedi-language-server
2. Copy the complete jedi virtualenv to `~/.venv/jedi/`
3. Make `~/.venv/jedi/bin/jedi-language-server` available for Emacs

## Usage with Emacs

The `init.el` is configured to automatically use the containerized jedi installation when available. The jedi-language-server will be found at:

```
~/.venv/jedi/bin/jedi-language-server
```

### Manual Activation

If you want to manually verify or use jedi:

```bash
# Check version
~/.venv/jedi/bin/jedi-language-server --version

# Run interactively (for testing)
~/.venv/jedi/bin/jedi-language-server
```

## Files

- `Dockerfile` - Container definition with jedi-language-server
- `docker-compose.yml` - Compose file for building and installing
- `setup-jedi.sh` - One-command install script

## Requirements

- Docker or Podman
- (Optional) docker-compose or podman-compose

## Updating Jedi

To update to the latest jedi-language-server:

```bash
# Rebuild without cache
docker build --no-cache -t jedi-language-server:latest .
./setup-jedi.sh
```

## Troubleshooting

### "jedi-language-server not found"

Make sure the setup completed successfully:
```bash
ls -la ~/.venv/jedi/bin/jedi-language-server
```

### Permission issues

Ensure the script is executable:
```bash
chmod +x setup-jedi.sh
```

### Docker not found

Install Docker:
- macOS: `brew install --cask docker`
- Ubuntu: `apt install docker.io`
- Or visit: https://docs.docker.com/get-docker/
