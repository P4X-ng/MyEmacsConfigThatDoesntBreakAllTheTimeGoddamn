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