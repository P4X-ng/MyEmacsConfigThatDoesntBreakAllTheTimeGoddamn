# Self-Contained Jedi Language Server Docker Container

This directory contains a **fully self-contained** Dockerfile for building a reliable Jedi language server environment for Python autocompletion in Emacs.

## Key Features

### 🔒 Complete Isolation
- **Everything is self-contained**: All Python packages are installed fresh from PyPI into a dedicated virtual environment inside the container
- **No host dependencies**: Does NOT rely on any Python packages from your host system's `/usr/local/lib/` or anywhere else
- **Reproducible builds**: Uses pinned package versions to ensure consistent behavior

### 🏗️ Architecture

The container creates:
- `/opt/jedi/venv/` - A Python virtual environment with all Jedi dependencies
- `/opt/jedi/bin/` - Wrapper scripts for easy execution
- `/opt/jedi/config/` - Configuration files for Jedi and PyLSP
- `/opt/jedi-deployment.tar.gz` - A complete archive that can be deployed to host systems

## Building the Container

### Quick Build
```bash
# From the project root:
./scripts/build-jedi.sh

# Or directly with Docker:
cd docker/
docker build -t reliable-jedi:latest .
```

### Fresh Build (Clear Cache)
If you're experiencing issues or want to ensure a completely fresh build:

```bash
# Using the build script:
./scripts/build-jedi.sh --no-cache

# Or directly with Docker:
cd docker/
docker build --no-cache -t reliable-jedi:latest .
```

**Important**: Always use `--no-cache` if you've experienced build failures before. Docker's layer caching can sometimes preserve broken intermediate states.

## Troubleshooting

### "No such file or directory" Build Error 
If you see errors like:
```
cp: target '/opt/jedi/lib/': No such file or directory
```

This indicates you have a corrupted Docker cache from a previous build attempt. **Solution**:

1. Clear the Docker build cache:
   ```bash
   docker builder prune -af
   ```

2. Rebuild without cache:
   ```bash
   ./scripts/build-jedi.sh --no-cache
   ```

### SSL Certificate Errors
If you see SSL/certificate verification errors when pip tries to download packages:

- This is usually an environment issue (corporate proxy, firewall, etc.)
- Try building from a different network
- Or configure pip to trust your certificates (not recommended for security)

### Package Version Conflicts
The Dockerfile uses specific package versions for reliability. If you need different versions:

1. Edit the `RUN /opt/jedi/venv/bin/pip install` section in the Dockerfile
2. Update version numbers as needed
3. Rebuild with `--no-cache` to ensure clean installation

## How It Works

1. **Base Image**: Starts with official `python:3.11-slim` image
2. **System Dependencies**: Installs build tools (gcc, git, etc.) needed for compiling Python packages
3. **Virtual Environment**: Creates an isolated Python venv at `/opt/jedi/venv/`
4. **Package Installation**: Downloads and installs all packages fresh from PyPI into the venv
5. **Verification**: Tests that all packages imported correctly
6. **Deployment Archive**: Creates a tar.gz that can be extracted on host systems

## What Gets Installed

The container includes:
- `jedi` - Python autocompletion library
- `jedi-language-server` - LSP server for Jedi
- `python-lsp-server` - Alternative Python LSP implementation
- `pylsp-mypy` - Mypy integration for type checking
- `python-lsp-black` - Black formatter integration
- `pylsp-rope` - Rope refactoring integration
- `rope` - Python refactoring library
- `autopep8` - PEP 8 auto-formatter
- `flake8` - Python linter
- `mypy` - Static type checker
- `black` - Code formatter

All packages are installed at specific pinned versions for reproducibility.

## Deployment

Once built, use the deployment script to extract the Jedi environment to your host:

```bash
# Deploy to current venv:
./scripts/deploy-jedi.sh

# Deploy to specific venv:
./scripts/deploy-jedi.sh /path/to/your/venv
```

This extracts the complete `/opt/jedi/` directory from the container to your host system.

## Differences from jedi-container/

This directory (`docker/`) provides a more comprehensive solution compared to `jedi-container/`:

- **docker/**: Full deployment system with wrapper scripts, health checks, and configuration
- **jedi-container/**: Simpler, minimal Jedi installation

Both are valid approaches. Use `docker/` if you want the complete system, or `jedi-container/` if you prefer simplicity.

## Why This Approach?

Traditional Jedi installation can fail due to:
- Python version mismatches
- Missing system dependencies
- Conflicting package versions
- Virtual environment issues

By containerizing the build, we:
- ✅ Guarantee a consistent environment
- ✅ Eliminate host system dependencies
- ✅ Make installation reproducible
- ✅ Avoid "works on my machine" problems

## Requirements

- Docker (or Podman)
- That's it! No Python, pip, or other dependencies needed on the host

## Further Help

- See `../scripts/build-jedi.sh` for build options
- See `../scripts/deploy-jedi.sh` for deployment options
- See `../scripts/health-check.sh` for testing the installation
