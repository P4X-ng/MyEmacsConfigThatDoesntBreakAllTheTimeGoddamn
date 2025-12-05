# Containerized jedi-language-server for reliable Python autocompletion
# This container builds a self-contained jedi venv that can be exported
# to the host system for use with Emacs LSP.

FROM python:3.11-slim

LABEL maintainer="Emacs Config Project"
LABEL description="Jedi Language Server for Python autocompletion"

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# Create the venv directory
RUN python -m venv /jedi-venv

# Upgrade pip and install jedi-language-server with all dependencies
RUN /jedi-venv/bin/pip install --no-cache-dir --upgrade pip && \
    /jedi-venv/bin/pip install --no-cache-dir \
    jedi-language-server \
    jedi \
    parso

# Make jedi-language-server executable accessible
RUN chmod +x /jedi-venv/bin/jedi-language-server

# Create a wrapper script for running jedi-language-server
RUN printf '#!/bin/bash\nexec /jedi-venv/bin/jedi-language-server "$@"\n' > /usr/local/bin/jedi-lsp && \
    chmod +x /usr/local/bin/jedi-lsp

# Default command runs the language server
CMD ["/jedi-venv/bin/jedi-language-server"]

# Expose stdio for LSP communication (the server uses stdin/stdout)
# No ports needed as it communicates via stdio
