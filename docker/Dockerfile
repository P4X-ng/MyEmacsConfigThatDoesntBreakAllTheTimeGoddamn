# Self-contained Jedi Language Server Container
# This Dockerfile creates a fully self-contained Python environment with Jedi and LSP tools.
# It does NOT rely on any packages from the host system - everything is installed fresh
# into a dedicated virtual environment within the container.

FROM python:3.11-slim

# Install system dependencies needed for building Python packages
# These are standard build tools and do not come from the host
RUN apt-get update && apt-get install -y \
    git \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create jedi user and workspace (not required but follows best practices)
RUN useradd -m -s /bin/bash jedi
WORKDIR /home/jedi

# Create jedi deployment structure
# Note: We create bin/ and config/ directories, but NOT lib/ since we use a venv
# All Python packages will be installed into the venv, not copied from anywhere
RUN mkdir -p /opt/jedi/bin /opt/jedi/config

# Create a dedicated virtual environment for all Jedi dependencies
# This ensures complete isolation from the system Python and any host environment
RUN python -m venv /opt/jedi/venv

# Install Python packages into the container's virtual environment
# These are downloaded from PyPI and installed fresh - NOT copied from the host
# Using specific versions for reproducibility and reliability
RUN /opt/jedi/venv/bin/pip install --no-cache-dir --upgrade pip setuptools wheel && \
    /opt/jedi/venv/bin/pip install --no-cache-dir \
    jedi==0.19.1 \
    jedi-language-server==0.41.2 \
    python-lsp-server[all]==1.8.2 \
    pylsp-mypy==0.6.7 \
    python-lsp-black==1.3.0 \
    pylsp-rope==0.1.11 \
    rope==1.11.0 \
    autopep8==2.0.4 \
    flake8==6.1.0 \
    mypy==1.7.1 \
    black==23.11.0


# Copy wrapper scripts that will use the venv Python interpreter
COPY jedi-wrapper.py /opt/jedi/bin/
COPY pylsp-wrapper.py /opt/jedi/bin/
RUN chmod +x /opt/jedi/bin/*.py

# Copy configuration files for Jedi and PyLSP
COPY jedi-config.json /opt/jedi/config/
COPY pylsp-config.json /opt/jedi/config/

# Copy health check script
COPY health-check.py /opt/jedi/bin/
RUN chmod +x /opt/jedi/bin/health-check.py

# Verify the installation by importing the packages from our venv
# This ensures everything was installed correctly in the container
RUN /opt/jedi/venv/bin/python -c "import jedi; print('✓ Jedi version:', jedi.__version__)" && \
    /opt/jedi/venv/bin/python -c "from jedi_language_server import server; print('✓ Jedi Language Server OK')" && \
    /opt/jedi/venv/bin/python -c "import pylsp; print('✓ Python LSP Server OK')"

# Create deployment archive containing the complete jedi installation
# This archive can be extracted to deploy jedi to a host virtual environment
RUN cd /opt && tar czf jedi-deployment.tar.gz jedi/

# Switch to non-root user for security
USER jedi

CMD ["/bin/bash"]