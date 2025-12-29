FROM python:3.11-slim

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create jedi user and workspace
RUN useradd -m -s /bin/bash jedi
WORKDIR /home/jedi

# Create jedi deployment structure  
# Note: We create the venv which will contain everything we need
RUN mkdir -p /opt/jedi/{bin,config}

# Create virtual environment for jedi
RUN python -m venv /opt/jedi/venv

# Install Python packages in the virtual environment with updated versions for security
# Updated from 2023 versions to more recent stable releases
RUN /opt/jedi/venv/bin/pip install --no-cache-dir --upgrade pip setuptools wheel && \
    /opt/jedi/venv/bin/pip install --no-cache-dir \
    jedi==0.19.2 \
    jedi-language-server==0.41.4 \
    python-lsp-server[all]==1.12.0 \
    pylsp-mypy==0.6.9 \
    python-lsp-black==2.0.0 \
    pylsp-rope==0.1.16 \
    rope==1.13.0 \
    autopep8==2.3.1 \
    flake8==7.1.1 \
    mypy==1.13.0 \
    black==24.10.0

# Create wrapper scripts that use the venv
COPY jedi-wrapper.py /opt/jedi/bin/
COPY pylsp-wrapper.py /opt/jedi/bin/
RUN chmod +x /opt/jedi/bin/*.py

# Create configuration files
COPY jedi-config.json /opt/jedi/config/
COPY pylsp-config.json /opt/jedi/config/

# Create health check script
COPY health-check.py /opt/jedi/bin/
RUN chmod +x /opt/jedi/bin/health-check.py

# Test the installation using the virtual environment
# All packages are in the venv, so we test from there
RUN /opt/jedi/venv/bin/python -c "import jedi; print('✅ Jedi version:', jedi.__version__)" && \
    /opt/jedi/venv/bin/python -c "from jedi_language_server import server; print('✅ Jedi Language Server OK')" && \
    /opt/jedi/venv/bin/python -c "import pylsp; print('✅ Python LSP Server OK')"

# Create deployment archive
# The venv contains everything needed - all site-packages, executables, etc.
# No need to copy individual files, the venv is self-contained
RUN cd /opt && tar czf jedi-deployment.tar.gz jedi/

# Switch to jedi user
USER jedi

CMD ["/bin/bash"]