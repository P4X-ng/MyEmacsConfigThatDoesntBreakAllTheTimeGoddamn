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
RUN mkdir -p /opt/jedi/{bin,lib,config}

# Create virtual environment for jedi
RUN python -m venv /opt/jedi/venv

# Install Python packages in the virtual environment with specific versions for reliability
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

# Create wrapper scripts
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
RUN /opt/jedi/venv/bin/python -c "import jedi; print('Jedi version:', jedi.__version__)" && \
    /opt/jedi/venv/bin/python -c "from jedi_language_server import server; print('Jedi Language Server OK')" && \
    /opt/jedi/venv/bin/python -c "import pylsp; print('Python LSP Server OK')"

# Create deployment archive
RUN cd /opt && tar czf jedi-deployment.tar.gz jedi/

# Switch to jedi user
USER jedi

CMD ["/bin/bash"]