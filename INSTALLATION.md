# Installation Guide

This guide walks you through installing this Emacs configuration on Ubuntu 24.04.

## 🚀 Quick Start (Automated)

The easiest way to get started:

```bash
# Clone the repository
git clone https://github.com/P4X-ng/MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn.git
cd MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn

# Run the automated setup script
./setup-ubuntu.sh
```

That's it! The script will:
- ✅ Check your Ubuntu version
- ✅ Install Emacs and all dependencies
- ✅ Install language servers (C/C++, Bash, TypeScript, Python)
- ✅ Install code formatters
- ✅ Deploy configuration to `~/.emacs.d` using rsync
- ✅ Optionally set up OpenAI API integration
- ✅ Provide clear feedback at each step

## 📋 What Gets Installed

### Core Components
- **Emacs** (latest version from apt)
- **Node.js and npm** (for language servers)
- **Git** (for version control and package management)

### Language Servers
- **clangd** - C/C++ language server
- **bash-language-server** - Bash/shell script language server
- **typescript-language-server** - TypeScript/JavaScript language server
- **pyright** - Python language server

### Code Formatters
- **black** - Python code formatter
- **prettier** - TypeScript/JavaScript formatter
- **shfmt** - Bash/shell script formatter
- **clang-format** - C/C++ formatter

### Configuration
- **Emacs configuration** deployed to `~/.emacs.d` via rsync
- **Environment file** (`.env`) for OpenAI API integration

## 🔧 Setup Script Details

### What the Script Does

1. **Version Check**: Verifies you're running Ubuntu (optimized for 24.04)
2. **Package Updates**: Updates apt package lists
3. **Emacs Installation**: Installs Emacs or upgrades if already present
4. **Node.js Setup**: Installs Node.js and npm for language servers
5. **Language Server Installation**: Installs all language servers
6. **Formatter Installation**: Installs code formatters
7. **Configuration Deployment**: Uses rsync to intelligently copy configuration files
8. **OpenAI Setup**: Optionally configures OpenAI API integration

### Why rsync?

The script uses `rsync` instead of a simple `cp` command because:
- **Smart copying**: Only copies changed files
- **Exclusions**: Automatically excludes cache directories, build artifacts, and .git
- **Preservation**: Maintains file permissions and timestamps
- **Incremental**: Can be run multiple times safely

### Excluded Directories

These directories are excluded from deployment (they'll be auto-generated):
- `.git*` - Git metadata
- `straight/` - Package manager cache
- `.cache/` - Emacs cache
- `auto-save-list/` - Auto-save files
- `eln-cache/` - Native compilation cache

## 🔐 OpenAI API Setup

The setup script will ask if you want to configure OpenAI integration:

### During Installation

1. When prompted, choose "Yes" to configure OpenAI
2. Enter your OpenAI API key (get one from https://platform.openai.com/api-keys)
3. The script will:
   - Create `~/.emacs.d/.env` with your configuration
   - Add environment loading to `~/.bashrc`
   - Source the environment for the current session

### Manual Setup

If you skip during installation or want to reconfigure:

```bash
# Copy the example file
cp .env.example ~/.emacs.d/.env

# Edit and add your API key
nano ~/.emacs.d/.env

# Add to your shell profile
echo 'if [ -f "$HOME/.emacs.d/.env" ]; then' >> ~/.bashrc
echo '    export $(grep -v "^#" "$HOME/.emacs.d/.env" | xargs)' >> ~/.bashrc
echo 'fi' >> ~/.bashrc

# Source for current session
source ~/.bashrc
```

### Using the AI Features

Once configured, you can use:
- `C-c C-g` - Open GPTel chat window
- `C-c g q` - Ask a quick question (answer inserted at cursor)
- `C-c g e` - Explain selected code
- `C-c g s` - Send region/buffer to ChatGPT

## 🧪 Testing the Installation

After installation:

### 1. Start Emacs

```bash
emacs
```

**First Launch Note**: The first time you start Emacs, it will:
- Download and install all packages via straight.el
- Compile packages for your system
- This takes 2-5 minutes - be patient!

### 2. Test Basic Features

- Press `F8` to toggle Treemacs file explorer
- Press `C-k` to view the keybinding cheat sheet
- Open a file: `C-x C-f`

### 3. Test Language Servers

Open a test file to verify LSP is working:

```bash
# In Emacs, open a file
C-x C-f ~/test.c

# Type some C code - you should see:
# - Auto-completion as you type
# - Syntax highlighting
# - Error checking
```

### 4. Test AI Features (if configured)

- Press `C-c C-g` to open GPTel
- Type a question and send it
- Or press `C-c g q` to ask an inline question

## 🔄 Updating the Configuration

To update your configuration:

```bash
# Navigate to the repo directory
cd MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn

# Pull latest changes
git pull

# Re-run deployment (this won't overwrite .env)
rsync -av --exclude='.git*' --exclude='straight/' --exclude='.cache/' \
      dot.emacs.d/ ~/.emacs.d/
```

Or re-run the setup script:

```bash
./setup-ubuntu.sh
```

## 📁 File Locations

After installation:

- **Emacs config**: `~/.emacs.d/`
- **Main config file**: `~/.emacs.d/init.el`
- **Environment file**: `~/.emacs.d/.env`
- **Package cache**: `~/.emacs.d/straight/`
- **Jedi cache**: `~/.cache/jedi/`
- **Old config backup**: `~/.emacs.d.backup.YYYYMMDD_HHMMSS/`

## 🐛 Troubleshooting

### Script Fails to Install Dependencies

If the script fails during package installation:

```bash
# Update package lists manually
sudo apt update

# Try installing failed package individually
# Example for clangd:
sudo apt install -y clangd

# Re-run the script
./setup-ubuntu.sh
```

### Language Server Not Working

Check if the language server is installed:

```bash
# Check clangd
which clangd

# Check bash-language-server
which bash-language-server

# Check typescript-language-server
which typescript-language-server

# Check pyright
which pyright
```

If missing, install manually:

```bash
# For npm-based servers
sudo npm install -g bash-language-server typescript-language-server pyright

# For clangd
sudo apt install -y clangd
```

### Emacs Won't Start

If Emacs fails to start after installation:

```bash
# Try with minimal config
emacs -q

# Try with debug mode
emacs --debug-init

# Check for syntax errors in init.el
emacs --batch --eval "(load-file \"~/.emacs.d/init.el\")"
```

### OpenAI Not Working

If AI features don't work:

1. Check API key is set:
   ```bash
   echo $OPENAI_API_KEY
   ```

2. Check .env file:
   ```bash
   cat ~/.emacs.d/.env
   ```

3. Manually export the key:
   ```bash
   export OPENAI_API_KEY=your_key_here
   ```

4. Restart Emacs

### Packages Not Installing

If straight.el fails to install packages:

```bash
# Delete package cache
rm -rf ~/.emacs.d/straight/

# Restart Emacs - packages will reinstall
emacs
```

## 📝 Manual Installation

If you prefer to install manually instead of using the script:

### Step 1: Install Emacs
```bash
sudo apt update
sudo apt install -y emacs
```

### Step 2: Install Dependencies
```bash
# Node.js and npm
sudo apt install -y nodejs npm

# Git
sudo apt install -y git

# Python and Go (for formatters)
sudo apt install -y python3 python3-pip golang-go
```

### Step 3: Install Language Servers
```bash
sudo apt install -y clangd
sudo npm install -g bash-language-server
sudo npm install -g typescript-language-server typescript
sudo npm install -g pyright
```

### Step 4: Install Formatters
```bash
python3 -m pip install --user black
sudo npm install -g prettier
go install mvdan.cc/sh/v3/cmd/shfmt@latest
```

### Step 5: Deploy Configuration
```bash
# Clone repository
git clone https://github.com/P4X-ng/MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn.git
cd MyEmacsConfigThatDoesntBreakAllTheTimeGoddamn

# Backup existing config
mv ~/.emacs.d ~/.emacs.d.backup

# Deploy with rsync
rsync -av --exclude='.git*' --exclude='straight/' --exclude='.cache/' \
      dot.emacs.d/ ~/.emacs.d/
```

### Step 6: Configure OpenAI (Optional)
```bash
cp .env.example ~/.emacs.d/.env
nano ~/.emacs.d/.env  # Add your API key
```

## 🎉 Next Steps

After successful installation:

1. **Start Emacs**: `emacs`
2. **Read the cheat sheet**: Press `C-k` in Emacs
3. **Explore features**: Open some code files and try:
   - Auto-completion
   - Code navigation (`C-c l g g` to go to definition)
   - AI assistance (`C-c C-g` for chat)
4. **Check the README**: More detailed feature documentation
5. **Try the examples**: `emacs examples/demo_c.c`

## 📚 Additional Resources

- [README.md](README.md) - Main documentation
- [AUTOCOMPLETE_SETUP.md](AUTOCOMPLETE_SETUP.md) - Detailed autocomplete guide
- [FIXES.md](FIXES.md) - Information about bug fixes
- [dot.emacs.d/README.cheatsheet.md](dot.emacs.d/README.cheatsheet.md) - Keybinding reference

## 💡 Tips

- **First launch is slow**: Package installation takes time - this is normal
- **Backup your old config**: The script does this automatically
- **Use rsync for updates**: Safer than overwriting everything
- **Environment variables**: Source `.env` in your shell profile
- **LSP indicators**: Look for "LSP" in the mode line when editing files
- **Package issues**: Delete `~/.emacs.d/straight/` and restart Emacs

## 🆘 Getting Help

If you encounter issues:

1. Check this installation guide's troubleshooting section
2. Review [FIXES.md](FIXES.md) for known issues
3. Check the [README.md](README.md) for general usage
4. Open an issue on GitHub with:
   - Your Ubuntu version
   - Error messages
   - Steps to reproduce
