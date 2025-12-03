#!/usr/bin/env python3
"""
Python LSP Server Wrapper with Jedi Integration
Ensures reliable Python language server operation
"""
import sys
import os
import json
from pathlib import Path

# Add jedi libs to Python path
jedi_lib_path = Path(__file__).parent.parent / "lib"
sys.path.insert(0, str(jedi_lib_path))

# Set up environment
os.environ["PYLSP_CACHE_DIRECTORY"] = str(Path.home() / ".cache" / "pylsp")
os.environ["PYLSP_SETTINGS"] = str(Path(__file__).parent.parent / "config" / "pylsp-config.json")

# Ensure cache directory exists
Path(os.environ["PYLSP_CACHE_DIRECTORY"]).mkdir(parents=True, exist_ok=True)

try:
    from pylsp.__main__ import main
    
    # Load configuration
    config_file = Path(__file__).parent.parent / "config" / "pylsp-config.json"
    if config_file.exists():
        with open(config_file) as f:
            config = json.load(f)
            # Set pylsp configuration via environment
            os.environ["PYLSP_CONFIG"] = json.dumps(config)
    
    # Start the server
    if __name__ == "__main__":
        main()
        
except ImportError as e:
    print(f"Error importing Python LSP Server: {e}", file=sys.stderr)
    sys.exit(1)
except Exception as e:
    print(f"Error starting Python LSP Server: {e}", file=sys.stderr)
    sys.exit(1)