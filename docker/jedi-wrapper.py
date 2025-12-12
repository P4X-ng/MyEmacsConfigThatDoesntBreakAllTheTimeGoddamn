#!/opt/jedi/venv/bin/python3
"""
Jedi Language Server Wrapper
Ensures reliable Jedi operation in any environment
"""
import sys
import os
import json
from pathlib import Path

# Set up environment
os.environ["JEDI_CACHE_DIRECTORY"] = str(Path.home() / ".cache" / "jedi")
os.environ["JEDI_SETTINGS"] = str(Path(__file__).parent.parent / "config" / "jedi-config.json")

# Ensure cache directory exists
Path(os.environ["JEDI_CACHE_DIRECTORY"]).mkdir(parents=True, exist_ok=True)

try:
    from jedi_language_server import server
    
    # Load configuration
    config_file = Path(__file__).parent.parent / "config" / "jedi-config.json"
    if config_file.exists():
        with open(config_file) as f:
            config = json.load(f)
            for key, value in config.items():
                os.environ[f"JEDI_{key.upper()}"] = str(value)
    
    # Start the server
    if __name__ == "__main__":
        server.main()
        
except ImportError as e:
    print(f"Error importing Jedi Language Server: {e}", file=sys.stderr)
    sys.exit(1)
except Exception as e:
    print(f"Error starting Jedi Language Server: {e}", file=sys.stderr)
    sys.exit(1)