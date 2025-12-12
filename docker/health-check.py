#!/opt/jedi/venv/bin/python3
"""
Jedi Health Check Script
Validates that Jedi and related tools are working correctly
"""
import sys
import os
import json
import subprocess
from pathlib import Path

def check_import(module_name, description):
    """Check if a module can be imported"""
    try:
        __import__(module_name)
        print(f"✅ {description}: OK")
        return True
    except ImportError as e:
        print(f"❌ {description}: FAILED - {e}")
        return False

def check_executable(exe_path, description):
    """Check if an executable exists and is runnable"""
    try:
        if Path(exe_path).exists():
            result = subprocess.run([exe_path, "--help"], 
                                  capture_output=True, 
                                  timeout=5)
            if result.returncode == 0:
                print(f"✅ {description}: OK")
                return True
            else:
                print(f"❌ {description}: FAILED - Exit code {result.returncode}")
                return False
        else:
            print(f"❌ {description}: FAILED - Executable not found")
            return False
    except Exception as e:
        print(f"❌ {description}: FAILED - {e}")
        return False

def check_jedi_functionality():
    """Test basic Jedi functionality"""
    try:
        import jedi
        
        # Test basic completion
        script = jedi.Script(code="import os\nos.", line=2, column=3)
        completions = script.completions()
        
        if len(completions) > 0:
            print(f"✅ Jedi Completion: OK ({len(completions)} completions found)")
            return True
        else:
            print("❌ Jedi Completion: FAILED - No completions found")
            return False
    except Exception as e:
        print(f"❌ Jedi Completion: FAILED - {e}")
        return False

def main():
    print("🔍 Jedi Health Check")
    print("=" * 50)
    
    # Get the jedi installation path
    jedi_path = Path(__file__).parent.parent
    venv_path = jedi_path / "venv"
    bin_path = jedi_path / "bin"
    config_path = jedi_path / "config"
    
    print(f"📁 Jedi Path: {jedi_path}")
    print(f"📁 Virtual Env Path: {venv_path}")
    print(f"📁 Bin Path: {bin_path}")
    print(f"📁 Config Path: {config_path}")
    print()
    
    checks_passed = 0
    total_checks = 0
    
    # Check core imports
    print("🐍 Python Module Checks:")
    modules = [
        ("jedi", "Jedi Core"),
        ("jedi_language_server", "Jedi Language Server"),
        ("pylsp", "Python LSP Server"),
        ("rope", "Rope Refactoring"),
        ("black", "Black Formatter"),
        ("mypy", "MyPy Type Checker")
    ]
    
    for module, desc in modules:
        if check_import(module, desc):
            checks_passed += 1
        total_checks += 1
    
    print()
    
    # Check executables
    print("🔧 Executable Checks:")
    executables = [
        (bin_path / "jedi-wrapper.py", "Jedi Wrapper"),
        (bin_path / "pylsp-wrapper.py", "PyLSP Wrapper")
    ]
    
    for exe, desc in executables:
        if check_executable(str(exe), desc):
            checks_passed += 1
        total_checks += 1
    
    print()
    
    # Check configuration files
    print("⚙️  Configuration Checks:")
    configs = [
        (config_path / "jedi-config.json", "Jedi Config"),
        (config_path / "pylsp-config.json", "PyLSP Config")
    ]
    
    for config_file, desc in configs:
        try:
            if config_file.exists():
                with open(config_file) as f:
                    json.load(f)  # Validate JSON
                print(f"✅ {desc}: OK")
                checks_passed += 1
            else:
                print(f"❌ {desc}: FAILED - File not found")
        except json.JSONDecodeError as e:
            print(f"❌ {desc}: FAILED - Invalid JSON: {e}")
        except Exception as e:
            print(f"❌ {desc}: FAILED - {e}")
        total_checks += 1
    
    print()
    
    # Test Jedi functionality
    print("🧠 Functionality Checks:")
    if check_jedi_functionality():
        checks_passed += 1
    total_checks += 1
    
    print()
    print("=" * 50)
    print(f"📊 Results: {checks_passed}/{total_checks} checks passed")
    
    if checks_passed == total_checks:
        print("🎉 All checks passed! Jedi is ready to use.")
        return 0
    else:
        print("⚠️  Some checks failed. Please review the errors above.")
        return 1

if __name__ == "__main__":
    sys.exit(main())