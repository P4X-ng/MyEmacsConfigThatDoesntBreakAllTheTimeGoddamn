"""
Demo Python file to test autocompletion

With the enhanced Emacs configuration, you should see:
- Completions appear automatically as you type
- Documentation popups next to completions
- Type hints and function signatures
- Import suggestions
"""

import os
import sys


def calculate_sum(a: int, b: int) -> int:
    """
    Calculate the sum of two integers.
    
    Args:
        a: First integer
        b: Second integer
        
    Returns:
        The sum of a and b
    """
    return a + b


def main():
    # Try typing 'os.' - you should see methods like getcwd, listdir, etc.
    current_dir = os.getcwd()
    
    # Try typing 'sys.' - you should see methods like exit, argv, etc.
    args = sys.argv
    
    # Try typing 'calculate_' - should complete to calculate_sum
    result = calculate_sum(10, 20)
    print(f"Result: {result}")
    
    # Try typing 'pri' - should suggest print
    print("Demo complete!")


if __name__ == "__main__":
    main()
