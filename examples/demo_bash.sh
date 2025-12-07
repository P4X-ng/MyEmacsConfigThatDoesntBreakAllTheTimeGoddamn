#!/bin/bash

# Demo Bash script to test autocompletion
#
# With the enhanced Emacs configuration, you should see:
# - Completions for bash built-ins and commands
# - Variable name completions
# - Function name completions

# Function with documentation
# Calculates the sum of two numbers
# Arguments:
#   $1 - First number
#   $2 - Second number
calculate_sum() {
    local num1=$1
    local num2=$2
    echo $((num1 + num2))
}

# Main script
main() {
    echo "Demo Bash autocompletion"
    
    # Try typing 'ech' - should complete to echo
    echo "Testing completions..."
    
    # Try typing variable names - should complete
    local test_variable="Hello"
    echo "$test_variable"
    
    # Try typing function names - should complete to calculate_sum
    result=$(calculate_sum 10 20)
    echo "Sum: $result"
    
    # Try typing common commands
    # pwd, ls, cd, etc. should autocomplete
    current_dir=$(pwd)
    echo "Current directory: $current_dir"
}

# Run main
main "$@"
