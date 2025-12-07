/*
 * Demo C file to test autocompletion
 *
 * With the enhanced Emacs configuration, you should see:
 * - Completions appear automatically as you type
 * - Function signatures and parameter hints
 * - Documentation for standard library functions
 * - Struct member completions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Custom struct to demo member completion
typedef struct {
    int id;
    char name[50];
    double value;
} DataRecord;

// Function with documentation
/**
 * Calculate the factorial of a number
 * @param n The number to calculate factorial for
 * @return The factorial of n
 */
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    // Try typing 'prin' - should complete to printf
    printf("Demo C autocompletion\n");
    
    // Try typing 'str' - should show string.h functions
    char buffer[100];
    strcpy(buffer, "Hello, World!");
    
    // Try typing 'fact' - should complete to factorial
    int result = factorial(5);
    printf("Factorial of 5: %d\n", result);
    
    // Try typing 'record.' - should show struct members
    DataRecord record;
    record.id = 1;
    strcpy(record.name, "Test");
    record.value = 3.14;
    
    printf("Record: id=%d, name=%s, value=%.2f\n", 
           record.id, record.name, record.value);
    
    return 0;
}
