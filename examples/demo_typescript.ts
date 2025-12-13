// TypeScript Autocompletion Demo
// This file demonstrates the autocompletion features in TypeScript mode

// ===================================
// 1. Basic Type Completions
// ===================================

// Type 'let myStr: str' and completion should suggest 'string'
let myString: string = "Hello, TypeScript!";

// Type 'let myNum: num' and completion should suggest 'number'
let myNumber: number = 42;

// ===================================
// 2. Interface and Type Completions
// ===================================

interface User {
  id: number;
  name: string;
  email: string;
  isActive: boolean;
}

// Type 'const user: Us' and completion should suggest 'User'
const user: User = {
  id: 1,
  name: "John Doe",
  email: "john@example.com",
  isActive: true
};

// Type 'user.' and completion should show: id, name, email, isActive
// Try: user.

// ===================================
// 3. Function Completions
// ===================================

function greetUser(user: User): string {
  return `Hello, ${user.name}!`;
}

// Type 'greetU' and completion should suggest 'greetUser'
// When you type '(', you should see parameter hints

// ===================================
// 4. Class Completions
// ===================================

class Calculator {
  add(a: number, b: number): number {
    return a + b;
  }

  subtract(a: number, b: number): number {
    return a - b;
  }

  multiply(a: number, b: number): number {
    return a * b;
  }

  divide(a: number, b: number): number {
    if (b === 0) {
      throw new Error("Cannot divide by zero");
    }
    return a / b;
  }
}

const calc = new Calculator();
// Type 'calc.' and completion should show: add, subtract, multiply, divide
// Try: calc.

// ===================================
// 5. Array Method Completions
// ===================================

const numbers: number[] = [1, 2, 3, 4, 5];

// Type 'numbers.' and completion should show array methods like:
// map, filter, reduce, forEach, find, etc.
// Try: numbers.

// ===================================
// 6. Generic Type Completions
// ===================================

function identity<T>(arg: T): T {
  return arg;
}

// Type 'identity' and you should get completion with generic type hints
const result = identity<string>("Hello");

// ===================================
// 7. Import Completions
// ===================================

// Type 'import { } from "' and you should get module suggestions
// (This works better in projects with node_modules)

// ===================================
// 8. Built-in Object Completions
// ===================================

// Type 'Math.' and completion should show Math methods
// Try: Math.

// Type 'console.' and completion should show console methods
// Try: console.

// ===================================
// 9. Promise and Async/Await
// ===================================

async function fetchData(url: string): Promise<string> {
  const response = await fetch(url);
  const data = await response.text();
  return data;
}

// Type 'fetchD' and completion should suggest 'fetchData'

// ===================================
// Tips for Testing:
// ===================================
// 1. Completions appear automatically after typing 2 characters
// 2. Press TAB to accept or cycle through completions
// 3. Press S-TAB to cycle backward
// 4. Press ESC to cancel the completion popup
// 5. Hover over functions to see documentation
// 6. Use C-c l g g to go to definition
// 7. Use C-c l g r to find references
