# Noopy Language - Developer Guide

Welcome to the **Noopy** programming language documentation. This guide provides an overview of the syntax, data types, and features available for developers.

## Table of Contents

1. [Variables & Types](#1-variables--types)
2. [Data Structures](#2-data-structures)
3. [Control Flow](#3-control-flow)
4. [Functions](#4-functions)
5. [Custom Structures](#5-custom-structures)
6. [Modules & Imports](#6-modules--imports)
7. [Built-ins](#7-built-ins)
8. [Syntactic sugar](#8-syntactic-sugar)

---

## 1. Variables & Types

Noopy is a strongly typed language that supports both explicit type declaration and type inference (auto).

### Variable Declaration

You can declare variables in two ways:

**1. Type Inference (Auto)**
The compiler guesses the type based on the value.
```
x = 42;              // Infers 'int'
is_active = True;    // Infers 'bool'
```

**2. Explicit Typing**
You can strictly define the type of a variable.
```
count: int = 10;
flag: bool = False;
nothing: void = void;
```

### Primitive Types

| Type   | Description | Example |
| :---   | :--- | :--- |
| `int`  | Signed Integer (64-bit max) | `42`, `-10` |
| `bool` | Boolean value | `True`, `False` |
| `void` | Null / No value | `void` |

---

## 2. Data Structures

### Strings
In Noopy, strings are syntactic sugar for **lists of characters**.
```
name = "Noopy";
// Equivalent to type: [char]
```

### Lists
Lists are homogeneous collections of elements.

```
// List of integers
numbers: [int] = [1, 2, 3, 4];

// List of lists
matrix: [[int]] = [[1, 0], [0, 1]];

// Accessing elements (0-indexed)
first = numbers[0];
```

---

## 3. Control Flow

### Conditionals (If / Else)

Parentheses around the condition are mandatory. Braces `{}` define the scope.

```
if (x > 100) {
    print("Large number");
} else if (x > 50) {
    print("Medium number");
} else {
    print("Small number");
}
```

### Loops

**While Loop**
Executes as long as the condition is true.
```
i = 0;
while (i < 5) {
    print(i);
    i = i + 1;
}
```

**For Loop**
Classic C-style loop with initialization, condition, and update.
```
// for (init; condition; update)
for (i = 0; i < 10; i = i + 1) {
    print(i);
}
```

---

## 4. Functions

### Named Functions
Defined using the `func` keyword. Arguments must have types. Return type is specified after `->` (defaults to `void` if omitted).

```
// Function with return value
func add(a: int, b: int) -> int {
    ret a + b;
}

// Function without return value
func log_message(msg: [char]) {
    print(msg);
}

// Calling functions
result = add(10, 5);
```

### Lambda Functions (Anonymous)
Functions can be treated as values and passed around using `lambda`.

```
// Defining a lambda
multiply = lambda(a, b) -> a * b;

// Usage
res = multiply(3, 4);
```

---

## 5. Custom Structures

You can define your own data types using `struct`.

### Definition
```
struct Vector2 {
    x: int;
    y: int;
}

struct Player {
    name: [char];
    pos: Vector2;
    hp: int;
}
```

### Instantiation (`new`)
Use the `new` keyword to create an instance.

```
my_vec = new Vector2 {
    x: 10,
    y: 20
};
```

### Field Access
Access fields using the dot `.` notation.

```
print(my_vec.x); // Prints 10
```

---

## 6. Modules & Imports

You can organize your code into multiple files. Files meant to be imported must have the **`.npy`** extension.

**File: `math_utils.npy`**
```
func square(x: int) -> int {
    ret x * x;
}
```

**File: `main.npy`**
```
import "math_utils.npy";

func main() {
    print(square(5)); // Prints 25
}
```

---

## 7. Built-ins

Standard operators and functions available globally.

| Operator | Description |
| :--- | :--- |
| `+`, `-`, `*` | Arithmetic operators |
| `/`, `div` | Integer division |
| `%`, `mod` | Modulo |
| `==`, `!=` | Equality check |
| `<`, `>`, `<=`, `>=` | Comparison |
| `&&`, `!`, `\|\|` | Logical operators |
| `print(val)` | Prints a value to stdout |
|`int8(integer)`, `uint8(integer)`, `int16(integer)`, `uint16(integer)`, `int32(integer)`, `uint32(integer)`, `int64(integer)`, `uint64(integer)`, `char(integer)`, `uchar(integer)` | Casts value to specific Integer type|
|`exit(code)`, `head(list)`, `cons(list)`, `tail(lsit)`, `nth(list, index)`| Standard list manipulation functions|


### 8.  Syntactic sugar

All types of syntactic sugar available.

| User Syntax|
| :--- |
| `i++` or `++i` |
| `i--` or `--i` |
| `x += y` |
| `x -= y` |
| `x *= y` |
| `x /= y` |
| `x >= y` or `x <= y` |
| `x != y` |
