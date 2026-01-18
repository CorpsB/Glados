# Noopy Language User Guide

This guide presents the **Noopy** programming language from the user’s point of view.
It is based on the official developer documentation and the grammar rules in order
to provide a complete overview. The goal is to explain the syntax, data types,
instruction behavior, as well as the standard libraries, with an emphasis on
semantics and practical examples.

## 1. Variables and Types

Noopy is a strongly typed language: every value has a type, and this type is checked
at compile time. Two assignment styles are possible:

1. **Declaration with inference (auto)** – The compiler deduces the type from the
   assigned value. For example:

   ```noopy
   x = 42;           // x is automatically of type int
   is_active = True; // is_active is of type bool
   ```

2. **Explicit declaration** – The type is specified after the name, followed by `:`.
   This syntax is useful for documentation and auto-completion:

   ```noopy
   count: int = 10;
   flag: bool = False;
   nothing: void = void;
   ```

### 1.1 Primitive Types

The basic available types are:

| Type | Description | Example |
| --- | --- | --- |
| `int` | 64-bit signed integer | `42`, `-10` |
| `bool` | Boolean value | `True`, `False` |
| `void` | Absence of value | `void` |
| `char` | Character | `'A'`, `'
'` |

Integers may be signed literals: a `+` or `-` sign directly attached to a digit is
part of the literal (and not a unary operator). Characters are written between
single quotes and support escape sequences (`'
'`, `'	'`, `'\\'`, `"`, etc.).

### 1.2 Composite Types

Types can be composed from base types:

- **Lists:** a list is a homogeneous container represented with brackets. The type
  `[T]` denotes a list of elements of type `T`. Lists can be nested (for example
  `[[int]]` for a matrix). A string is syntactic sugar for a list of `char`; for
  example, the variable `name = "Noopy"` has type `[char]`.

- **Structures (`struct`):** you can define your own types by grouping named fields
  (see § 5). A structure identifier can be used as a variable type.

### 1.3 Literals and Basic Expressions

Elementary expressions include:

- **Integers:** a sequence of digits optionally preceded by a sign;
- **Booleans:** `True` or `False`;
- **Characters:** `'A'` or `'
'`;
- **Strings:** `"Hello"`, which is equivalent to a list of `char`;
- **Lists:** `[1, 2, 3]` or `[[1, 2], [3, 4]]`;
- **Void:** the literal `void` represents the absence of a value.

An expression can be grouped in parentheses to force operator precedence.

### 1.4 Fixed-Width Integer Types (via Casts)

| Type | Bits | Signed |
|------|------|--------|
| `int8` | 8 | yes |
| `uint8` | 8 | no |
| `int16` | 16 | yes |
| `uint16` | 16 | no |
| `int32` | 32 | yes |
| `uint32` | 32 | no |
| `int64` | 64 | yes |
| `uint64` | 64 | no |
| `uchar` | 8 | no |

Example:
```noopy
x = 127;
x = uint32(x);
```

## 2. Data Structures

### 2.1 Lists

A list is an ordered, homogeneous collection of elements. It is written with square
brackets and comma-separated elements: `numbers = [1, 2, 3, 4];`. Empty lists are
written as `[]`. The notation `[[1,0],[0,1]]` defines a list of lists (a matrix).

Elements are accessed using bracket notation, starting at index 0:
`first = numbers[0];`. An out-of-bounds access triggers a runtime error.

Noopy provides several built-in list functions:

- `head(list)`: returns the first element. Error if the list is empty.
- `tail(list)`: returns the list without its first element. Error if empty.
- `cons(elem, list)`: adds `elem` to the front and returns a new list. The type of
  `elem` must be compatible with the element type of `list`.
- `nth(list, index)`: returns the element at position `index`. The index must be an
  integer and within bounds.

These functions do not modify their arguments; lists are immutable with respect to
these operations, but you can reassign a variable to a new list. You can also modify
an element via assignment: `numbers[2] = 42;` overwrites the third value.

### 2.2 Strings

Strings are lists of characters (`[char]`). All list operations can be used on
strings: `head("abc")` returns `'a'`, `tail("abc")` returns `"bc"`. String literals
use double quotes and support escape sequences (`\n`, `\t`, etc.).

### 2.3 User Structures

Noopy allows defining custom types using the `struct` construct. A structure groups
named, typed fields:

```noopy
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

#### Creation and Field Access

To create a structure instance, use the `new` operator followed by initial values in
a block:

```noopy
my_vec = new Vector2 {
    x: 10,
    y: 20
};
```

Each field is initialized with `field: value`. Omitted fields receive a default value
(zero or `void` depending on the type). Fields are accessed using dot notation:
`print(my_vec.x);`.

Structures are mutable; you can change a field’s value after creation:

```noopy
my_vec.x = my_vec.x + 1;
player.pos.x = 5;        // chained update on a nested field
```

Internally, this operation corresponds to a call to a special function `attr_update`
that checks the field type.

## 3. Expressions and Operators

### 3.1 Calls and Suffixes

A **primary expression** may be a literal, an identifier, or a parenthesized
expression. You can then chain suffixes:

- Function call: `funcName(arg1, arg2)`
- Indexing: `listExpr[indexExpr]`
- Field access: `structExpr.field`
- Post-increment / post-decrement: `expr++` or `expr--`

These suffixes combine freely, for example `items()[0]++`.

### 3.2 Unary and Binary Operators

Noopy distinguishes prefix operators: `!` (logical negation), `-` (arithmetic
negation), `++` and `--` (prefix increment/decrement). They have the highest
precedence.

Binary operators are ordered by decreasing precedence:

1. **Multiplicative:** `*`, `/`, `%` (or their compiler-used synonyms `div` and
   `mod`);
2. **Additive:** `+`, `-`;
3. **Order comparison:** `<`, `>`, `<=`, `>=`;
4. **Equality:** `==`, `!=` (weak equality) and `===`, `!==` (strict equality that
   compares both value and type);
5. **Logical AND:** `&&`;
6. **Logical OR:** `||`.

Operators of equal precedence are evaluated left to right. Parentheses may be used
to change evaluation order.

### 3.3 Assignment

Basic assignment is written `var = expression;` and is used both to declare and
update a variable. Compound operators such as `+=`, `-=`, `*=`, `/=`, `%=` allow
updating a variable in place:

```noopy
x += 2;  // equivalent to x = x + 2
```

More complex targets are also supported: `x.y[0].z = 10;` updates a field in a nested
structure. This syntax is supported by the implementation even though it is not
explicitly described in the developer guide.

### 3.4 Conditional `if` Expressions

In addition to the conditional statement (see § 4.1), Noopy provides a conditional
**expression**:

```noopy
result = if (condition) { expr1 } else { expr2 };
```

The `if` expression appears where a value is expected. It evaluates the condition and
returns `expr1` if true, otherwise `expr2`. The `else` block is mandatory to guarantee
a return value; however, the implementation allows it to be omitted, in which case
the returned value is `void`.

## 4. Statements and Control Flow

### 4.1 `if` Statement

To conditionally execute statements, use the statement form:

```noopy
if (x > 100) {
    print("Large value");
} else if (x > 50) {
    print("Medium value");
} else {
    print("Small value");
}
```

The condition must be enclosed in parentheses and evaluate to a boolean. Multiple
`else if` clauses may be chained. The final `else` block is optional but recommended.

### 4.2 Loops

#### 4.2.1 `while` Loop

The `while` loop evaluates the condition at the beginning of each iteration and
executes its block as long as the condition is true:

```noopy
i = 0;
while (i < 5) {
    print(i);
    i = i + 1;
}
```

#### 4.2.2 `for` Loop

The `for` loop uses the syntax `(init ; condition ; update)`. Each part is optional:
you can omit initialization or update to create infinite loops. For example:

```noopy
// Classic loop from 0 to 9
for (i = 0; i < 10; i = i + 1) {
    print(i);
}

// Infinite loop equivalent to while(true)
for (; ; ) {
    // … instructions …
}
```

In the initialization, you may declare a local variable (with or without a type). The
scope of loop variables is limited to the loop block.

### 4.3 Function Return

The statement `ret expression;` immediately exits the function and returns the value
of the expression. If the function is declared with return type `void`, you may simply
write `ret;` or `ret void;`.

## 5. Functions

### 5.1 Named Functions

A function is declared with the `func` keyword, a name, a list of typed parameters,
and optionally a return type after `->`. If the return type is not specified, it
defaults to `void`. Example:

```noopy
func add(a: int, b: int) -> int {
    ret a + b;
}

func log_message(msg: [char]) {
    print(msg);
}

result = add(10, 5);
```

Parameters are passed **by value**: modifying the parameter inside the function does
not change the caller’s argument. A function may be defined anywhere in a file, but it
is customary to group declarations at the top.

### 5.2 Anonymous Functions (Lambdas)

Noopy supports anonymous functions via the `lambda` keyword. The syntax is:

```noopy
multiply = lambda(a, b) -> a * b;
res = multiply(3, 4);
```

A lambda may be assigned to a variable or passed as an argument. The implementation
allows omitting the `->` arrow: if absent, the expression following the parentheses is
used as the body. Lambda parameters do not have explicit types; their type is inferred
from usage.

## 6. Modules and Imports

A Noopy program may be split across multiple files. Files intended for import must
use the `.npy` extension. A module is imported with `import "name.npy";`, and its
functions are accessed directly. Example:

```noopy
// math_utils.npy
func square(x: int) -> int {
    ret x * x;
}

// main.npy
import "math_utils.npy";

func main() {
    print(square(5)); // Prints 25
}
main();
```

Modules are evaluated upon import. There is no namespace system: all imported
functions share the same global namespace. Avoid giving the same name to two
functions from different modules.

## 7. Standard Library and Built-in Functions

In addition to operators, Noopy provides a small set of built-in functions:

| Name | Description |
| --- | --- |
| `print(val)` | Prints `val` to standard output and returns `void`. |
| `exit(code)` | Immediately terminates the program with an integer exit code. |
| `int8(n)`, `uint8(n)`, …, `int64(n)`, `uint64(n)` | Converts an integer to a specific-size type. An error is raised if the value is out of range. |
| `char(n)`, `uchar(n)` | Converts an integer to a character (ASCII/Unicode code). |
| `head(list)` | Returns the first element of a list. |
| `tail(list)` | Returns the list without its first element. |
| `cons(elem, list)` | Prepends an element and returns a new list. |
| `nth(list, index)` | Returns the element at position `index`. The index must be an integer. |
| `typeof(arg)` | Returns a string describing the type of `arg`. |
| `fread(path)` | Reads a file and returns its contents as a list of characters. |
| `fwrite(path, content)` | Writes `content` (a list of `char`) to a file and returns the number of bytes written. |
| `open(path, mode)` | Opens a file and returns a descriptor. Supported modes are `"r"`, `"w"`, `"a"`. |
| `read(fd, size)` | Reads `size` bytes from a file descriptor and returns a list of `char`. |
| `input(fd)` | Reads a line from standard input (or a descriptor) and returns a list of `char`. |

These functions allow common operations without importing additional modules. They
may raise errors if misused (for example, `head([])` or reading a non-existent file).

## 8. Semantics and Best Practices

### 8.1 Variable Scope

Variables declared in a block (`{ ... }`) are visible only within that block and its
sub-blocks. Variables defined at the top level of a file are global and accessible
from any function in that file (or in importing modules).

Function parameters and loop variables (`for` and `while`) are local to the function
or loop block. A global variable may be shadowed by a local variable of the same name.

### 8.2 Mutability and Copies

Integers and booleans are **immutable**: an assignment creates a new value. Lists and
structures are mutable in the sense that their components can be modified. However,
assigning one list variable to another does not create a deep copy: both variables
share the same structure. To create a modified list, use `cons`, `tail`, or manually
construct a new list.

### 8.3 Error Handling

Noopy raises runtime errors in the following cases: out-of-bounds list access, calling
`head` or `tail` on an empty list, invalid parameter types, division by zero, calling
an unknown function, etc. These errors stop program execution, as the language
currently provides no error-handling mechanism.

### 8.4 Comments

Although the official language documentation does not specify comment syntax, the
implementation accepts single-line comments (`// comment`) and multi-line comments
(`/* … */`). They are ignored by the compiler and may be used to document your code.
However, avoid abusing them: the language provides no conditional compilation via
comments.

## 9. Complete Examples

### 9.1 Hello World

The following minimal program prints `Hello, world!`:

```noopy
func main() {
    print("Hello, world!");
}
main();
```

### 9.2 Sum of a List

The code below computes the sum of a list of integers using a function and a `for`
loop:

```noopy
func sum_list(nums: [int]) -> int {
    total: int = 0;
    for (i = 0; i < nth(nums, 0); i = i + 1) {
        total += nums[i];
    }
    ret total;
}

func main() {
    print(sum_list([1,2,3,4])); // prints 10
}
main();
```

Note: to obtain the length of a list, you may define a helper function or access a
global `len` variable if exposed by the implementation. The example above uses
`nth(nums, 0)` as a placeholder, to be replaced by your own length computation.

### 9.3 Structures and Movement

Here is an example using structures to represent a point and functions to move it:

```noopy
struct Point {
    x: int;
    y: int;
}

func move_right(p: Point, dx: int) -> Point {
    p.x = p.x + dx;
    ret p;
}

func main() {
    player = new Point { x: 0, y: 0 };
    player = move_right(player, 5);
    print(player.x); // prints 5
}
```

## Conclusion

This user guide provides a complete description of the Noopy language, from data
types and control structures to built-in functions. By complementing the developer
documentation, it introduces features supported by the parser (such as `===`
operators and `if` expressions) and describes best practices. You can now use Noopy
to write robust programs and maintain a clear, strongly typed codebase.