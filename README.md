NOOPY PROJECT
=============

Noopy is a programming language developed as part of a third-year project at Epitech Strasbourg.
The project focuses on designing and implementing a custom programming language with a clear
syntax inspired by low-level languages such as C, while providing simplified native libraries
to ease learning and experimentation.

Noopy is intended for students and developers who want to better understand language design,
compilation concepts, and low-level programming paradigms through a structured yet accessible
language.


CURRENT VERSION
---------------

Noopy 2.2 introduces multiple fixes, including improvements to lambda handling and release
mechanisms, as well as the first native libraries.


DOCUMENTATION
-------------

Developer documentation:
https://corpsb.github.io/Glados/AST-Semantics-Check.html

User documentation: https://glados.ncarabin.cloud

GLaDOS / Noopy – Feature Checklist
================================

Legend:
- [ ] Not implemented / To do
- [x] Implemented


--------------------------------
Part 0 – Tooling, Tests & CI
--------------------------------
- [x] Build system present (Makefile)
- [x] Project builds from a fresh clone
- [x] Language implemented in Haskell
- [x] Binary named `glados`
- [x] Proper exit codes (0 on success, 84 on error)
- [x] Error messages written to stderr

- [x] Unit tests
- [x] Integration tests
- [x] Test coverage available
- [x] Continuous Integration (CI)
- [x] Continuous Delivery (automatic release build)


--------------------------------
Part 1 – Minimal LISP Interpreter
--------------------------------
- [x] S-Expression parser
- [x] Atoms: integers
- [x] Atoms: symbols
- [x] Lists (nested S-expressions)
- [x] Read program from stdin
- [x] Stop execution on first error
- [x] Exit code 84 on error

Core concepts:
- [x] 64-bit integers
- [x] Boolean values (#t / #f)
- [x] Procedure / function type

Bindings:
- [x] Symbol binding
- [x] Error on unbound symbol

Functions & lambdas:
- [x] Anonymous functions (lambda)
- [x] Named functions
- [x] Function calls
- [x] Recursive functions

Conditionals:
- [x] if expression

Built-in functions:
- [x] eq?
- [x] <
- [x] +
- [x] -
- [x] *
- [x] div
- [x] mod


--------------------------------
Part 2 – Custom Language (Noopy)
--------------------------------
Security & robustness:
- [x] Strong typing
- [x] Type checking before execution
- [x] Runtime error handling

Syntax & semantics:
- [x] Custom syntax (not S-expressions)
- [x] Blocks with braces
- [x] Expressions split across lines
- [x] Infix arithmetic operators
- [x] Operator precedence
- [x] Syntactic sugar

Types:
- [x] int
- [x] bool
- [x] void
- [x] Lists
- [x] Strings (as list of char)
- [x] Structs

Variables:
- [x] Variable declaration
- [x] Type inference (auto)
- [x] Explicit typing
- [x] Mutable variables

Control flow:
- [x] if / else if / else
- [x] while loops
- [x] for loops

Functions:
- [x] Named functions
- [x] Lambdas
- [x] Closures
- [x] Return values
- [x] Functions as values

Data structures:
- [x] Lists
- [x] Nested lists
- [x] Structs
- [x] Field access

Modules:
- [x] Import system
- [x] Multi-file programs

Built-ins:
- [x] print
- [x] exit
- [x] List manipulation (head, tail, cons, nth)
- [x] Cast functions (int8, int16, int32, int64, etc.)


--------------------------------
Evaluation & Compilation
--------------------------------
- [x] AST generation
- [x] Semantic analysis
- [x] Compiler
- [x] Virtual Machine
- [x] Custom instruction set
- [x] Bytecode format
- [x] Human-readable disassembly
- [x] VM executes bytecode
- [x] Tail-call support (TAILCALL instruction)


--------------------------------
Documentation
--------------------------------
- [x] Developer documentation
- [x] User manual (complete)
- [x] Grammar description
- [x] Compilation pipeline documented
- [x] Security design explained


--------------------------------
Bonus Features
--------------------------------
- [x] Type inference
- [x] Lists with syntactic sugar ([1, 2, 3])
- [x] Strings
- [x] Structs
- [x] Closures
- [x] Imperative constructs
- [x] Loops
- [x] Tail-call optimisation
- [x] Extensive functional test suite
- [x] Clear ASM / VM specification

- [ ] Floating point numbers
- [x] File I/O
- [ ] Foreign Function Interface (FFI)
- [ ] Networking / graphics bindings
- [ ] Additional backend (WASM, native, etc.)
- [ ] Second VM implementation
- [ ] Metaprogramming (macros)

PREREQUISITES
=============

- Haskell toolchain (compatible with GHC 9.10.3)
- Stack
- Make

Optional:
- Docker (to reproduce the CI environment)



BUILD
=====

make        : build the project
make clean  : remove intermediate files
make fclean : remove all generated binaries
make re     : full rebuild



BUILD, RELEASES AND USAGE
========================

The project uses Stack and a Makefile.
Releases are generated automatically through the CI/CD pipeline.

Executables:
- glados     : Noopy compiler
- glados-vm  : Virtual Machine

Compilation:
    ./glados <source.npy> <output_binary>

Execution:
    ./glados-vm <output_binary>



ARCHITECTURE AND PROJECT STRUCTURE
==================================

Global overview
---------------

The project is organized around a clear separation of concerns between:

- the compiler
- the virtual machine
- shared abstractions (AST, types, errors)
- tooling, documentation, and tests

The main compilation flow is:
Parser → AST → Semantic checks → ASM generation → Bytecode → Execution by the VM


Top-level directories
---------------------
```
app/
    Entry points of the project.
    - Main.hs        : compiler executable (glados)
    - VM/Main.hs     : virtual machine executable (glados-vm)

src/
    Core implementation of the language.

    AST/
        Abstract Syntax Tree definitions and semantic analysis.
        - Ast.hs                  : core AST definitions
        - Semantics/              : semantic checks (types, calls, validity)

    Parser/
        Lexing and parsing logic.
        Responsible for converting source files (.npy) into an AST.
        Includes expression parsing, statements, conditions, imports, etc.

    Compiler/
        Compilation pipeline from AST to bytecode.
        - ASM/                    : intermediate assembly representation
        - Bytecode/               : bytecode encoding and serialization
        - ResolveLabels/          : label resolution passes
        - CompilerState.hs        : compiler state management

    VM/
        Virtual machine responsible for executing compiled bytecode.
        Includes:
        - instruction implementations
        - stack and state management
        - bytecode reader and runner

    Common/
        Shared utilities used by both the compiler and the VM.
        - Error handling
        - Type definitions
        - Helper utilities (lists, bytecode helpers)

    @extension/
        Editor tooling.
        - VS Code extension (syntax highlighting, snippets)

    @lib/
        Native or internal libraries provided to the language.


test/
    Automated test suite.

    Functional/
        End-to-end functional tests.
        Each test consists of a `.npy` source file executed through:
        compiler → VM → output comparison.

    Unit/
        Unit tests written in Haskell.
        Tests are organized by component:
        - AST
        - Parser
        - Compiler
        - VM
        - Common utilities


doc/
    Documentation related to the compiler and bytecode.
    Includes:
    - ASM specifications
    - Compiler documentation
    - Contribution guidelines (FR / EN)


tools/
    Development and CI helper scripts.
    - coding style checks
    - coverage tools
    - commit helpers
```


TESTING
=======

Unit tests:
    test/Unit/

Functional tests:
    test/Functional/

Run functional tests:
    make
    ./test/Functional/test.sh


CONTRIBUTORS
============

**Jason KOENIG (OkotEgarim)**
- Lead Developer · Project Management · VM Engineer · Compiler Engineer · Lib IO
- https://github.com/OkotEgarim

**Thibaut LOUIS (Turtelthib)**
- Parsing (Lisp & Noopy) · Error Management
- https://github.com/Turtelthib

**Pierre-Louis SCHORSCH (simsipierre)**
- VM Engineer · Compiler Engineer · Math Library
- https://github.com/simsipierre

**Thomas CAMUSET (2k5Type0)**
- Compiler Engineer · String Library
- https://github.com/2k5Type0

**Tom RUDOLF (Frogoth)**
- Compiler Engineer · Assert Library · Example Project (Crocus)
- https://github.com/Frogoth

**Noé CARABIN (CorpsB)**
- Project Management · CI/CD · Testing Policy
- https://github.com/CorpsB