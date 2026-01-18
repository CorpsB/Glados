# ASM_SPEC.md

## Overview
Binary format `.gla` for the GLADOS project VM.
- **Endianness**: Big-Endian (BE) for all multi-byte values.
- **Scope**: Contract between Compiler (Team A) and Assembler/VM (Team B).
- **Version**: `0x02` (Simplified types).

## File header
Bytes:
- `0x00..0x03`: MAGIC = `"GLAD"` (0x47 0x4C 0x41 0x44)
- `0x04`: VERSION (`0x02`)
- `0x05`: FLAGS (bitmask, `0x00` default)
- `0x06..0x09`: CODE_SIZE (Int32 BE) - size in bytes of the instruction segment
- `0x0A..`: BYTECODE

<br>

---

# Instruction Set

> Arithmetic, logic, and comparison instructions do not take explicit arguments. They implicitly consume their operands from the stack (as shown in Stack Effect).

## 1. Stack Operations
| Name (Opcode) | Description | Usage (ASM) | Stack Effect |
|---------------|-------------|-------------|--------------|
| `0x01 PUSH` | Pushes a typed immediate value onto the stack. | `PUSH [type] [val]` | `( ... ) -> ( ..., val )` |
| `0x02 POP` | Pops and discards the top value. | `POP` | `( ..., val ) -> ( ... )` |
| `0x03 DUP` | Duplicates the top value. | `DUP` | `( ..., val ) -> ( ..., val, val )` |
| `0x04 SWAP` | Swaps the two topmost values. | `SWAP` | `( ..., a, b ) -> ( ..., b, a )` |

## 2. Arithmetic
| Name (Opcode) | Description | Usage (ASM) | Stack Effect |
|---------------|-------------|-------------|--------------|
| `0x10 ADD` | Adds `a` and `b`. | `ADD` | `( ..., a, b ) -> ( ..., a+b )` |
| `0x11 SUB` | Subtracts `b` from `a` (`a - b`). | `SUB` | `( ..., a, b ) -> ( ..., a-b )` |
| `0x12 MUL` | Multiplies `a` by `b`. | `MUL` | `( ..., a, b ) -> ( ..., a*b )` |
| `0x13 DIV` | Divides `a` by `b`. Error if `b==0`. | `DIV` | `( ..., a, b ) -> ( ..., a/b )` |
| `0x14 MOD` | Modulo `a % b`. Error if `b==0`. | `MOD` | `( ..., a, b ) -> ( ..., a%b )` |

## 3. Logic & Comparison
| Name (Opcode) | Description | Usage (ASM) | Stack Effect |
|---------------|-------------|-------------|--------------|
| `0x20 EQ` | Equality (`a == b`). | `EQ` | `( ..., a, b ) -> ( ..., bool )` |
| `0x21 LT` | Strictly less than (`a < b`). | `LT` | `( ..., a, b ) -> ( ..., bool )` |
| `0x25 LE` | Less than or equal (`a <= b`). | `LE` | `( ..., a, b ) -> ( ..., bool )` |
| `0x22 NOT` | Boolean negation. | `NOT` | `( ..., bool ) -> ( ..., !bool )` |
| `0x23 AND` | Logical AND (`a && b`). | `AND` | `( ..., a, b ) -> ( ..., bool )` |
| `0x24 OR` | Logical OR (`a || b`). | `OR` | `( ..., a, b ) -> ( ..., bool )` |

## 4. Flow Control
| Name (Opcode) | Description | Usage (ASM) | Stack Effect |
|---------------|-------------|-------------|--------------|
| `0x30 JUMP` | Unconditional jump (`ip += off`). | `JUMP [off]` | `( ... ) -> ( ... )` |
| `0x31 JUMP_IF_FALSE` | Jumps if `cond` is false. | `JUMP_IF_FALSE [off]` | `( ..., cond ) -> ( ... )` |
| `0x32 JUMP_IF_TRUE` | Jumps if `cond` is true. | `JUMP_IF_TRUE [off]` | `( ..., cond ) -> ( ... )` |

## 5. Functions & Calls
| Name (Opcode) | Description | Usage (ASM) | Stack Effect |
|---------------|-------------|-------------|--------------|
| `0x40 CALL` | Calls the function at the given offset. | `CALL [off]` | `( ..., args ) -> ( ..., ret )` |
| `0x41 TAILCALL` | Tail call (replaces current frame). | `TAILCALL [off]` | `( ..., args ) -> ( ..., ret )` |
| `0x42 CALL_INDIRECT` | Calls the address/closure on top of the stack. | `CALL_INDIRECT` | `( ..., func, args ) -> ( ..., ret )` |
| `0x43 RET` | Returns from the current function. | `RET` | `( ..., retVal ) -> (Vide frame) -> (Caller: ..., retVal)` |

## 6. Memory (Variables)
| Name (Opcode) | Description | Usage (ASM) | Stack Effect |
|---------------|-------------|-------------|--------------|
| `0x50 LOAD_LOCAL` | Loads local/argument `idx`. | `LOAD_LOCAL [idx]` | `( ... ) -> ( ..., val )` |
| `0x51 STORE_LOCAL` | Stores the top value into local `idx`. | `STORE_LOCAL [idx]` | `( ..., val ) -> ( ... )` |
| `0x52 LOAD_GLOBAL` | Loads global `idx`. | `LOAD_GLOBAL [idx]` | `( ... ) -> ( ..., val )` |
| `0x53 STORE_GLOBAL` | Stores the top value into global `idx`. | `STORE_GLOBAL [idx]` | `( ..., val ) -> ( ... )` |
| `0x54 LOAD_CAPTURE` | Loads captured variable `idx`. | `LOAD_CAPTURE [idx]` | `( ... ) -> ( ..., val )` |
| `0x55 STORE_CAPTURE` | Stores the top value into capture `idx`. | `STORE_CAPTURE [idx]` | `( ..., val ) -> ( ... )` |

## 7. Closures & Types
| Nom (Opcode) | Description | Utilisation (ASM) | Stack Effect |
| ------------ | :--- | :--- | :--- |
| `0x60 MAKE_CLOSURE` | Creates a closure with `n` captures. | `MAKE_CLOSURE [addr] [n]` | `( ..., v1..vn ) -> ( ..., clos )` |
| `0x61 GET_FUNC_ADDR` | Pushes a function address onto the stack. | `GET_FUNC_ADDR [id]` | `( ... ) -> ( ..., addr )` |
| `0x80 CAST` | Converts `val` to `TypeID`. | `CAST [type]` | `( ..., val ) -> ( ..., newVal )` |

## 8. System / Debug
| Name (Opcode) | Description | Usage (ASM) | Stack Effect |
|---------------|-------------|-------------|--------------|
| `0x70 PRINT` | Prints the value (stdout). | `PRINT` | `( ..., val ) -> ( ... )` |
| `0x71 HALT` | Stops the VM (Success). | `HALT` | `( ... ) -> ( Stop )` |
| `0xFE CHECK_STACK` | Checks stack depth >= N. | `CHECK_STACK [N]` | `( ... ) -> ( ... )` |
| `0xFF NOP` | No operation. | `NOP` | `( ... ) -> ( ... )` |

<br>

---

# PUSH Type System
The system uses one-byte TypeIDs that specify value type and immediate size.

## Type Table (TypeID)
| TypeID (hex) | Type   | Immediate Size (bytes)  | Description                              |
|--------------|--------|-------------------------|------------------------------------------|
| `0x00`       | Bool   | 1                       | `0x00` = False, `0x01` = True            |
| `0x01`       | i8     | 1                       | signed 8-bit integer                     |
| `0x02`       | u8     | 1                       | unsigned 8-bit integer                   |
| `0x03`       | i16    | 2                       | signed 16-bit integer                    |
| `0x04`       | u16    | 2                       | unsigned 16-bit integer                  |
| `0x05`       | i32    | 4                       | signed 32-bit integer                    |
| `0x06`       | u32    | 4                       | unsigned 32-bit integer                  |
| `0x07`       | i64    | 8                       | signed 64-bit integer                    |
| `0x08`       | u64    | 8                       | unsigned 64-bit integer                  |
| `0x09..`     | *Reserved* | -                   | Reserved (floats, pointers, etc.)        |

**PUSH instruction (opcode `0x01`)**  
Layout: `[0x01] [TypeID] [Immediate bytes]`  
The VM reads the TypeID to know how many bytes to read next.

*Examples:*  
- `PUSH Bool True`  -> `01 00 01`  
- `PUSH i32 500`    -> `01 05 00 00 01 F4`

<br>

---

# Promotion & arithmetic rules (detailed)
1. **Runtime values carry TypeID**. All operations consult TypeIDs and apply promotion rules.
2. **Promotion algorithm (deterministic):**
   - If both operands share same signedness (both signed or both unsigned) → choose width = max(widths). Operation performed in that width (or using internal wider temporary, e.g., 64-bit).
   - If operands differ in signedness:
      - If one operand width > other's width, promote the smaller-width operand to the larger width with correct extension (sign or zero extend as appropriate), then:
         - If larger-width operand is signed and large enough: perform signed operation.
         - If ambiguity remains (e.g., i32 + u32), **VM default policy**: widen to signed 64 (`i64`) and perform signed operation; alternative policies are allowed but must be implemented consistently and documented by Team B. Compiler can avoid ambiguity by inserting explicit CASTs.
3. **Result narrowing:** After computation, if result fits in smaller width preserving signedness, VM may narrow to that minimal width; otherwise result remains at promoted width. If result exceeds maximum representable width (e.g., > 64-bit) -> runtime error (unless VM supports BigInt).
4. **Comparison rules:** follow same promotion rules, then compare. If both are unsigned, use unsigned comparison semantics.
5. **DIV / MOD:** division semantics follow signedness: signed division truncates toward zero (common choice); unsigned division uses unsigned semantics. Division by zero -> runtime error.

<br>

---

# Runtime error policy (reiteration)
VM must immediately terminate with a stderr message and **exit 84** for:
- Division or modulo by zero.
- Stack underflow.
- Invalid LOAD/STORE index.
- Type mismatch for an operation.
- Invalid jump target.

<br>

---

# Assembler conventions & pseudo-instructions
- Labels: `fun_[name]_[arity]`, `lambda_[id]_[arity]`, `lbl_[id]`.
- Pseudo forms: `LabelDef name`, `JumpLabel name`, `CallLabel name`, `Real Instruction`.
- Two-pass assembly:
   - Pass1: compute label offsets using `pseudoSize`.
   - Pass2: replace placeholders with concrete instructions (compute offsets using `offset = target - (cur + sizeOfInstr)`).
- Validations: unknown label -> error; target not at instruction boundary -> error.

<br>

---

# Versioning
- Increment `VERSION` byte for any change to encoding, TypeID semantics, opcode table, or offset formula.
