# Assembly & Label Resolution Specification

## 1. Overview

This document specifies the assembly phase responsible for resolving symbolic labels into concrete bytecode offsets and producing executable instructions. The compiler emits an intermediate list of `PsInstruction`, which may contain textual labels and label-referencing operations. The assembler transforms this list into a list of concrete `Instruction` values by resolving all labels.

Label resolution is performed in **two distinct passes**:

1. **Pass 1** computes the byte offset of every defined label.
2. **Pass 2** replaces label-based pseudo-instructions with concrete instructions using resolved offsets.

This document defines the exact size model, offset conventions, and error conditions used during this process.

---

## 2. Pseudo-instruction Model

The assembler input is a linear list of `PsInstruction` values.

### Categories

* **Concrete instructions**

  * `Real Instruction`
  * Already fully specified and directly serializable.

* **Label definition**

  * `LabelDef name`
  * Defines a symbolic label at the current byte position.
  * Generates no bytecode.

* **Label-referencing pseudo-instructions**

  * `JumpLabel name`
  * `JumpIfFalseLabel name`
  * `JumpIfTrueLabel name`
  * `CallLabel name`
  * `TailCallLabel name`
  * `MakeClosureLabel name n`
  * `GetFuncAddrLabel name`

These pseudo-instructions are placeholders that must be resolved to concrete instructions during assembly.

---

## 3. Instruction Size Model

During label resolution, each `PsInstruction` contributes a **pseudo-size** used to compute byte offsets. This size corresponds to the final size of the concrete instruction it will resolve to.

### Size Rules

| PsInstruction          | Pseudo-size (bytes)     |
| ---------------------- | ----------------------- |
| `LabelDef _`           | `0`                     |
| `Real inst`            | `instructionSize(inst)` |
| `JumpLabel _`          | `1 + 4`                 |
| `JumpIfFalseLabel _`   | `1 + 4`                 |
| `JumpIfTrueLabel _`    | `1 + 4`                 |
| `CallLabel _`          | `1 + 4`                 |
| `TailCallLabel _`      | `1 + 4`                 |
| `MakeClosureLabel _ n` | `1 + 4 + 4`             |
| `GetFuncAddrLabel _`   | `1 + 4`                 |

Where:

* `1` byte corresponds to the opcode
* `4` bytes correspond to a signed 32-bit relative offset
* Additional operands (e.g., capture count) contribute extra bytes

---

## 4. Pass 1 – Label Address Computation

Pass 1 performs a linear scan over the `PsInstruction` list to compute a mapping from label names to byte offsets.

### Algorithm

* Initialize `currentByteIndex = 0`
* Initialize an empty map `labelMap : Text -> Int`
* For each `PsInstruction` in order:

  * If the instruction is `LabelDef name`:

    * Record `labelMap[name] = currentByteIndex`
  * Otherwise:

    * Increment `currentByteIndex` by the pseudo-size of the instruction

The value stored for each label is the byte index of the **first opcode byte** of the instruction immediately following the label.

Duplicate label definitions are considered an error.

---

## 5. Pass 2 – Pseudo-instruction Resolution

Pass 2 replaces all label-based pseudo-instructions with concrete `Instruction` values using the label map computed in Pass 1.

### General Resolution Scheme

For each label-referencing pseudo-instruction:

1. Retrieve `targetByteIndex` from `labelMap`
2. Determine `currentByteIndex`, the byte index of the opcode of the current instruction
3. Compute a relative offset
4. Produce the corresponding concrete `Instruction`

Concrete `Real Instruction` values are emitted unchanged.

---

## 6. Offset Conventions

All control-flow and call instructions use **signed relative byte offsets**.

### Offset Formula

For any instruction that encodes a target address:

```
offset = targetByteIndex - (currentByteIndex + sizeOfCurrentInstruction)
```

Where:

* `currentByteIndex` is the byte index of the instruction opcode
* `sizeOfCurrentInstruction` is the full size of the instruction (opcode + operands)
* `targetByteIndex` is the byte index of the target label

The offset is therefore relative to the instruction **immediately following** the current one.

### Affected Instructions

This convention applies uniformly to:

* `Jump`
* `JumpIfFalse`
* `JumpIfTrue`
* `Call`
* `TailCall`
* `MakeClosure`
* `GetFuncAddr`

Using a single relative-offset convention ensures consistency across all control-flow and call mechanisms.

---

## 7. Error Conditions

The assembler must detect and report the following errors:

* Reference to an undefined label
* Duplicate label definitions
* Computed offset does not fit in a signed 32-bit integer
* Invalid or unsupported pseudo-instruction patterns

Assembly must fail if any such error is encountered.

---

## 8. Normative Statement

This document is normative. Any assembler implementation must strictly follow the conventions defined herein to ensure compatibility between the compiler and the virtual machine.
