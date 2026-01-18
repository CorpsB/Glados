{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Stack Instructions
-}

{-|
Module      : VM.Instruction.Stack
Description : Implementation of stack management opcodes.
Stability   : stable

Handles instructions that modify the stack structure itself (PUSH, POP, DUP).
This module relies on 'VM.BytecodeReader' to decode operands of varying sizes.
-}
module VM.Instruction.Stack
    ( instPush
    , instPop
    , instDup
    , instSwap
    , instCast
    , instTypeOf
    ) where

import Data.Word (Word8)
import Common.Type.Integer (IntValue(..))
import VM.VMState (VirtualMachine)
import VM.VMValue
    ( VMValue(..)
    , castValue
    , getValueName
    , stringToValue
    )
import VM.Bytecode.Reader
    ( readByte
    , readInt8
    , readWord8
    , readInt16
    , readWord16
    , readInt32
    , readWord32
    , readInt64
    , readWord64
    )
import VM.VMStack
    ( stackPush
    , stackPop
    , stackTop
    )

-- | Helper function to parse and push a value based on its TypeID.
--
-- @args
--   - typeId: The 8-bit identifier representing the type of the value to read.
--
-- @details
--   Matches the 'typeId' against supported types defined in the ASM spec,
--   consumes the appropriate number of bytes from the reader, constructs
--   the 'VMValue', and pushes it onto the stack.
--
--   **Supported TypeIDs:**
--   * 0x00: Bool (1 byte)
--   * 0x01: Int8 (1 byte)
--   * 0x02: UInt8 (1 byte)
--   * 0x03: Int16 (2 bytes)
--   * 0x04: UInt16 (2 bytes)
--   * 0x05: Int32 (4 bytes)
--   * 0x06: UInt32 (4 bytes)
--   * 0x07: Int64 (8 bytes)
--   * 0x08: UInt64 (8 bytes)
--   * 0x09: Char (1 byte, treated as Int8)
--   * 0x10: UChar (1 byte, treated as UInt8)
--
-- @throws
--   Error "VM Error: Unsupported PUSH TypeID" if the provided TypeID is unknown.
--
stackPushValue :: Word8 -> VirtualMachine ()
stackPushValue 0x00 = do v <- readByte; stackPush (VBool (v /= 0))
stackPushValue 0x01 = do v <- readInt8; stackPush (VInt (I8 v))
stackPushValue 0x02 = do v <- readWord8; stackPush (VInt (UI8 v))
stackPushValue 0x03 = do v <- readInt16; stackPush (VInt (I16 v))
stackPushValue 0x04 = do v <- readWord16; stackPush (VInt (UI16 v))
stackPushValue 0x05 = do
    v <- readInt32
    stackPush (VInt (I32 (fromIntegral v))) -- Int32 cast
stackPushValue 0x06 = do v <- readWord32; stackPush (VInt (UI32 v))
stackPushValue 0x07 = do v <- readInt64; stackPush (VInt (I64 v))
stackPushValue 0x08 = do v <- readWord64; stackPush (VInt (UI64 v))
stackPushValue 0x09 = do v <- readInt8; stackPush (VInt (IChar v))
stackPushValue 0x10 = do v <- readWord8; stackPush (VInt (UIChar v))
stackPushValue t = error $ "VM Error: Unsupported PUSH TypeID: 0x" ++ show t

-- | Implements the PUSH instruction (Opcode 0x01).
--
-- @details
--   This is the entry point for the PUSH operation. It performs the following steps:
--   1. Reads the **TypeID** byte from the bytecode stream.
--   2. Delegates the reading of the value and the stack push to 'stackPushValue'.
--
--   This separation allows for a cleaner implementation and easier extension
--   of supported types in the future.
--
instPush :: VirtualMachine ()
instPush = do
    typeId <- readByte
    stackPushValue typeId

-- | Implements the POP instruction (Opcode 0x02).
--
-- @details
--   Removes the top element of the stack and discards it.
--   Commonly used to clean up the stack after an expression evaluation
--   where the result is not needed (e.g., `func();` where return is ignored).
--
-- @throws
--   Error "Stack Underflow" if the stack is empty.
--
instPop :: VirtualMachine ()
instPop = do
    _ <- stackPop
    return ()

-- | Implements the DUP instruction (Opcode 0x03).
--
-- @details
--   Duplicates the top element of the stack.
--   Useful for operations that consume a value but need to preserve it
--   for a subsequent operation.
--   State change: `[..., A] -> [..., A, A]`
--
-- @throws
--   Error "Stack Underflow" if the stack is empty.
--
instDup :: VirtualMachine ()
instDup = do
    v <- stackTop
    stackPush v

-- | Implements the SWAP instruction (Opcode 0x04).
--
-- @details
--   Swaps the top two elements of the stack.
--   Stack: [..., A, B] -> [..., B, A]
--
instSwap :: VirtualMachine ()
instSwap = do
    b <- stackPop
    a <- stackPop
    stackPush b
    stackPush a

-- | Implements CAST (Opcode 0x80).
--
-- @details
--   Reads a TypeID and tries to convert the top of stack to that type.
--
instCast :: VirtualMachine ()
instCast = do
    typeId <- readByte
    v <- stackPop
    stackPush (castValue typeId v)

-- | Implements TYPEOF (Opcode 0x81).
--
-- @details
--   Pops a value from the stack, determines its runtime type,
--   and pushes a string representation (e.g., "int", "list").
--
--   Stack: [value] -> ["type_name"]
--
instTypeOf :: VirtualMachine ()
instTypeOf = do
    v <- stackPop
    let typeVal = getValueName v
    stackPush (stringToValue typeVal)
