{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Index Instructions
-}

{-|
Module      : VM.Instruction.Index
Description : Implementation of branching and jump operations.
Stability   : stable

Handles relative jumps and conditional branching instructions.
These instructions modify the 'bytecodeIndex' (Instruction Pointer) of the VMState.
-}
module VM.Instruction.Index
    ( instJump
    , instJumpIfFalse
    , instJumpIfTrue
    ) where

import Control.Monad.State.Strict (modify)

import VM.VMState (VirtualMachine, VMState(..))
import VM.VMValue (VMValue(..))
import VM.Bytecode.Reader (readInt32)
import VM.VMStack (stackPop)

-- | Implements Unconditional Jump (Opcode 0x30).
--
-- @details
--   1. Reads a signed 32-bit integer (offset) from the bytecode.
--   2. Adds this offset to the current 'bytecodeIndex'.
--
--   Note: Since 'readInt32' advances the index by 4 bytes, the offset
--   is effectively relative to the instruction immediately following the JUMP.
--
instJump :: VirtualMachine ()
instJump = do
    offset <- readInt32
    modify $ \vm -> vm { bytecodeIndex = bytecodeIndex vm + offset }

-- | Implements Jump If False (Opcode 0x31).
--
-- @details
--   Conditional jump used for 'if' statements and loops.
--   1. Reads the jump offset.
--   2. Pops a value from the stack.
--   3. If the value is 'VBool False', applies the jump.
--   4. If 'VBool True', continues execution normally (fall-through).
--
-- @throws
--   Error "VM Error: JUMP_IF_FALSE expects Boolean" if the popped value is not a boolean.
--
instJumpIfFalse :: VirtualMachine ()
instJumpIfFalse = do
    offset <- readInt32
    v <- stackPop
    case v of
        VBool False ->
            modify $ \vm -> vm { bytecodeIndex = bytecodeIndex vm + offset }
        VBool True  -> return ()
        _           -> error "VM Error: JUMP_IF_FALSE expects Boolean"

-- | Implements Jump If True (Opcode 0x32).
--
-- @details
--   Conditional jump (reverse logic of JumpIfFalse).
--   1. Reads the jump offset.
--   2. Pops a value from the stack.
--   3. If the value is 'VBool True', applies the jump.
--
-- @throws
--   Error "VM Error: JUMP_IF_TRUE expects Boolean" if the popped value is not a boolean.
--
instJumpIfTrue :: VirtualMachine ()
instJumpIfTrue = do
    offset <- readInt32
    v <- stackPop
    case v of
        VBool True ->
            modify $ \vm -> vm { bytecodeIndex = bytecodeIndex vm + offset }
        VBool False -> return ()
        _           -> error "VM Error: JUMP_IF_TRUE expects Boolean"
