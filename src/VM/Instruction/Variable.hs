{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Variable Instructions
-}

{-|
Module      : VM.Instruction.Variable
Description : Implementation of local variable access instructions.
Stability   : stable

Handles LOAD_LOCAL and STORE_LOCAL operations.
These instructions access the stack relative to the current Frame Pointer (FP).
This allows functions to access their arguments (negative offsets) and 
local variables (positive offsets) regardless of the absolute stack depth.
-}
module VM.Instruction.Variable
    ( instLoadLocal
    , instStoreLocal
    , instLoadCapture
    , instStoreCapture
    ) where

import Control.Monad.State.Strict (get, put)
import qualified Data.Vector as V

import VM.VMState (VirtualMachine, VMState(..))
import VM.Bytecode.Reader (readInt32)
import VM.VMStack (stackPush, stackPop)

-- | Implements LOAD_LOCAL (Opcode 0x50).
--
-- @details
--   1. Reads the signed 32-bit index (offset) from the bytecode.
--   2. Calculates the absolute index: Frame Pointer (FP) + Offset.
--   3. Copies the value at that index to the top of the stack.
--
--   This instruction is used to retrieve arguments and local variables.
--
-- @throws
--   Error "VM Error: LOAD_LOCAL out of bounds" if the calculated index 
--   is outside the valid stack range.
--
instLoadLocal :: VirtualMachine ()
instLoadLocal = do
    idx <- readInt32
    vm <- get
    let baseIdx = baseVStackIndex vm + idx
    let stack = vStack vm
    case baseIdx >= 0 && baseIdx < V.length stack of
        False -> error $ "VM Error: LOAD_LOCAL out of bounds (Index: " ++
            show baseIdx ++ ", Size: " ++ show (V.length stack) ++ ")"
        True -> stackPush (stack V.! baseIdx)

-- | Implements STORE_LOCAL (Opcode 0x51).
--
-- @details
--   1. Reads the signed 32-bit index (offset) from the bytecode.
--   2. Pops the value to store from the top of the stack.
--   3. Calculates the absolute index: Frame Pointer (FP) + Offset.
--   4. Updates the stack at that index with the popped value.
--
--   This instruction is used to assign values to existing variables.
--
-- @throws
--   Error "VM Error: STORE_LOCAL out of bounds" if the calculated index 
--   is outside the valid stack range.
--
instStoreLocal :: VirtualMachine ()
instStoreLocal = do
    idx <- readInt32
    v <- stackPop
    vm <- get
    let baseIdx = baseVStackIndex vm + idx
    let stack = vStack vm
    case baseIdx >= 0 && baseIdx < V.length stack of
        False -> error $ "VM Error: STORE_LOCAL out of bounds (Index: " ++
            show baseIdx ++ ", Size: " ++ show (V.length stack) ++ ")"
        True -> put $ vm { vStack = stack V.// [(baseIdx, v)] }

-- | Implements LOAD_CAPTURE (Opcode 0x54).
--
-- @details
--   Accesses a variable in the current function's captured environment.
--   1. Reads the capture index (Int32).
--   2. Pushes the value at 'env[index]' onto the stack.
--
instLoadCapture :: VirtualMachine ()
instLoadCapture = do
    idx <- readInt32
    vm <- get
    let currEnv = env vm
    case idx >= 0 && idx < V.length currEnv of
        False -> error $ "VM Error: LOAD_CAPTURE out of bounds (" ++
            show idx ++ ")"
        True -> stackPush (currEnv V.! idx)

-- | Implements STORE_CAPTURE (Opcode 0x55).
--
-- @details
--   Updates a variable in the current function's captured environment.
--   1. Reads the capture index.
--   2. Pops the new value.
--   3. Writes to 'env[index]'.
--
instStoreCapture :: VirtualMachine ()
instStoreCapture = do
    idx <- readInt32
    val <- stackPop
    vm <- get
    let currEnv = env vm
    case idx >= 0 && idx < V.length currEnv of
        False -> error $ "VM Error: STORE_CAPTURE out of bounds (" ++
            show idx ++ ")"
        True -> put $ vm { env = currEnv V.// [(idx, val)] }
