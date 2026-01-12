{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Struct
-}

{-|
Module      : VM.Instruction.Struct
Description : Implementation of structures.
Stability   : stable

Handles the creation of structures.
-}
module VM.Instruction.Struct
    ( instBuildStruct
    , pushStruct
    ) where

import qualified Data.Vector as V
import Control.Monad.State.Strict (get, put)

import VM.VMState (VMState(..), VirtualMachine)
import VM.VMValue (VMValue(..))
import VM.VMStack (stackPush)
import VM.Bytecode.Reader (readInt32)

-- | Helper for instBuildStruct to manipulate the stack.
--
-- @args
--   - vm: Current VM state
--   - count: Number of fields in the struct
--
pushStruct :: VMState -> Int -> VirtualMachine ()
pushStruct vm count =
    let start = V.length (vStack vm) - count
        fields = V.slice start count (vStack vm) in
    put (vm { vStack = V.take start (vStack vm) }) >>
       stackPush (VStruct fields)

-- | Implements BUILD_STRUCT (Opcode 0x62).
--
-- @details
--   Pop 'n' elements from the stack and wraps them in a VStruct.
--   1. Reads 'n' (Int32).
--   2. Consumes 'n' values.
--   3. Pushes VStruct.
--
instBuildStruct :: VirtualMachine ()
instBuildStruct = do
    count <- readInt32
    vm <- get
    case V.length (vStack vm) >= count of
        False -> error "VM Error: BUILD_STRUCT Stack Underflow"
        True -> pushStruct vm count
