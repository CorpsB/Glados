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
    , instGetStructField
    , instAttrUpdate
    , pushStruct
    ) where

import qualified Data.Vector as V
import Control.Monad.State.Strict (get, put)

import VM.VMState (VMState(..), VirtualMachine)
import VM.VMValue (VMValue(..))
import VM.VMStack (stackPush, stackPop)
import VM.Bytecode.Reader (readInt32)
import Common.Type.Integer (intValueToInt)

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

-- | Implements GET_STRUCT_FIELD (Opcode 0x63).
--
-- @details
--   Reads a field index and retrieves the value from a Struct on the stack.
--   1. Reads 'index' (Int32).
--   2. Pops 'struct' (VStruct).
--   3. Pushes 'struct[index]'.
--
-- @throws
--   Error if the popped value is not a VStruct or if index is out of bounds.
--
instGetStructField :: VirtualMachine ()
instGetStructField = do
    idx <- readInt32
    v <- stackPop
    case v of
        VStruct fields -> case idx >= 0 && idx < V.length fields of
            True  -> stackPush (fields V.! idx)
            False -> error $ "VM Error: Struct Field Access Out of Bounds " ++
                "(" ++ show idx ++ ")"
        _ -> error "VM Error: GET_STRUCT_FIELD expects a Struct"

-- | Implements ATTR_UPDATE (Opcode 0x64).
--
-- @details
--   Updates a field in a structure.
--   Because structures are immutable, this creates a shallow copy.
--
--   Stack Order:
--     Top    -> New Value
--     Next   -> Field Index (Int)
--     Bottom -> Struct
--
instAttrUpdate :: VirtualMachine ()
instAttrUpdate = do
    v <- stackPop
    idx <- stackPop
    struct <- stackPop
    case (struct, idx) of
        (VStruct vec, VInt vi) -> let i = intValueToInt vi in
            case i >= 0 && i < V.length vec of
                True -> stackPush (VStruct (vec V.// [(i, v)]))
                False -> error $ "VM Error: attr_update OOB (" ++ show i ++ ")"
        (other, _) -> error $ "VM Error: attr_update not struct " ++ show other
