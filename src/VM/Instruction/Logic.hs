{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Logic Instructions
-}

{-|
Module      : VM.Instruction.Logic
Description : Implementation of comparison and logic operations.
Stability   : stable
-}
module VM.Instruction.Logic
    ( instEq
    , instLt
    , instLe
    , instNot
    , instAnd
    , instOr
    ) where

import VM.VMState (VirtualMachine)
import VM.VMValue (VMValue(..), eqValue)
import VM.VMStack (stackPush, stackPop)
import Common.Type.Integer (intValueToInt)

-- | Implements Logical NOT (Opcode 0x22).
--
-- @details
--   Pops a boolean and pushes its inverse.
--   Stack: [a] -> [not a]
--
-- @throws
--   Error if the popped value is not a VBool.
--
instNot :: VirtualMachine ()
instNot = do
    v <- stackPop
    case v of
        VBool b -> stackPush (VBool (not b))
        _ -> error "VM Error: NOT expects Boolean"

-- | Implements Logical AND (Opcode 0x23).
--
-- @details
--   Pops two booleans and pushes their conjunction.
--   Stack: [a, b] -> [a && b]
--
instAnd :: VirtualMachine ()
instAnd = do
    v2 <- stackPop
    v1 <- stackPop
    case (v1, v2) of
        (VBool b1, VBool b2) -> stackPush (VBool (b1 && b2))
        _ -> error "VM Error: AND expects Booleans"

-- | Implements Logical OR (Opcode 0x24).
--
-- @details
--   Pops two booleans and pushes their disjunction.
--   Stack: [a, b] -> [a || b]
--
instOr :: VirtualMachine ()
instOr = do
    v2 <- stackPop
    v1 <- stackPop
    case (v1, v2) of
        (VBool b1, VBool b2) -> stackPush (VBool (b1 || b2))
        _ -> error "VM Error: OR expects Booleans"

-- | Implements Equality Comparison (Opcode 0x20).
--
-- @details
--   Pops two values and performs a deep equality check.
--   Supports: Ints (loose), Bools, Lists (recursive), Structs, FuncPtrs.
--
instEq :: VirtualMachine ()
instEq = do
    v2 <- stackPop
    v1 <- stackPop
    stackPush (VBool (eqValue v1 v2))

-- | Implements Less Than Comparison (Opcode 0x21).
--
-- @details
--   Pops two values and checks if the second popped (Left) is less than the first (Right).
--   Stack: [a, b] -> [a < b] (Push VBool)
--
instLt :: VirtualMachine ()
instLt = do
    v2 <- stackPop
    v1 <- stackPop
    case (v1, v2) of
        (VInt i1, VInt i2) -> stackPush (VBool (
            intValueToInt i1 < intValueToInt i2))
        _ -> error "VM Error: LT expects Integers"

-- | Implements Less or Equal Comparison (Opcode 0x25).
--
-- @details
--   Pops two integers and checks if Left <= Right.
--   Stack: [a, b] -> [a <= b]
--
instLe :: VirtualMachine ()
instLe = do
    v2 <- stackPop
    v1 <- stackPop
    case (v1, v2) of
        (VInt i1, VInt i2) -> stackPush (VBool (
            intValueToInt i1 <= intValueToInt i2))
        _ -> error "VM Error: LE expects Integers"
