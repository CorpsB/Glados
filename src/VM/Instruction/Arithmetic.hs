{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Arithmetic Instructions
-}

{-|
Module      : VM.Instruction.Arithmetic
Description : Implementation of mathematical operations.
Stability   : stable

Handles basic arithmetic. 
WARNING: Operations are performed on the top two stack elements. 
Since the stack is LIFO, the first popped element is the Right-Hand Operand.
-}
module VM.Instruction.Arithmetic
    ( instAdd
    , instSub
    , instMul
    , instDiv
    , instMod
    ) where

import Common.Type.Integer (IntValue(..), intValueToInt)
import VM.VMState (VirtualMachine)
import VM.VMValue (VMValue(..))
import VM.VMStack (stackPush, stackPop)

-- | Helper function for binary integer operations.
--
-- @args
--   - fn: The Haskell binary function to apply (e.g., (+), (-)).
--
-- @details
--   1. Pops the Right Operand (v2).
--   2. Pops the Left Operand (v1).
--   3. Checks if both are integers.
--   4. Applies 'fn' and pushes the result.
--
-- @throws
--   Error if operands are not VInt.
--
binaryInstInt :: (Int -> Int -> Int) -> VirtualMachine ()
binaryInstInt fn = do
    v2 <- stackPop
    v1 <- stackPop
    case (v1, v2) of
        (VInt i1, VInt i2) ->
            let result = fn (intValueToInt i1) (intValueToInt i2) in
            stackPush (VInt (I64 (fromIntegral result)))
        _ -> error "VM Error: Arithmetic instruction expects Integers"

-- | Implements Addition (Opcode 0x10).
--
-- Stack: [a, b] -> [a + b]
--
instAdd :: VirtualMachine ()
instAdd = binaryInstInt (+)

-- | Implements Subtraction (Opcode 0x11).
--
-- Stack: [a, b] -> [a - b] (Note: b is popped first)
--
instSub :: VirtualMachine ()
instSub = binaryInstInt (-)

-- | Implements Multiplication (Opcode 0x12).
--
-- Stack: [a, b] -> [a * b]
--
instMul :: VirtualMachine ()
instMul = binaryInstInt (*)

-- | Implements Division (Opcode 0x13).
--
-- Stack: [a, b] -> [a / b]
--
instDiv :: VirtualMachine ()
instDiv = binaryInstInt div

-- | Implements Modulo (Opcode 0x14).
--
-- Stack: [a, b] -> [a % b]
--
instMod :: VirtualMachine ()
instMod = binaryInstInt mod
