{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- List
-}

module VM.Instruction.List
    ( instCons
    , instHead
    , instTail
    , instNth
    ) where

import qualified Data.Vector as V

import VM.VMState (VirtualMachine)
import VM.VMValue (VMValue(..), valueToInt)
import VM.VMStack (stackPop, stackPush)

-- | Implements CONS (Opcode 0x90).
--
-- @details
--   Constructs a new list by prepending an element.
--   Stack: [..., elem, list] -> [..., new_list]
--   Note: Pops 'list' first (Right), then 'elem' (Left).
--
instCons :: VirtualMachine ()
instCons = do
    listVal <- stackPop
    elemVal <- stackPop
    case listVal of
        VList v -> stackPush (VList (V.cons elemVal v))
        _       -> error "VM Error: CONS expects a List as second argument"

-- | Implements HEAD (Opcode 0x91).
--
-- @details
--   Extracts the first element of a list.
--   Stack: [..., list] -> [..., head]
--
-- @throws Error if list is empty.
--
instHead :: VirtualMachine ()
instHead = do
    listVal <- stackPop
    case listVal of
        VList v -> case V.null v of
            True  -> error "VM Error: HEAD called on empty list"
            False -> stackPush (V.head v)
        _ -> error "VM Error: HEAD expects a List"

-- | Implements TAIL (Opcode 0x92).
--
-- @details
--   Extracts the rest of the list (minus the first element).
--   Stack: [..., list] -> [..., tail]
--
-- @throws Error if list is empty.
--
instTail :: VirtualMachine ()
instTail = do
    listVal <- stackPop
    case listVal of
        VList v -> case V.null v of
            True  -> error "VM Error: TAIL called on empty list"
            False -> stackPush (VList (V.tail v))
        _ -> error "VM Error: TAIL expects a List"

-- | Implements NTH (Opcode 0x93).
--
-- @details
--   Access to a list element.
--   Stack: [index, list] -> [element]
--
instNth :: VirtualMachine ()
instNth = do
    idxVal <- stackPop
    listVal <- stackPop
    let idx = valueToInt idxVal
    case listVal of
        VList v -> case idx >= 0 && idx < V.length v of
            True  -> stackPush (v V.! idx)
            False -> error $ "VM Error: Nth index out of bounds (" ++
                show idx ++ ")"
        _ -> error "VM Error: Nth expects a List"
