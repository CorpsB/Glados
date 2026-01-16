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
    , instBuildList
    ) where

import qualified Data.Vector as V
import Control.Monad.State.Strict (get, put)
import VM.Bytecode.Reader (readInt32)

import VM.VMState (VirtualMachine, VMState(..))
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

-- | Implements BUILD_LIST (Opcode 0x94).
--
-- @details
--   Pop 'n' elements from the stack and creates a VList.
--   Preserves the order: [e1, e2] -> push e1 -> push e2 -> BuildList 2 -> VList [e1, e2]
--
instBuildList :: VirtualMachine ()
instBuildList = do
    count <- readInt32
    vm <- get
    let size = V.length (vStack vm)
    case size < count of
        True -> error "VM Error: BUILD_LIST Stack Underflow"
        False -> let start = size - count
                     elements = V.slice start count (vStack vm) in
                put (vm { vStack = V.take start (vStack vm) }) >>
                stackPush (VList elements)
