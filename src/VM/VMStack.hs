{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Stack
-}

{-|
Module      : VM.VMStack
Description : Low-level stack manipulation primitives.
Stability   : stable

This module provides the core abstractions to interact with the VM's operand stack.
It abstracts the underlying 'Vector' implementation and handles underflow safety checks.
-}
module VM.VMStack
    ( stackPush
    , stackPop
    , stackTop
    ) where

import Control.Monad.State.Strict (get, put, modify)
import qualified Data.Vector as V

import VM.VMState (VMState(..), VirtualMachine)
import VM.VMValue (VMValue)

-- | Pushes a value onto the top of the global value stack.
--
-- @args
--   - val: The 'VMValue' to push.
--
-- @details
--   Modifies the VM state by appending the value to the end of the 'vStack' vector.
--   This operation is generally O(1).
--
stackPush :: VMValue -> VirtualMachine ()
stackPush val = modify $ \s -> s { vStack = V.snoc (vStack s) val }

-- | Removes and returns the value at the top of the stack.
--
-- @details
--   Retrieves the last element of the 'vStack' vector and removes it from the state.
--   Uses 'case' instead of 'if' to avoid DoAndIfThenElse extension.
--   
-- @throws
--   Error "VM Error: Stack Underflow" if the stack is empty.
--
-- @return
--   The 'VMValue' that was at the top.
--
stackPop :: VirtualMachine VMValue
stackPop = do
    s <- get
    case V.null (vStack s) of
        True -> error "VM Error: Stack Underflow"
        False -> put (s { vStack = V.init (vStack s) }) >>
            return (V.last (vStack s))

-- | Returns the value at the top of the stack without removing it (Peek).
--
-- @details
--   Useful for instructions like DUP or conditional jumps that need to inspect
--   the value without consuming it.
--
-- @throws
--   Error "VM Error: Stack Underflow (Top)" if the stack is empty.
--
-- @return
--   The 'VMValue' currently at the top.
--
stackTop :: VirtualMachine VMValue
stackTop = do
    s <- get
    case V.null (vStack s) of
        True -> error "VM Error: Stack Underflow (Top)"
        False -> return (V.last (vStack s))
