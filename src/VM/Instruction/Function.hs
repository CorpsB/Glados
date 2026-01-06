{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Function Instructions
-}

{-|
Module      : VM.Instruction.Function
Description : Implementation of function call and return operations.
Stability   : stable

Handles the creation and destruction of stack frames (CallSnapshots).
Ensures proper scope management and control flow between functions.
-}
module VM.Instruction.Function
    ( instCall
    , instRet
    ) where

import Control.Monad.State.Strict (get, put)
import qualified Data.Vector as V

import VM.VMState (VirtualMachine, VMState(..))
import VM.VMValue (VMValue)
import VM.CallSnapshot (CallSnapshot(..))
import VM.Bytecode.Reader (readInt32)
import VM.VMStack (stackPop, stackPush)

-- | Implements CALL (Opcode 0x40).
--
-- @details
--   1. Reads the relative jump offset.
--   2. Creates a 'CallSnapshot' to save the current context (Return Address, Old FP).
--   3. Pushes the snapshot to the 'snapshotStack'.
--   4. Updates 'baseVStackIndex' (FP) to the current Stack Pointer (SP).
--      This marks the start of the new stack frame.
--   5. Updates 'programIndex' (IP) to jump to the function code.
--
instCall :: VirtualMachine ()
instCall = do
    offset <- readInt32
    s <- get
    let snap = CallSnapshot { callbackIndex = programIndex s,
        vStackIndex = baseVStackIndex s, vEnv = V.empty }
    put $ s { snapshotStack = snap : snapshotStack s,
        baseVStackIndex = V.length (vStack s),
        programIndex = programIndex s + offset }

-- | Implements RET (Opcode 0x43).
--
-- @details
--   1. Pops the return value from the top of the stack.
--   2. Retrieves and removes the top 'CallSnapshot'.
--   3. "Cleans" the stack by truncating it to the start of the current frame.
--      This effectively discards all local variables.
--   4. Restores the Caller's context (IP and FP).
--   5. Pushes the return value back onto the restored stack.
--
-- @throws
--   Error "VM Error: Return called with empty call stack" if executed in global scope.
--
instRet :: VirtualMachine ()
instRet = do
    retV <- stackPop
    s <- get
    case snapshotStack s of
        [] -> error "VM Error: Return called with empty call stack"
        (snap:rest) -> put (s { programIndex = callbackIndex snap,
            baseVStackIndex = vStackIndex snap, snapshotStack = rest,
            vStack = V.take (baseVStackIndex s) (vStack s) }) >> stackPush retV
