{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- System Instructions
-}

{-|
Module      : VM.Instruction.System
Description : System calls and VM control flow.
Stability   : stable
-}
module VM.Instruction.System
    ( instPrint
    , instHalt
    ) where

import Control.Monad.State.Strict (liftIO, modify)
import VM.VMState (VirtualMachine, VMState(..))
import VM.VMValue (valueToString)
import VM.VMStack (stackPop)

-- | Implements PRINT (Opcode 0x70).
--
-- @details
--   Pops the top value from the stack and prints its string representation
--   to the standard output (stdout), followed by a newline.
--   Useful for debugging or simple output programs.
--
instPrint :: VirtualMachine ()
instPrint = do
    val <- stackPop
    liftIO $ putStrLn (valueToString val)

-- | Implements HALT (Opcode 0x71).
--
-- @details
--   Sets the 'isRunning' flag of the VM state to False.
--   This will cause the main Execution loop to terminate gracefully
--   on the next cycle.
--
instHalt :: VirtualMachine ()
instHalt = modify $ \s -> s { isRunning = False }
