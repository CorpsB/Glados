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
    , instExit
    , instCheckStack
    ) where

import System.Exit (exitWith, ExitCode(..))
import Control.Monad.State.Strict (liftIO, modify, get)
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO

import VM.VMState (VirtualMachine, VMState(..))
import VM.VMValue (valueToString, valueToInt)
import VM.VMStack (stackPop)
import VM.Bytecode.Reader (readInt32)

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
    liftIO $ TIO.putStr (valueToString val)

-- | Implements HALT (Opcode 0x71).
--
-- @details
--   Sets the 'isRunning' flag of the VM state to False.
--   This will cause the main Execution loop to terminate gracefully
--   on the next cycle.
--
instHalt :: VirtualMachine ()
instHalt = modify $ \vm -> vm { isRunning = False }

-- | Implements CHECK_STACK (Opcode 0xFE).
--
-- @details
--   Ensures the stack has at least 'N' elements.
--   Reads 'N' as an Int32. Throws error if stack is too small.
--
instCheckStack :: VirtualMachine ()
instCheckStack = do
    required <- readInt32
    vm <- get
    case V.length (vStack vm) >= required of
        False -> error $ "VM Error: Stack Check Failed (Required: " ++
            show required ++ ", Actual: " ++ show (V.length (vStack vm)) ++ ")"
        True -> return ()

-- | Implements EXIT (Opcode 0x72).
--
-- @details
--   Pops an integer status code from the stack and terminates the process.
--   Useful for unit testing (exit 84 on failure).
--
instExit :: VirtualMachine ()
instExit = do
    v <- stackPop
    let code = valueToInt v
    case code of
        0 -> liftIO $ exitWith ExitSuccess
        _ -> liftIO $ exitWith (ExitFailure code)
