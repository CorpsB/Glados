{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM State
-}

module VM.VMState
    ( VMState(..)
    , VirtualMachine
    , createVMState
    ) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Control.Monad.State.Strict (StateT)

import VM.VMValue (VMValue)
import VM.CallSnapshot (CallSnapshot)

-- | The core state of the Virtual Machine.
--
-- @details
--   This structure holds the entire context of the executing program at any
--   given microsecond. It includes memory (stack, heap/globals), the processor
--   registers (IP, FP), and the code itself.
--
data VMState = VMState
    { bytecode :: BS.ByteString
      -- ^ The read-only byte array containing the compiled machine code.

    , programIndex :: Int
      -- ^ The current index in 'bytecode'.
      --   When a function is called, this value (plus offset) becomes the 
      --   'callbackIndex' in the pushed CallSnapshot.

    , vStack :: V.Vector VMValue
      -- ^ The main data stack.
      --   Contains inputs, outputs, and local variables.

    , baseVStackIndex :: Int
      -- ^ The index in 'vStack' where the current function's local variables begin.
      --   When a function is called, this value is saved as 'vStackIndex' 
      --   in the CallSnapshot.

    , snapshotStack :: [CallSnapshot]
      -- ^ The stack of saved contexts (formerly callStack).
      --   Used to restore the state upon returning from a function.

    , globalEnv :: V.Vector VMValue
      -- ^ The persistent global environment.
      --   Stores global variables accessible throughout the program life.

    , isRunning :: Bool
      -- ^ Execution flag. If False, the VM loop terminates.
    }

-- | The Monad Transformer stack used for the VM.
--
-- @details
--   We use 'StateT' to carry the 'VMState' and 'IO' to allow for
--   side effects like printing to the console (PRINT instruction).
type VirtualMachine a = StateT VMState IO a

-- | Creates the initial state of the VM for a given program.
--
-- @args
--   - code: The compiled binary code to execute.
--
-- @return
--   A pristine 'VMState' ready for execution.
--
createVMState :: BS.ByteString -> VMState
createVMState code = VMState
    { bytecode        = code
    , programIndex    = 0
    , valueStack      = V.empty
    , baseVStackIndex = 0
    , snapshotStack   = []
    , globalEnv       = V.replicate 1024 (undefined)
    , isRunning       = True
    }
