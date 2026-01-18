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
    , createSnapshot
    , doSnapshot
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Control.Monad.State.Strict (StateT, get, put)
import System.IO (Handle, stdin, stdout, stderr)

import VM.VMValue (VMValue)
import VM.CallSnapshot (CallSnapshot(..))

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

    , bytecodeIndex :: Int
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

    , env :: V.Vector VMValue
      -- ^ The Current Environment (Closures).
      --   Contains variables captured by the currently executing function.
      --   If the current function is not a closure, this is typically empty.

    , globalEnv :: V.Vector VMValue
      -- ^ The persistent global environment.
      --   Stores global variables accessible throughout the program life.

    , isRunning :: Bool
      -- ^ Execution flag. If False, the VM loop terminates.

    , isDebug :: Bool
      -- ^ Enable tracing

    , vmFds :: Map.Map Int Handle
      -- ^ Table of file descriptors (Kernel)

    , nextFd :: Int
      -- ^ Next free fd
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
createVMState :: BS.ByteString -> Bool -> VMState
createVMState code debug = VMState
    { bytecode = code, bytecodeIndex = 0
    , vStack = V.empty, baseVStackIndex = 0
    , snapshotStack = [], env = V.empty
    , globalEnv = V.replicate 1024 (undefined)
    , isRunning = True, isDebug = debug
    , vmFds = Map.fromList [(0, stdin), (1, stdout), (2, stderr)]
    , nextFd = 3 }

-- | Creates a snapshot of the current VM context (IP, FP, Env).
--
-- @args
--   - vm: The current VM state.
--
-- @details
--   Captures the current Instruction Pointer, Frame Pointer, and Closure Environment.
--   Used by CALL instructions to save the state before jumping.
--
-- @return
--   A 'CallSnapshot' containing the saved context.
--
createSnapshot :: VMState -> CallSnapshot
createSnapshot vm = CallSnapshot
    { callbackIndex = bytecodeIndex vm
    , vStackIndex = baseVStackIndex vm
    , vEnv = env vm }

-- | Sets up a new stack frame for a function call (Shared Logic).
--
-- @args
--   - idx: The absolute address (index) to jump to.
--   - newEnv: The closure environment to load (empty for static calls).
--
-- @details
--   1. Creates and pushes a snapshot of the current state.
--   2. Updates FP (baseVStackIndex) and IP (bytecodeIndex).
--   3. Sets the new environment.
--
-- @return
--   Unit. State is modified.
--
doSnapshot :: Int -> V.Vector VMValue -> VirtualMachine ()
doSnapshot idx nEnv = do
    vm <- get
    put $ vm { snapshotStack = createSnapshot vm : snapshotStack vm
             , baseVStackIndex = V.length (vStack vm)
             , bytecodeIndex = idx
             , env = nEnv }
