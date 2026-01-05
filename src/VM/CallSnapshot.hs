{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Call Frame
-}

module VM.CallSnapshot (
    CallSnapshot(..)
) where

import Data.Vector (Vector)
import VM.VMValue (VMValue)

-- | Represents a single frame in the call stack.
--
-- @details
--   A CallSnapshot is pushed onto the call stack whenever a function is called.
--   It acts as a checkpoint, storing the context of the caller so the VM
--   can restore it when the called function returns.
--
data CallSnapshot = CallSnapshot
    { callbackIndex :: Int
      -- ^ The address (ByteString index) where execution should resume
      --   after the current function returns.

    , vStackIndex :: Int
      -- ^ The value of the Frame Pointer before the call.
      --   This is essential to restore the caller's view of its local variables.

    , vEnv :: Vector VMValue
      -- ^ The environment captured by the closure being executed.
      --   Allows the function to access variables defined outside its body
      --   but available at definition time.
    }
    deriving (Show, Eq)
