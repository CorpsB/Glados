{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Value Data Type
-}

module VM.VMValue
    ( VMValue(..)
    , valueToString
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Common.Type.Integer (IntValue(..), intValueToInt)

-- | Represents a runtime value within the Virtual Machine.
--
-- @details
--   This data structure encompasses all possible types that can exist
--   on the stack or in memory during the execution of the program.
--   It bridges the gap between the static AST and the dynamic runtime.
--
data VMValue
    = VInt IntValue
      -- ^ Represents an integer value (wrapping the Common IntValue type).
      --   Used for all arithmetic operations.

    | VBool Bool
      -- ^ Represents a boolean value (#t or #f).
      --   Used for logic operations and conditional jumps.

    | VList (Vector VMValue)
      -- ^ Represents a dynamic list of values.
      --   Corresponds to the 'AList' node in the AST.

    | VStruct (Vector VMValue)
      -- ^ Represents an instance of a structure.
      --   Fields are stored contiguously. The compiler resolves field names to indices.

    | VClosure Int (Vector VMValue)
      -- ^ Represents a closure (anonymous function with captured context).
      --   @param Int The absolute instruction pointer address of the function code.
      --   @param Vector The captured environment (free variables) from the creation scope.

    | VFuncPtr Int
      -- ^ Represents a raw function pointer without captured variables.
      --   @param Int The absolute instruction pointer address.

    | VVoid
      -- ^ Represents the absence of a value (Unit/Null).
      --   Used for functions that do not return data.
    deriving (Eq, Show)

-- | Converts a runtime Value to its string representation for debugging or printing.
--
-- @args
--   - val: The 'Value' to convert.
--
-- @return
--   A 'String' describing the value (e.g., "#t", "42", "<closure>").
--
valueToString :: VMValue -> String
valueToString (VInt i) = show (intValueToInt i)
valueToString (VBool True) = "#t"
valueToString (VBool False) = "#f"
valueToString (VList v) = "'(" ++ unwords (
  	map valueToString (V.toList v)) ++ ")"
valueToString (VStruct v) = "{struct:" ++ unwords (
  	map valueToString (V.toList v)) ++ "}"
valueToString (VClosure addr caps) = 
    "#<procedure @" ++ show addr ++ " captures:" ++ show (V.length caps) ++ ">"
valueToString (VFuncPtr addr) = "#<function @" ++ show addr ++ ">"
valueToString VVoid = "void"
