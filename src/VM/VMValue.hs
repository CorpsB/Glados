{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Value Data Type
-}

module VM.VMValue
    ( VMValue(..)
    , castValue
    , valueToString
    , valueToInt
    ) where

import Data.Word (Word8)
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
valueToString (VBool True) = "True"
valueToString (VBool False) = "False"
valueToString (VList v) = "'(" ++ unwords (
    map valueToString (V.toList v)) ++ ")"
valueToString (VStruct v) = "{struct:" ++ unwords (
    map valueToString (V.toList v)) ++ "}"
valueToString (VClosure addr caps) =
    "#<procedure @" ++ show addr ++ " captures:" ++ show (V.length caps) ++ ">"
valueToString (VFuncPtr addr) = "#<function @" ++ show addr ++ ">"
valueToString VVoid = "void"

-- | Helper to extract a raw integer value for casting purposes.
--
-- @details
--   - Integers: returns the raw value.
--   - Booleans: True -> 1, False -> 0.
--   - Pointers: returns the address.
--   - Others: returns 0 (safe default).
valueToInt :: VMValue -> Int
valueToInt (VInt i) = intValueToInt i
valueToInt (VBool True) = 1
valueToInt (VBool False) = 0
valueToInt (VFuncPtr addr) = addr
valueToInt _ = 0

-- | Converts a VMValue to a specific target TypeID.
--
-- @args
--   - typeId: The target type identifier (see ASM_SPEC).
--   - val: The value to convert.
--
-- @details
--   Performs a native conversion (truncation or extension) via 'fromIntegral'.
--   Supports converting between Booleans, Integers (signed/unsigned) and Chars.
--
castValue :: Word8 -> VMValue -> VMValue
castValue 0x00 v = VBool (valueToInt v /= 0)
castValue 0x01 v = VInt (I8 (fromIntegral (valueToInt v)))
castValue 0x02 v = VInt (UI8 (fromIntegral (valueToInt v)))
castValue 0x03 v = VInt (I16 (fromIntegral (valueToInt v)))
castValue 0x04 v = VInt (UI16 (fromIntegral (valueToInt v)))
castValue 0x05 v = VInt (I32 (fromIntegral (valueToInt v)))
castValue 0x06 v = VInt (UI32 (fromIntegral (valueToInt v)))
castValue 0x07 v = VInt (I64 (fromIntegral (valueToInt v)))
castValue 0x08 v = VInt (UI64 (fromIntegral (valueToInt v)))
castValue 0x09 v = VInt (IChar (fromIntegral (valueToInt v)))
castValue 0x10 v = VInt (UIChar (fromIntegral (valueToInt v)))
castValue _ v = v
