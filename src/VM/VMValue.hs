{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- VM Value Data Type
-}

module VM.VMValue
    ( VMValue(..)
    , getValueName
    , castValue
    , valueToString
    , valueToInt
    , eqValue
    , stringToValue
    , charToValue
    ) where

import Data.Word (Word8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (chr, ord)

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

-- | Helper: Extracts a Char from a VMValue if and only if it is an
--           integer type representing a character.
--
-- @param val The value to check.
-- @return Just Char if the value is IChar or UIChar, Nothing otherwise.
--
getCharFromValue :: VMValue -> Maybe Char
getCharFromValue (VInt (IChar c))  = Just (chr (fromIntegral c))
getCharFromValue (VInt (UIChar c)) = Just (chr (fromIntegral c))
getCharFromValue _ = Nothing

-- | Helper: Attempts to convert a list of VMValues into a Text string.
--
-- @param vals The list of values to inspect.
-- @return Just Text if all elements are characters, Nothing otherwise.
--
tryExtractString :: [VMValue] -> Maybe Text
tryExtractString [] = Nothing
tryExtractString vals = fmap T.pack maybeChars
    where maybeChars = mapM getCharFromValue vals

-- | Converts a VMValue into a human-readable Text representation.
--
-- @param val The value to convert.
-- @return A Text representing the value.
--
-- @details
--   This function handles type-specific formatting:
--   - **Chars**: Converted to single-character strings.
--   - **Lists**: If the list contains only Characters, it is rendered as a raw string
--     (e.g., "Hello"). Otherwise, it uses list notation (e.g., "[1, 2, #t]").
--   - **Structs**: Rendered with curly braces (e.g., "{10, 20}").
--   - **Bools**: Rendered as #t or #f (Scheme style).
--
valueToString :: VMValue -> Text
valueToString VVoid = T.pack "void"
valueToString (VInt (IChar c)) = T.singleton (chr $ fromIntegral c)
valueToString (VInt (UIChar c)) = T.singleton (chr $ fromIntegral c)
valueToString (VInt i) = T.pack (show (intValueToInt i))
valueToString (VBool True) = T.pack "True"
valueToString (VBool False) = T.pack "False"
valueToString (VClosure addr caps) = T.pack "#<procedure @" <>
    T.pack (show addr) <> T.pack " captures:" <>
    T.pack (show (V.length caps)) <> T.pack ">"
valueToString (VFuncPtr addr) = T.pack "#<function @" <>
    T.pack (show addr) <> T.pack ">"
valueToString (VStruct v) = T.pack "{" <>
    T.intercalate (T.pack ", ") (map valueToString (V.toList v)) <> T.pack "}"
valueToString (VList v) = let elements = V.toList v in
    case tryExtractString elements of
        Just txt -> txt
        Nothing -> T.pack "[" <> (T.intercalate (T.pack ", ")
            (map valueToString elements)) <> T.pack "]"

-- | Converts a single Character to a VMValue.
--
-- @arg c: The character to convert.
--
-- @details
--   Wraps the character's ordinal value into an IChar (Int8).
--   Used as a helper for string conversion.
--
charToValue :: Char -> VMValue
charToValue c = VInt (IChar (fromIntegral (ord c)))

-- | Converts a Text string into a VMValue.
--
-- @arg txt: The text string to convert.
--
-- @details
--   Transforms the Text into a VList of VInt (IChar).
--   This reverses the logic of 'valueToString' for strings, creating
--   the standard list-based string representation used by the VM.
--
stringToValue :: Text -> VMValue
stringToValue txt = VList (V.fromList (map charToValue (T.unpack txt)))

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

-- | Helpers to get string representation of type.
--
-- @arg val: The VMValue to inspect.
-- @return The string name of the type.
--
getValueName :: VMValue -> T.Text
getValueName (VInt _) = T.pack "int"
getValueName (VBool _) = T.pack "bool"
getValueName (VList _) = T.pack "list"
getValueName (VStruct _) = T.pack "struct"
getValueName (VClosure _ _) = T.pack "function"
getValueName (VFuncPtr _) = T.pack "function"
getValueName VVoid = T.pack "void"

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

-- | Helper to compare two vectors using the custom equality logic.
checkVectorEq :: V.Vector VMValue -> V.Vector VMValue -> Bool
checkVectorEq v1 v2 =
    V.length v1 == V.length v2 && V.and (V.zipWith eqValue v1 v2)

-- | Recursive equality check for VMValues.
--
-- @details
--   Handles loose integer comparison inside Lists and Structs.
--   e.g., [I8 5] == [I64 5] returns True.
--
eqValue :: VMValue -> VMValue -> Bool
eqValue (VInt a) (VInt b) = intValueToInt a == intValueToInt b
eqValue (VBool a) (VBool b) = a == b
eqValue (VFuncPtr a) (VFuncPtr b) = a == b
eqValue VVoid VVoid = True
eqValue (VList a) (VList b) = checkVectorEq a b
eqValue (VStruct a) (VStruct b) = checkVectorEq a b
eqValue _ _ = False
