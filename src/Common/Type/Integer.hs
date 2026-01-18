{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Integer
-}

module Common.Type.Integer
    ( IntValue(..)
    , intValueToInt
    , fitInteger
    , toInt64
    , fromInt64
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

data IntValue
    = I8 Int8
    | UI8 Word8
    | I16 Int16
    | UI16 Word16
    | I32 Int32
    | UI32 Word32
    | I64 Int64
    | UI64 Word64
    | IChar Int8
    | UIChar Word8
    deriving (Show, Eq, Ord)

-- | Converts an IntValue to a standard Haskell Int.
--
-- @args
--   - v: The IntValue to convert.
--
-- @details
--   This function handles all IntValue constructors, converting them
--   to the system's native 'Int' type. Note that converting large
--   64-bit values on a 32-bit system might cause overflow, though
--   this is rare in modern environments.
--
-- @return
--   The integer value as an 'Int'.
intValueToInt :: IntValue -> Int
intValueToInt (I8 i) = fromIntegral i
intValueToInt (UI8 i) = fromIntegral i
intValueToInt (I16 i) = fromIntegral i
intValueToInt (UI16 i) = fromIntegral i
intValueToInt (I32 i) = fromIntegral i
intValueToInt (UI32 i) = fromIntegral i
intValueToInt (I64 i) = fromIntegral i
intValueToInt (UI64 i) = fromIntegral i
intValueToInt (IChar i) = fromIntegral i
intValueToInt (UIChar i) = fromIntegral i

-- | Converts an IntValue to a standard Haskell Int64.
--
-- @args
--   - v: The IntValue to convert.
--
-- @details
--   Provides a safe conversion to 'Int64' for all underlying types.
--   Unsigned 64-bit values are treated as signed (wrapping) if they
--   exceed 'maxBound :: Int64'.
--
-- @return
--   The integer value as an 'Int64'.
toInt64 :: IntValue -> Int64
toInt64 (I8 i) = fromIntegral i
toInt64 (UI8 i) = fromIntegral i
toInt64 (I16 i) = fromIntegral i
toInt64 (UI16 i) = fromIntegral i
toInt64 (I32 i) = fromIntegral i
toInt64 (UI32 i) = fromIntegral i
toInt64 (I64 i) = i
toInt64 (UI64 i) = fromIntegral i
toInt64 (IChar i) = fromIntegral i
toInt64 (UIChar i) = fromIntegral i

-- | Converts an Int64 to the smallest suitable IntValue representation.
--
-- @args
--   - n: The Int64 value to compress.
--
-- @details
--   This function checks the bounds of the integer to determine the
--   most compact storage format (I8, UI8, I16, etc.). It prioritizes
--   signed representation for small positive numbers but will use
--   unsigned types if it saves space (e.g., 200 fits in UI8 but requires I16).
--
-- @return
--   An 'IntValue' wrapping the value in the smallest possible container.
fromInt64 :: Int64 -> IntValue
fromInt64 n | n >= fromIntegral (minBound :: Int8) &&
        n <= fromIntegral (maxBound :: Int8) = I8 (fromIntegral n)
    | n >= 0 && n <= fromIntegral (maxBound :: Word8) = UI8 (fromIntegral n)
    | n >= fromIntegral (minBound :: Int16) &&
        n <= fromIntegral (maxBound :: Int16) = I16 (fromIntegral n)
    | n >= 0 && n <= fromIntegral (maxBound :: Word16) = UI16 (fromIntegral n)
    | n >= fromIntegral (minBound :: Int32) &&
        n <= fromIntegral (maxBound :: Int32) = I32 (fromIntegral n)
    | n >= 0 && n <= fromIntegral (maxBound :: Word32) = UI32 (fromIntegral n)
    | otherwise = I64 n

-- | Converts an arbitrary Integer to the smallest suitable IntValue.
--
-- @args
--   - n: The Integer to compress.
--
-- @details
--   Similar to 'fromInt64' but works on arbitrary precision 'Integer's.
--   It attempts to fit the value into I8/UI8, then I16/UI16, etc.
--   If the value exceeds the signed 64-bit range but fits in unsigned 64-bit,
--   it uses UI64.
--
-- @return
--   The optimized 'IntValue'.
fitInteger :: Integer -> IntValue
fitInteger n | n >= fromIntegral (minBound :: Int8) &&
        n <= fromIntegral (maxBound :: Int8) = I8 (fromInteger n)
    | n >= 0 && n <= fromIntegral (maxBound :: Word8) = UI8 (fromInteger n)
    | n >= fromIntegral (minBound :: Int16) &&
        n <= fromIntegral (maxBound :: Int16) = I16 (fromInteger n)
    | n >= 0 && n <= fromIntegral (maxBound :: Word16) = UI16 (fromInteger n)
    | n >= fromIntegral (minBound :: Int32) &&
        n <= fromIntegral (maxBound :: Int32) = I32 (fromInteger n)
    | n >= 0 && n <= fromIntegral (maxBound :: Word32) = UI32 (fromInteger n)
    | otherwise = I64 (fromInteger n)
