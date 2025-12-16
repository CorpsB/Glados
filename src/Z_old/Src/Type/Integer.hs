{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Integer
-}

module Z_old.Src.Type.Integer (IntValue(..), fitInteger, toInt64,
    fromInt64, intValueEq, intValueToInt) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

data IntValue
    = I8 Int8
    | I16 Int16
    | I32 Int32
    | I64 Int64
    | U8 Word8
    | U16 Word16
    | U32 Word32
    | U64 Word64
    | IChar Char
    deriving (Show, Eq, Ord)

toInt64 :: IntValue -> Int64
toInt64 (I8 n)  = fromIntegral n
toInt64 (I16 n) = fromIntegral n
toInt64 (I32 n) = fromIntegral n
toInt64 (I64 n) = n
toInt64 (U8 n)  = fromIntegral n
toInt64 (U16 n) = fromIntegral n
toInt64 (U32 n) = fromIntegral n
toInt64 (U64 n) = fromIntegral n
toInt64 (IChar c) = fromIntegral (fromEnum c)

fromInt64 :: Int64 -> IntValue
fromInt64 n
    | n >= fromIntegral (minBound :: Int8)
        && n <= fromIntegral (maxBound :: Int8)  = I8  (fromIntegral n)
    | n >= fromIntegral (minBound :: Int16)
        && n <= fromIntegral (maxBound :: Int16) = I16 (fromIntegral n)
    | n >= fromIntegral (minBound :: Int32)
        && n <= fromIntegral (maxBound :: Int32) = I32 (fromIntegral n)
    | otherwise = I64 n

fitInteger :: Int -> IntValue
fitInteger n = fromInt64 (fromIntegral n)

intValueToInt :: IntValue -> Int
intValueToInt (I8 n)  = fromIntegral n
intValueToInt (I16 n) = fromIntegral n
intValueToInt (I32 n) = fromIntegral n
intValueToInt (I64 n) = fromIntegral n
intValueToInt (U8 n)  = fromIntegral n
intValueToInt (U16 n) = fromIntegral n
intValueToInt (U32 n) = fromIntegral n
intValueToInt (U64 n) = fromIntegral n
intValueToInt (IChar c) = fromEnum c

intValueEq :: IntValue -> Int64 -> Bool
intValueEq v ref = (toInt64 v) == ref
