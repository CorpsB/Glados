{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Integer
-}

module Type.Integer (IntValue(..), fitInteger, toInt64,
    fromInt64, intValueEq, intValueToInt) where

import Data.Int (Int8, Int16, Int32, Int64)

data IntValue
    = I8 Int8
    | I16 Int16
    | I32 Int32
    | I64 Int64
    deriving (Show, Eq, Ord)

toInt64 :: IntValue -> Int64
toInt64 (I8 n)  = fromIntegral n
toInt64 (I16 n) = fromIntegral n
toInt64 (I32 n) = fromIntegral n
toInt64 (I64 n) = n

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

intValueEq :: IntValue -> Int64 -> Bool
intValueEq v ref = (toInt64 v) == ref
