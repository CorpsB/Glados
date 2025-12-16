{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Integer
-}

module Common.Type.Integer (IntValue(..)) where

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
    deriving (Show, Eq, Ord)
