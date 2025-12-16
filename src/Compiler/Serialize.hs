{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- Serialize
-}

module Compiler.Serialize (
    Instruction(..),
    serializeInstruction,
    instructionSize,
    buildGLA
) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32)
import Data.Monoid ((<>))

data Instruction
    = InstructionRaw B.Builder
    | InstructionNOP
    deriving (Eq, Show)

serializeInstruction :: Instruction -> B.Builder
serializeInstruction (InstructionRaw b) = b
serializeInstruction InstructionNOP = mempty

instructionSize :: Instruction -> Int
instructionSize (InstructionRaw b)
= fromIntegral . LBS.length $ B.toLazyByteString b
instructionSize InstructionNOP = 0

buildGLA :: [Instruction] -> B.Builder
buildGLA = mconcat . map serializeInstruction
