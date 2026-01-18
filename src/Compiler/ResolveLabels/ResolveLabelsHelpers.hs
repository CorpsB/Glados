{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- ComputeOffset
-}

module Compiler.ResolveLabels.ResolveLabelsHelpers (
    checkInt32Range,
    computeOffset,
    sizeOfJumpInst,
    sizeOfJumpIfFalseInst,
    sizeOfJumpIfTrueInst,
    sizeOfCallInst,
    sizeOfTailCallInst,
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64, Int32)

import Compiler.Instruction (Instruction(..), instructionSize)

sizeOfJumpInst :: Int
sizeOfJumpInst = instructionSize (Jump 0)
sizeOfJumpIfFalseInst :: Int
sizeOfJumpIfFalseInst = instructionSize (JumpIfFalse 0)
sizeOfJumpIfTrueInst :: Int
sizeOfJumpIfTrueInst = instructionSize (JumpIfTrue 0)
sizeOfCallInst :: Int
sizeOfCallInst = instructionSize (Call 0)
sizeOfTailCallInst :: Int
sizeOfTailCallInst = instructionSize (Call 0)

computeOffset :: Int -> Int -> Int64
computeOffset target idx = fromIntegral target - fromIntegral idx

checkInt32Range :: Int64 -> Either Text Int32
checkInt32Range off =
    case off < fromIntegral (minBound :: Int32)
        || off > fromIntegral (maxBound :: Int32) of
        True  -> Left (T.pack "Offset out of range")
        False -> Right (fromIntegral off)
