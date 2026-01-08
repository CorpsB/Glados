{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2JumpIfTrueLabel
-}

module Compiler.ResolveLabels.Step2JumpIfTrueLabel (step2JumpIfTrueLabel, ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Compiler.Instruction (Instruction(..), instructionSize)
import Data.Int (Int32, Int64)
import Compiler.ResolveLabels.ResolveLabelsHelpers (computeOffset, checkInt32Range, sizeOfJumpIfTrueInst)

step2JumpIfTrueLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2JumpIfTrueLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> jumpIfTrueResult target out idx name

jumpIfTrueResult :: Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
jumpIfTrueResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfJumpIfTrueInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "JumpIfTrue offset for label '"
            <> name <> T.pack "' " <> err)
        Right off -> Right (out ++ [JumpIfTrue (fromIntegral off)],
            idx + sizeOfJumpIfTrueInst)
