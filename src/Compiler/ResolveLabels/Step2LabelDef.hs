{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2LabelDef
-}

module Compiler.ResolveLabels.Step2LabelDef (step2LabelDef) where

import Data.Text (Text)
import Compiler.Instruction (Instruction)

step2LabelDef :: [Instruction] -> Int -> Either Text ([Instruction], Int)
step2LabelDef out idx = Right (out, idx)
