{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2Real
-}

module Compiler.ResolveLabels.Step2Real (step2Real) where

import Data.Text (Text)
import Compiler.Instruction (Instruction, instructionSize)

step2Real :: [Instruction] -> Int -> Instruction ->
    Either Text ([Instruction], Int)
step2Real out idx instr = Right (out ++ [instr], idx + instructionSize instr)
