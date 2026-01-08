{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- ResolveLabels
-}

module Compiler.ResolveLabels.ResolveLabels (resolveLabels) where

import Compiler.ResolveLabels.Step2CallLabel (step2CallLabel)
import Compiler.ResolveLabels.Step2GetFuncAddrLabel (step2GetFuncAddrLabel)
import Compiler.ResolveLabels.Step2JumpIfFalseLabel (step2JumpIfFalseLabel)
import Compiler.ResolveLabels.Step2JumpIfTrueLabel (step2JumpIfTrueLabel)
import Compiler.ResolveLabels.Step2JumpLabel (step2JumpLabel)
import Compiler.ResolveLabels.Step2LabelDef (step2LabelDef)
import Compiler.ResolveLabels.Step2MakeClosureLabel (step2MakeClosureLabel)
import Compiler.ResolveLabels.Step2Real (step2Real)
import Compiler.ResolveLabels.Step2TailCallLabel (step2TailCallLabel)
import Compiler.Instruction (instructionSize, Instruction(..))
import Compiler.ResolveLabels.ResolveLabelsHelpers (sizeOfJumpInst, sizeOfJumpIfFalseInst, sizeOfJumpIfTrueInst, sizeOfCallInst, sizeOfTailCallInst)
import Compiler.PsInstruction (PsInstruction(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Control.Monad (foldM)

resolveLabels :: [PsInstruction] -> Either Text [Instruction]
resolveLabels pseudos =
    -- Pass 1: build label map
    let (_, labelMap) = foldl' step1 (0, Map.empty) pseudos
    -- Pass 2: resolve pseudo instructions to real instructions
    in fmap fst $ foldM (step2 labelMap) ([], 0) pseudos

step1 :: (Int, Map.Map Text Int) -> PsInstruction -> (Int, Map.Map Text Int)
step1 (idx, m) pseudo = case pseudo of
    LabelDef name -> (idx, Map.insert name idx m)
    Real instr    -> (idx + instructionSize instr, m)
    JumpLabel _   -> (idx + sizeOfJumpInst, m)
    JumpIfFalseLabel _ -> (idx + sizeOfJumpIfFalseInst, m)
    JumpIfTrueLabel _  -> (idx + sizeOfJumpIfFalseInst, m)
    CallLabel _   -> (idx + sizeOfCallInst, m)
    TailCallLabel _ -> (idx + sizeOfTailCallInst, m)
    MakeClosureLabel _ _ -> (idx + instructionSize (MakeClosure 0 0), m)
    GetFuncAddrLabel _ -> (idx + instructionSize (GetFuncAddr 0), m)

step2 :: Map.Map Text Int -> ([Instruction], Int) -> PsInstruction -> Either Text ([Instruction], Int)
step2 labelMap (out, idx) pseudo = case pseudo of
    Real instr -> step2Real out idx instr
    LabelDef _ -> step2LabelDef out idx
    JumpLabel name -> step2JumpLabel labelMap out idx name
    JumpIfFalseLabel name -> step2JumpIfFalseLabel labelMap out idx name
    JumpIfTrueLabel name -> step2JumpIfTrueLabel labelMap out idx name
    CallLabel name -> step2CallLabel labelMap out idx name
    TailCallLabel name -> step2TailCallLabel labelMap out idx name
    MakeClosureLabel name n -> step2MakeClosureLabel labelMap out idx name n
    GetFuncAddrLabel name -> step2GetFuncAddrLabel labelMap out idx name
