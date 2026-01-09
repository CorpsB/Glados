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
import qualified Data.Text as T

resolveLabels :: [PsInstruction] -> Either Text [Instruction]
resolveLabels pseudos =
    foldM step1 (0, Map.empty) pseudos >>= \(_, labelMap) ->
    fmap fst $ foldM (step2 labelMap) ([], 0) pseudos

step1 :: (Int, Map.Map Text Int) -> PsInstruction -> Either Text (Int, Map.Map Text Int)
step1 (idx, m) pseudo = case pseudo of
    LabelDef name -> detectDuplicateLabels idx name m
    Real instr    -> Right (idx + instructionSize instr, m)
    JumpLabel _   -> Right (idx + sizeOfJumpInst, m)
    JumpIfFalseLabel _ -> Right (idx + sizeOfJumpIfFalseInst, m)
    JumpIfTrueLabel _  -> Right (idx + sizeOfJumpIfFalseInst, m)
    CallLabel _   -> Right (idx + sizeOfCallInst, m)
    TailCallLabel _ -> Right (idx + sizeOfTailCallInst, m)
    MakeClosureLabel _ _ -> Right (idx + instructionSize (MakeClosure 0 0), m)
    GetFuncAddrLabel _ -> Right (idx + instructionSize (GetFuncAddr 0), m)

detectDuplicateLabels :: Int -> Text -> Map.Map Text Int -> Either Text (Int, Map.Map Text Int)
detectDuplicateLabels idx name m =
    if Map.member name m
        then Left (T.pack $ "Duplicate label: " ++ T.unpack name)
        else Right (idx, Map.insert name idx m)

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
