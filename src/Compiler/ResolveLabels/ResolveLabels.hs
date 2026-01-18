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
import Compiler.ResolveLabels.ResolveLabelsHelpers
    ( sizeOfJumpInst
    , sizeOfJumpIfFalseInst
    , sizeOfJumpIfTrueInst
    , sizeOfCallInst
    , sizeOfTailCallInst
    )
import Compiler.PsInstruction (PsInstruction(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Control.Monad (foldM)
import qualified Data.Text as T

resolveLabels :: [PsInstruction] -> Either Text [Instruction]
resolveLabels pseudos =
    foldM step1 (0, Map.empty, Set.empty) pseudos >>=
        \(_, labelMap, startsSet) ->
        fmap fst $ foldM (step2 labelMap startsSet) ([], 0) pseudos

step1 :: (Int, Map.Map Text Int, Set.Set Int) -> PsInstruction
    -> Either Text (Int, Map.Map Text Int, Set.Set Int)
step1 (idx, m, s) (LabelDef name) = case detectDuplicateLabels idx name m of
    Left err -> Left err
    Right (_, newM) -> Right (idx, newM, s)
step1 (idx, m, s) pseudo = Right (idx + pseudoSize pseudo, m, Set.insert idx s)

pseudoSize :: PsInstruction -> Int
pseudoSize (Real instr) = instructionSize instr
pseudoSize (JumpLabel _) = sizeOfJumpInst
pseudoSize (JumpIfFalseLabel _) = sizeOfJumpIfFalseInst
pseudoSize (JumpIfTrueLabel _) = sizeOfJumpIfTrueInst
pseudoSize (CallLabel _) = sizeOfCallInst
pseudoSize (TailCallLabel _) = sizeOfTailCallInst
pseudoSize (MakeClosureLabel _ _) = instructionSize (MakeClosure 0 0)
pseudoSize (GetFuncAddrLabel _) = instructionSize (GetFuncAddr 0)
pseudoSize (LabelDef _) = 0

detectDuplicateLabels :: Int -> Text -> Map.Map Text Int
    -> Either Text (Int, Map.Map Text Int)
detectDuplicateLabels idx name m =
    case Map.member name m of
        True  -> Left (T.pack $ "Duplicate label: " ++ T.unpack name)
        False -> Right (idx, Map.insert name idx m)

step2 :: Map.Map Text Int -> Set.Set Int -> ([Instruction], Int)
    -> PsInstruction -> Either Text ([Instruction], Int)
step2 _ _ (out, idx) (Real instr) = step2Real out idx instr
step2 _ _ (out, idx) (LabelDef _) = step2LabelDef out idx
step2 lm ss (out, idx) pseudo = step2Pseudo lm ss out idx pseudo

step2Pseudo :: Map.Map Text Int -> Set.Set Int -> [Instruction]
    -> Int -> PsInstruction -> Either Text ([Instruction], Int)
step2Pseudo lm ss out idx (JumpLabel name) = step2JumpLabel lm ss out idx name
step2Pseudo lm ss out idx (JumpIfFalseLabel name) =
    step2JumpIfFalseLabel lm ss out idx name
step2Pseudo lm ss out idx (JumpIfTrueLabel name) =
    step2JumpIfTrueLabel lm ss out idx name
step2Pseudo lm ss out idx other = step2PseudoRest lm ss out idx other

step2PseudoRest :: Map.Map Text Int -> Set.Set Int -> [Instruction]
    -> Int -> PsInstruction -> Either Text ([Instruction], Int)
step2PseudoRest lm ss out idx (CallLabel name) =
    step2CallLabel lm ss out idx name
step2PseudoRest lm ss out idx (TailCallLabel name) =
    step2TailCallLabel lm ss out idx name
step2PseudoRest lm ss out idx (MakeClosureLabel name n) =
    step2MakeClosureLabel lm ss out idx name n
step2PseudoRest lm ss out idx (GetFuncAddrLabel name) =
    step2GetFuncAddrLabel lm ss out idx name
step2PseudoRest _ _ out idx _ = Right (out, idx)
