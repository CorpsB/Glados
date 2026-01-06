{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- ResolveLabels
-}

module Compiler.ResolveLabels (resolveLabels, ) where

import Compiler.Instruction (instructionSize, Instruction(..))
import Compiler.PsInstruction (PsInstruction(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Control.Monad (foldM)
import qualified Data.Text as T

sizeOfJumpInst :: Int
sizeOfJumpInst = instructionSize (Jump 0)
sizeOfJumpIfFalseInst :: Int
sizeOfJumpIfFalseInst = instructionSize (JumpIfFalse 0)
sizeOfCallInst :: Int
sizeOfCallInst = instructionSize (Call 0)
sizeOfTailCallInst :: Int
sizeOfTailCallInst = instructionSize (Call 0)

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
    CallLabel _   -> (idx + sizeOfCallInst, m)
    TailCallLabel _ -> (idx + sizeOfTailCallInst, m)

step2 :: Map.Map Text Int -> ([Instruction], Int) -> PsInstruction -> Either Text ([Instruction], Int)
step2 labelMap (out, idx) pseudo = case pseudo of
    Real instr -> step2Real out idx instr
    LabelDef _ -> step2LabelDef out idx
    JumpLabel name -> step2JumpLabel labelMap out idx name
    JumpIfFalseLabel name -> step2JumpIfFalseLabel labelMap out idx name
    CallLabel name -> step2CallLabel labelMap out idx name
    TailCallLabel name -> step2TailCallLabel labelMap out idx name

-- Helper functions for step2 cases
step2Real :: [Instruction] -> Int -> Instruction -> Either Text ([Instruction], Int)
step2Real out idx instr = Right (out ++ [instr], idx + instructionSize instr)

step2LabelDef :: [Instruction] -> Int -> Either Text ([Instruction], Int)
step2LabelDef out idx = Right (out, idx)

step2JumpLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2JumpLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target ->
            let off = (target - (idx + sizeOfJumpInst))
            in Right (out ++ [Jump off],
                    idx + sizeOfJumpInst)

step2JumpIfFalseLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2JumpIfFalseLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target ->
            let off = (target - (idx + sizeOfJumpIfFalseInst))
            in Right (out ++ [JumpIfFalse off],
                    idx + sizeOfJumpIfFalseInst)

step2CallLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2CallLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target ->
            let off = (target - (idx + sizeOfCallInst))
            in Right (out ++ [Call off],
                    idx + sizeOfCallInst)

step2TailCallLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2TailCallLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target ->
            let off = (target - (idx + sizeOfTailCallInst))
            in Right (out ++ [TailCall off],
                    idx + sizeOfTailCallInst)