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

sizeOfJumpIfTrueInst :: Int
sizeOfJumpIfTrueInst = instructionSize (JumpIfTrue 0)

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



step2Real :: [Instruction] -> Int -> Instruction -> Either Text ([Instruction], Int)
step2Real out idx instr = Right (out ++ [instr], idx + instructionSize instr)



step2LabelDef :: [Instruction] -> Int -> Either Text ([Instruction], Int)
step2LabelDef out idx = Right (out, idx)



step2JumpLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2JumpLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> jumpLabelResult target out idx name

jumpLabelResult :: Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
jumpLabelResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfJumpInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "Jump offset for label '" <> name
            <> T.pack "' " <> err)
        Right off -> Right (out ++ [Jump off], idx + sizeOfJumpInst)



step2JumpIfFalseLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2JumpIfFalseLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> jumpIfFalseResult target out idx name

jumpIfFalseResult :: Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
jumpIfFalseResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfJumpIfFalseInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "JumpIfFalse offset for label '"
            <> name <> T.pack "' " <> err)
        Right off -> Right (out ++ [JumpIfFalse off],
            idx + sizeOfJumpIfFalseInst)



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
        Right off -> Right (out ++ [JumpIfTrue off],
            idx + sizeOfJumpIfTrueInst)



step2CallLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2CallLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> callLabelResult target out idx name

callLabelResult :: Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
callLabelResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfCallInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "Call offset for label '" <> name
            <> T.pack "' " <> err)
        Right off -> Right (out ++ [Call off], idx + sizeOfCallInst)



step2TailCallLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2TailCallLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> tailCallLabelResult target out idx name

tailCallLabelResult :: Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
tailCallLabelResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfTailCallInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "TailCall offset for label '" <> name
            <> T.pack "' " <> err)
        Right off -> Right (out ++ [TailCall off],
            idx + sizeOfTailCallInst)



step2MakeClosureLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Int -> Either Text ([Instruction], Int)
step2MakeClosureLabel labelMap out idx name n =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> makeClosureResult target out idx name n

makeClosureResult :: Int -> [Instruction] -> Int -> Text -> Int -> Either Text ([Instruction], Int)
makeClosureResult target out idx name n =
    let mcSize = instructionSize (MakeClosure 0 0)
        off64 = computeOffset target (idx + mcSize)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "MakeClosure offset for label '"
            <> name <> T.pack "' " <> err)
        Right off -> Right (out ++ [MakeClosure off n], idx + mcSize)



step2GetFuncAddrLabel :: Map.Map Text Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2GetFuncAddrLabel labelMap out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> getFuncAddrResult target out idx name

getFuncAddrResult :: Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
getFuncAddrResult target out idx name =
    let off64 = computeOffset target (idx + instructionSize (GetFuncAddr 0))
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "GetFuncAddr offset for label '"
            <> name <> T.pack "' " <> err)
        Right off -> Right (out ++ [GetFuncAddr off],
            idx + instructionSize (GetFuncAddr 0))



computeOffset :: Int -> Int -> Int64
computeOffset target idx = fromIntegral target - fromIntegral idx

checkInt32Range :: Int64 -> Either Text Int32
checkInt32Range off =
    if off < fromIntegral (minBound :: Int32) || off > fromIntegral
        (maxBound :: Int32)
        then Left (T.pack "Offset out of range")
        else Right (fromIntegral off)