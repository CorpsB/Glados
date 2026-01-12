{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2JumpIfTrueLabel
-}

module Compiler.ResolveLabels.Step2JumpIfTrueLabel (step2JumpIfTrueLabel) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Compiler.Instruction (Instruction(..))
import Compiler.ResolveLabels.ResolveLabelsHelpers
    ( computeOffset
    , checkInt32Range
    , sizeOfJumpIfTrueInst)

step2JumpIfTrueLabel :: Map.Map Text Int -> Set.Set Int ->
    [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2JumpIfTrueLabel labelMap starts out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target ->
            case Set.member target starts of
                True -> jumpIfTrueResult target out idx name
                False -> Left (T.pack
                    "JumpIfTrue target not at instruction boundary")

jumpIfTrueResult :: Int -> [Instruction] -> Int ->
    Text -> Either Text ([Instruction], Int)
jumpIfTrueResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfJumpIfTrueInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "JumpIfTrue offset for label '"
            <> name <> T.pack "' " <> err)
        Right off -> Right (out ++ [JumpIfTrue (fromIntegral off)],
            idx + sizeOfJumpIfTrueInst)
