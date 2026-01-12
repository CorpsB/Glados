{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- step2JumpLabel
-}

module Compiler.ResolveLabels.Step2JumpLabel (step2JumpLabel) where

import Compiler.Instruction (Instruction(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Compiler.ResolveLabels.ResolveLabelsHelpers
    ( computeOffset
    , checkInt32Range
    , sizeOfJumpInst
    )

step2JumpLabel :: Map.Map Text Int -> Set.Set Int -> [Instruction] ->
    Int -> Text -> Either Text ([Instruction], Int)
step2JumpLabel labelMap startsSet out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target ->
            case Set.member target startsSet of
                True -> jumpLabelResult target out idx name
                False -> Left (T.pack
                    "Jump target not at instruction boundary")

jumpLabelResult :: Int -> [Instruction] -> Int ->
    Text -> Either Text ([Instruction], Int)
jumpLabelResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfJumpInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "Jump offset for label '" <> name
            <> T.pack "' " <> err)
        Right off ->
            Right (out ++ [Jump (fromIntegral off)], idx + sizeOfJumpInst)
