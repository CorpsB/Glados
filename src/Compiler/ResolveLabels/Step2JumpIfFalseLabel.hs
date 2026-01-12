{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2JumpLabel
-}

module Compiler.ResolveLabels.Step2JumpIfFalseLabel (step2JumpIfFalseLabel) where

import Compiler.Instruction (Instruction(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Compiler.ResolveLabels.ResolveLabelsHelpers
    ( computeOffset
    , checkInt32Range
    , sizeOfJumpIfFalseInst
    )

step2JumpIfFalseLabel :: Map.Map Text Int -> Set.Set Int
    -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2JumpIfFalseLabel labelMap starts out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> 
            case Set.member target starts of
                True  -> jumpIfFalseResult target out idx name
                False -> Left (T.pack
                    "JumpIfFalse target not at instruction boundary")

jumpIfFalseResult :: Int -> [Instruction] -> Int ->
    Text -> Either Text ([Instruction], Int)
jumpIfFalseResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfJumpIfFalseInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "JumpIfFalse offset for label '"
            <> name <> T.pack "' " <> err)
        Right off -> Right (out ++ [JumpIfFalse (fromIntegral off)],
            idx + sizeOfJumpIfFalseInst)
