{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2CallLabel
-}

module Compiler.ResolveLabels.Step2CallLabel (step2CallLabel) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Compiler.Instruction (Instruction(..))
import Compiler.ResolveLabels.ResolveLabelsHelpers (computeOffset, checkInt32Range, sizeOfCallInst)

step2CallLabel :: Map.Map Text Int -> Set.Set Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2CallLabel labelMap starts out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> if Set.member target starts
                       then callLabelResult target out idx name
                       else Left (T.pack
                        "Call target not at instruction boundary")

callLabelResult :: Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
callLabelResult target out idx name =
    let off64 = computeOffset target (idx + sizeOfCallInst)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "Call offset for label '" <> name
            <> T.pack "' " <> err)
        Right off ->
            Right (out ++ [Call (fromIntegral off)], idx + sizeOfCallInst)
