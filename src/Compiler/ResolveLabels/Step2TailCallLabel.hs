{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- step2TailCallLabel
-}

module Compiler.ResolveLabels.Step2TailCallLabel (step2TailCallLabel, ) where

import Data.Text (Text)
import Compiler.Instruction (Instruction(..), instructionSize)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Compiler.ResolveLabels.ResolveLabelsHelpers (computeOffset, checkInt32Range, sizeOfTailCallInst)

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
        Right off -> Right (out ++ [TailCall (fromIntegral off)],
            idx + sizeOfTailCallInst)
