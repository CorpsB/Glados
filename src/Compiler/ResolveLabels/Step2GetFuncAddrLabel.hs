{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2GetFuncAddrLabel
-}

module Compiler.ResolveLabels.Step2GetFuncAddrLabel (step2GetFuncAddrLabel, ) where

import Data.Text (Text)
import Compiler.Instruction (Instruction(..), instructionSize)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Compiler.ResolveLabels.ResolveLabelsHelpers (computeOffset, checkInt32Range)

step2GetFuncAddrLabel :: Map.Map Text Int -> Set.Set Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
step2GetFuncAddrLabel labelMap starts out idx name =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target -> if Set.member target starts
                       then getFuncAddrResult target out idx name
                       else Left (T.pack
                        "GetFuncAddr target not at instruction boundary")

getFuncAddrResult :: Int -> [Instruction] -> Int -> Text -> Either Text ([Instruction], Int)
getFuncAddrResult target out idx name =
    let off64 = computeOffset target (idx + instructionSize (GetFuncAddr 0))
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "GetFuncAddr offset for label '"
            <> name <> T.pack "' " <> err)
        Right off -> Right (out ++ [GetFuncAddr (fromIntegral off)],
            idx + instructionSize (GetFuncAddr 0))
