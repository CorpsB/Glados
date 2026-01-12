{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- step2MakeClosureLabel
-}

module Compiler.ResolveLabels.Step2MakeClosureLabel (step2MakeClosureLabel, ) where


import Compiler.Instruction (Instruction(..), instructionSize)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Compiler.ResolveLabels.ResolveLabelsHelpers (computeOffset, checkInt32Range)

step2MakeClosureLabel :: Map.Map Text Int -> Set.Set Int ->
    [Instruction] -> Int -> Text -> Int -> Either Text ([Instruction], Int)
step2MakeClosureLabel labelMap starts out idx name n =
    case Map.lookup name labelMap of
        Nothing -> Left (T.pack "Unknown label: " <> name)
        Just target ->
            case Set.member target starts of
                True -> makeClosureResult target out idx name n
                False -> Left (T.pack
                    "MakeClosure target not at instruction boundary")

makeClosureResult :: Int -> [Instruction] -> Int ->
    Text -> Int -> Either Text ([Instruction], Int)
makeClosureResult target out idx name n =
    let mcSize = instructionSize (MakeClosure 0 0)
        off64 = computeOffset target (idx + mcSize)
    in case checkInt32Range off64 of
        Left err -> Left (T.pack "MakeClosure offset for label '"
            <> name <> T.pack "' " <> err)
        Right off ->
            Right (out ++ [MakeClosure (fromIntegral off) n], idx + mcSize)
