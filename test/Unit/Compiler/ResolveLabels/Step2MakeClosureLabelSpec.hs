{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2MakeClosureLabelSpec.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ResolveLabels.Step2MakeClosureLabelSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Compiler.Instruction (Instruction(..), instructionSize)
import Compiler.ResolveLabels.Step2MakeClosureLabel (step2MakeClosureLabel)

spec :: Spec
spec = describe "Step2MakeClosureLabel" $ do

    let mcSize = instructionSize (MakeClosure 0 0)

    it "resolves a valid forward MakeClosure label" $ do
        let labelName = "closure_target"
        let targetAddr = 100
        let currentIdx = 50
        let captureCount = 3
        
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, 50, 100]
        let currentOut = []

        let expectedOffset = 100 - (50 + mcSize)
        let expectedInstr = MakeClosure expectedOffset captureCount
        let expectedIdx = 50 + mcSize

        step2MakeClosureLabel labelMap validStarts currentOut currentIdx labelName captureCount
            `shouldBe` Right ([expectedInstr], expectedIdx)

    it "resolves a valid backward MakeClosure label" $ do
        let labelName = "recursive_closure"
        let targetAddr = 20
        let currentIdx = 40
        let captureCount = 1

        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [20, 40]

        let expectedOffset = 20 - (40 + mcSize)
        let expectedInstr = MakeClosure expectedOffset captureCount

        step2MakeClosureLabel labelMap validStarts [] currentIdx labelName captureCount
            `shouldBe` Right ([expectedInstr], 40 + mcSize)

    it "returns error when label is unknown" $ do
        let labelName = "unknown"
        let captureCount = 0
        let labelMap = Map.empty
        let validStarts = Set.empty

        step2MakeClosureLabel labelMap validStarts [] 0 labelName captureCount
            `shouldBe` Left "Unknown label: unknown"

    it "returns error when target is not at instruction boundary" $ do
        let labelName = "bad_align"
        let targetAddr = 13
        let currentIdx = 0
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, 10, 20]

        step2MakeClosureLabel labelMap validStarts [] currentIdx labelName 0
            `shouldBe` Left "MakeClosure target not at instruction boundary"

    it "returns error when offset exceeds Int32 range" $ do
        let labelName = "far_label"
        let targetAddr = 3000000000
        let currentIdx = 0
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, targetAddr]

        case step2MakeClosureLabel labelMap validStarts [] currentIdx labelName 0 of
            Left err -> err `shouldSatisfy` (\msg ->
                "MakeClosure offset for label 'far_label'" `T.isInfixOf` msg
                && "Offset out of range" `T.isInfixOf` msg)
            Right _ -> expectationFailure "Should have failed with range error"
