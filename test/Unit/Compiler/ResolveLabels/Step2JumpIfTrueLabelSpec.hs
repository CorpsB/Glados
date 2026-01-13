{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2JumpIfTrueLabelSpec.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ResolveLabels.Step2JumpIfTrueLabelSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Compiler.Instruction (Instruction(..))
import Compiler.ResolveLabels.Step2JumpIfTrueLabel (step2JumpIfTrueLabel)

spec :: Spec
spec = describe "Step2JumpIfTrueLabel" $ do

    it "resolves a valid forward JumpIfTrue label" $ do
        let labelName = "lbl_A"
        let targetAddr = 100
        let currentIdx = 50
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, 50, 100]
        let currentOut = []
        
        let expectedOffset = 100 - (50 + 5)
        let expectedInstr = JumpIfTrue expectedOffset
        let expectedIdx = 55

        step2JumpIfTrueLabel labelMap validStarts currentOut currentIdx labelName
            `shouldBe` Right ([expectedInstr], expectedIdx)

    it "resolves a valid backward JumpIfTrue label" $ do
        let labelName = "lbl_B"
        let targetAddr = 20
        let currentIdx = 40
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [20, 40]

        let expectedOffset = 20 - (40 + 5)
        let expectedInstr = JumpIfTrue expectedOffset

        step2JumpIfTrueLabel labelMap validStarts [] currentIdx labelName
            `shouldBe` Right ([expectedInstr], 45)

    it "returns error when label is unknown" $ do
        let labelName = "unknown"
        let labelMap = Map.empty
        let validStarts = Set.empty

        step2JumpIfTrueLabel labelMap validStarts [] 0 labelName
            `shouldBe` Left "Unknown label: unknown"

    it "returns error when target is not at instruction boundary" $ do
        let labelName = "bad_align"
        let targetAddr = 13
        let currentIdx = 0
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, 10, 20]

        step2JumpIfTrueLabel labelMap validStarts [] currentIdx labelName
            `shouldBe` Left "JumpIfTrue target not at instruction boundary"

    it "returns error when offset exceeds Int32 range" $ do
        let labelName = "far_label"
        let targetAddr = 3000000000
        let currentIdx = 0
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, targetAddr]

        case step2JumpIfTrueLabel labelMap validStarts [] currentIdx labelName of
            Left err -> err `shouldSatisfy` (\msg ->
                "JumpIfTrue offset for label 'far_label'" `T.isInfixOf` msg
                && "Offset out of range" `T.isInfixOf` msg)
            Right _ -> expectationFailure "Should have failed with range error"
