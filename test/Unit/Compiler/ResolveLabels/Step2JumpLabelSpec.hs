{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2JumpLabelSpec.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ResolveLabels.Step2JumpLabelSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Compiler.Instruction (Instruction(..))
import Compiler.ResolveLabels.Step2JumpLabel (step2JumpLabel)

spec :: Spec
spec = describe "Step2JumpLabel" $ do

    it "resolves a valid forward Jump label" $ do
        let labelName = "lbl_Forward"
        let targetAddr = 100
        let currentIdx = 50
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, 50, 100]
        let currentOut = []
        
        -- Offset = Target - (Current + SizeOfJump)
        -- 100 - (50 + 5) = 45
        let expectedOffset = 45
        let expectedInstr = Jump expectedOffset
        let expectedIdx = 55

        step2JumpLabel labelMap validStarts currentOut currentIdx labelName
            `shouldBe` Right ([expectedInstr], expectedIdx)

    it "resolves a valid backward Jump label" $ do
        let labelName = "lbl_Back"
        let targetAddr = 20
        let currentIdx = 40
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [20, 40]

        -- Offset = 20 - (40 + 5) = -25
        let expectedOffset = -25
        let expectedInstr = Jump expectedOffset

        step2JumpLabel labelMap validStarts [] currentIdx labelName
            `shouldBe` Right ([expectedInstr], 45)

    it "returns error when label is unknown" $ do
        let labelName = "unknown_lbl"
        let labelMap = Map.empty
        let validStarts = Set.empty

        step2JumpLabel labelMap validStarts [] 0 labelName
            `shouldBe` Left "Unknown label: unknown_lbl"

    it "returns error when target is not at instruction boundary" $ do
        let labelName = "bad_align"
        let targetAddr = 13
        let currentIdx = 0
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, 10, 20]

        step2JumpLabel labelMap validStarts [] currentIdx labelName
            `shouldBe` Left "Jump target not at instruction boundary"

    it "returns error when offset exceeds Int32 range" $ do
        let labelName = "far_label"
        let targetAddr = 3000000000
        let currentIdx = 0
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, targetAddr]

        case step2JumpLabel labelMap validStarts [] currentIdx labelName of
            Left err -> err `shouldSatisfy` (\msg ->
                "Jump offset for label 'far_label'" `T.isInfixOf` msg
                && "Offset out of range" `T.isInfixOf` msg)
            Right _ -> expectationFailure "Should have failed with range error"
