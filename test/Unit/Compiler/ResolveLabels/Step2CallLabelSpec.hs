{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2CallLabelSpec
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ResolveLabels.Step2CallLabelSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Int (Int32)
import Compiler.Instruction (Instruction(..))
import Compiler.ResolveLabels.Step2CallLabel (step2CallLabel)

spec :: Spec
spec = describe "Step2CallLabel Logic" $ do

    it "resolves a valid forward Call label" $ do
        let labelName = T.pack "func_A"
        let targetAddr = 100
        let currentIdx = 50

        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, 50, 100] 
        let currentOut = [Nop]

        let expectedInstr = Call 45
        let expectedIdx = 55

        step2CallLabel labelMap validStarts currentOut currentIdx labelName
            `shouldBe` Right ([Nop, expectedInstr], expectedIdx)

    it "resolves a valid backward Call label" $ do
        let labelName = T.pack "func_B"
        let targetAddr = 20
        let currentIdx = 40
        
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [20, 40]

        let expectedInstr = Call (-25)

        step2CallLabel labelMap validStarts [] currentIdx labelName
            `shouldBe` Right ([expectedInstr], 45)

    it "returns error when label is unknown" $ do
        let labelName = T.pack "unknown_func"
        let labelMap = Map.empty
        let validStarts = Set.empty

        step2CallLabel labelMap validStarts [] 0 labelName
            `shouldBe` Left (T.pack "Unknown label: unknown_func")

    it "returns error when target is not at instruction boundary" $ do
        let labelName = T.pack "bad_align"
        let targetAddr = 13
        let currentIdx = 0
        
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, 10, 20]

        step2CallLabel labelMap validStarts [] currentIdx labelName
            `shouldBe` Left (T.pack "Call target not at instruction boundary")

    it "returns error when offset exceeds Int32 range" $ do
        let labelName = T.pack "far_far_away"
        let targetAddr = 3000000000
        let currentIdx = 0
        
        let labelMap = Map.fromList [(labelName, targetAddr)]
        let validStarts = Set.fromList [0, targetAddr]

        case step2CallLabel labelMap validStarts [] currentIdx labelName of
            Left err -> err `shouldSatisfy` (\msg -> 
                T.pack "Call offset for label 'far_far_away'" `T.isInfixOf` msg 
                && T.pack "Offset out of range" `T.isInfixOf` msg)
            Right _ -> expectationFailure "Should have failed with range error"
