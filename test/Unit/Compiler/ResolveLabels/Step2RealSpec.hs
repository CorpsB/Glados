{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2RealSpec.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ResolveLabels.Step2RealSpec (spec) where

import Test.Hspec
import Compiler.Instruction (Instruction(..), instructionSize, Immediate(..))
import Common.Type.Integer (IntValue(..))
import Compiler.ResolveLabels.Step2Real (step2Real)

spec :: Spec
spec = describe "Step2Real" $ do

    it "handles simple 1-byte instruction (Nop)" $ do
        let currentOut = []
        let currentIdx = 0
        let instr = Nop

        step2Real currentOut currentIdx instr 
            `shouldBe` Right ([Nop], 1)

    it "handles multi-byte instruction (Jump) appending to existing list" $ do
        let currentOut = [Nop]
        let currentIdx = 1
        let instr = Jump 42

        let expectedSize = instructionSize instr -- 5
        let expectedIdx = currentIdx + expectedSize -- 6

        step2Real currentOut currentIdx instr 
            `shouldBe` Right ([Nop, Jump 42], expectedIdx)

    it "handles complex variable size instruction (Push Int32)" $ do
        let currentOut = []
        let currentIdx = 100
        let instr = Push (ImmInt (I32 12345))

        let expectedSize = instructionSize instr
        let expectedIdx = 100 + expectedSize

        step2Real currentOut currentIdx instr 
            `shouldBe` Right ([instr], expectedIdx)

    it "uses instructionSize dynamically" $ do
        let instr = Call 0
        let startIdx = 10

        case step2Real [] startIdx instr of
            Right (_, newIdx) -> 
                (newIdx - startIdx) `shouldBe` instructionSize instr
            Left _ -> expectationFailure "Should always succeed"
