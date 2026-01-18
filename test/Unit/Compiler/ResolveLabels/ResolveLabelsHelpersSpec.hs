{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- ResolveLabelsHelpersSpec
-}

module Compiler.ResolveLabels.ResolveLabelsHelpersSpec (spec) where

import Test.Hspec
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Compiler.Instruction (Instruction(..), instructionSize)

-- On importe le module à tester
import Compiler.ResolveLabels.ResolveLabelsHelpers

spec :: Spec
spec = describe "ResolveLabelsHelpers Unit Tests" $ do

    describe "Instruction Size Constants" $ do
        
        it "evaluates sizeOfJumpInst correctly" $ do
            sizeOfJumpInst `shouldBe` instructionSize (Jump 0)
            sizeOfJumpInst `shouldSatisfy` (> 0)

        it "evaluates sizeOfJumpIfFalseInst correctly" $ do
            sizeOfJumpIfFalseInst `shouldBe` instructionSize (JumpIfFalse 0)

        it "evaluates sizeOfJumpIfTrueInst correctly" $ do
            sizeOfJumpIfTrueInst `shouldBe` instructionSize (JumpIfTrue 0)

        it "evaluates sizeOfCallInst correctly" $ do
            sizeOfCallInst `shouldBe` instructionSize (Call 0)

        it "evaluates sizeOfTailCallInst correctly" $ do
            sizeOfTailCallInst `shouldBe` instructionSize (Call 0)
    describe "computeOffset" $ do
        
        it "calculates forward jump (positive offset)" $ do
            let target = 10
            let current = 2
            computeOffset target current `shouldBe` (8 :: Int64)

        it "calculates backward jump (negative offset)" $ do
            let target = 5
            let current = 15
            computeOffset target current `shouldBe` (-10 :: Int64)

        it "calculates zero jump (target == current)" $ do
            computeOffset 42 42 `shouldBe` (0 :: Int64)

    describe "checkInt32Range" $ do
        
        it "accepts 0" $ do
            checkInt32Range 0 `shouldBe` Right (0 :: Int32)

        it "accepts Max Int32 bound" $ do
            let maxI32 = 2147483647 :: Int64
            checkInt32Range maxI32 `shouldBe` Right (2147483647 :: Int32)

        it "accepts Min Int32 bound" $ do
            let minI32 = -2147483648 :: Int64
            checkInt32Range minI32 `shouldBe` Right (-2147483648 :: Int32)

        it "rejects Value > Max Int32 (Overflow)" $ do
            let overflow = 2147483648 :: Int64 -- Max + 1
            checkInt32Range overflow `shouldBe` Left (T.pack "Offset out of range")

        it "rejects Value < Min Int32 (Underflow)" $ do
            let underflow = -2147483649 :: Int64 -- Min - 1
            checkInt32Range underflow `shouldBe` Left (T.pack "Offset out of range")
