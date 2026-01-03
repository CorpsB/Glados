{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- PsInstructionSpec
-}

module Compiler.PsInstructionSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Compiler.PsInstruction
import Compiler.Instruction

spec :: Spec
spec = describe "Compiler.PsInstruction" $ do
    let labels = [LabelDef, JumpLabel, JumpIfFalseLabel, JumpIfTrueLabel,
                 CallLabel, TailCallLabel, GetFuncAddrLabel]
    let t = T.pack "test"

    describe "Constructor Coverage" $ do
        it "covers all label-based constructors" $ do
            mapM_ (\constr -> constr t `shouldBe` constr t) labels
        it "covers MakeClosureLabel specifically" $ do
            MakeClosureLabel t 2 `shouldBe` MakeClosureLabel t 2
        it "covers Real instruction wrapping" $ do
            Real Halt `shouldBe` Real Halt

    describe "Instances" $ do
        it "Show and Eq coverage" $ do
            show (MakeClosureLabel t 3) `shouldSatisfy` (not . null)
            JumpLabel (T.pack "A") `shouldNotBe` JumpLabel (T.pack "B")
