{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- PsInstructionSpec.hs
-}

{-# LANGUAGE LambdaCase #-}

module Compiler.PsInstructionSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Compiler.PsInstruction (PsInstruction(..))
import Compiler.Instruction (Instruction(..), Immediate(..))
import Common.Type.Integer (IntValue(..))

-- Predicats avec alternatives (case) : HPC comptera les branches.
isRealPushI8_42 :: PsInstruction -> Bool
isRealPushI8_42 = \case
  Real (Push (ImmInt (I8 42))) -> True
  _                            -> False

isLabelDefMain :: PsInstruction -> Bool
isLabelDefMain = \case
  LabelDef t | t == T.pack "main" -> True
  _                               -> False

isJumpLabelL1 :: PsInstruction -> Bool
isJumpLabelL1 = \case
  JumpLabel t | t == T.pack "L1" -> True
  _                              -> False

spec :: Spec
spec = describe "Compiler.PsInstruction (with real alternatives covered)" $ do
  describe "Real (Push I8 42)" $ do
    it "Matches when correct" $
      isRealPushI8_42 (Real (Push (ImmInt (I8 42)))) `shouldBe` True

    it "Does not match when different (covers _ branch)" $
      isRealPushI8_42 (Real Halt) `shouldBe` False

  describe "LabelDef main" $ do
    it "Matches main" $
      isLabelDefMain (LabelDef (T.pack "main")) `shouldBe` True

    it "Does not match other label" $
      isLabelDefMain (LabelDef (T.pack "notmain")) `shouldBe` False

  describe "JumpLabel L1" $ do
    it "Matches L1" $
      isJumpLabelL1 (JumpLabel (T.pack "L1")) `shouldBe` True

    it "Does not match other label" $
      isJumpLabelL1 (JumpLabel (T.pack "L2")) `shouldBe` False

  describe "Eq sanity" $ do
    it "Different constructors are not equal" $
      Real Halt `shouldNotBe` LabelDef (T.pack "Halt")
