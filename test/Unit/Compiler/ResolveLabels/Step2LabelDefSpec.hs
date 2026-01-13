{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Step2LabelDefSpec.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Compiler.ResolveLabels.Step2LabelDefSpec (spec) where

import Test.Hspec
import Compiler.Instruction (Instruction(..))
import Compiler.ResolveLabels.Step2LabelDef (step2LabelDef)

spec :: Spec
spec = describe "Step2LabelDef" $ do

    it "acts as identity: preserves instructions and index unchanged" $ do
        let currentOut = [Nop, Halt]
        let currentIdx = 10

        step2LabelDef currentOut currentIdx
            `shouldBe` Right (currentOut, currentIdx)

    it "works with initial empty state" $ do
        step2LabelDef [] 0 `shouldBe` Right ([], 0)
